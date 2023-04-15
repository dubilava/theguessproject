# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(magick)
library(Cairo)
library(crypto2)

# plot aesthetics
theme_guess <- function(base_size=10,base_family="sans",title_family="sans",border=F){
  theme_foundation(base_size=base_size,base_family=base_family) +
    theme(
      line = element_line(linetype=1,colour="black"),
      rect = element_rect(linetype=0,colour=NA),
      text = element_text(colour="black"),
      # title = element_text(family=title_family,size=rel(1.1)),
      # panel.background=element_rect(fill="transparent",color=NA),
      panel.grid = element_line(colour=NULL,linetype=3),
      panel.grid.major = element_line(colour="darkgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      # plot.background=element_rect(fill="transparent",color=NA),
      plot.title=element_text(colour="black",hjust=0,size=rel(1.1)),
      plot.caption = element_text(family=base_family,size=rel(0.7),colour="slategray",hjust=0,margin=margin(t=5,r=1,b=1,l=1)),
      plot.margin=unit(c(0.25,0.25,0.25,1.25),"lines"),
      # axis.title = element_blank(),
      axis.text = element_text(family=base_family,size=rel(0.9),margin=margin(t=1,r=1,b=1,l=1)),
      axis.text.x = element_text(colour = NULL),
      axis.text.y = element_text(colour = NULL),
      axis.ticks = element_blank(),
      axis.line = element_line(),
      axis.line.y = element_blank(),
      legend.background=element_rect(fill="transparent",color=NA),
      legend.position="none",
      legend.title=element_blank(),
      legend.text=element_text(family=base_family,size=rel(0.9),colour="slategray"),
      legend.key = element_rect(fill="transparent"),
      legend.key.size=unit(.75,'lines'),
      strip.background=element_blank(),
      strip.text=element_text(size=rel(.8),colour="slategray",margin=margin(.1,0,.1,0,"cm"))
    )
}

# load the logo (for branding)
logo <- image_read("../logo.png")

# load all active coins
coins <- crypto_list()

# store as the data.table object
coins_dt <- data.table(coins)

# select the coin of interest -- Bitcoin
coins_sub_dt <- coins_dt[symbol=="BTC"]

# fetch the historical data beginning from 1 Jan 2020
btc_tb <- crypto_history(coin_list = coins_sub_dt,start_date="20200101",end_date="20221231")

# reformat the dates
btc_tb$timestamp <- as.POSIXct(btc_tb$timestamp,format="%Y-%m-%d")
btc_tb$time_open <- as.POSIXct(btc_tb$time_open,format="%Y-%m-%d")
btc_tb$time_close <- as.POSIXct(btc_tb$time_close,format="%Y-%m-%d")
btc_tb$time_high <- as.POSIXct(btc_tb$time_high,format="%Y-%m-%d")
btc_tb$time_low <- as.POSIXct(btc_tb$time_low,format="%Y-%m-%d")

# store the dataset as the data.table object
btc_dt <- data.table(btc_tb)

# keep only date and closing price (expressed in thousand dollars)
btc_dt <- btc_dt[,.(date=as.Date(substr(timestamp,1,10)),BTC=close/1000)]

# plot the time series
gg_ts <- ggplot(btc_dt,aes(x=date,y=BTC))+
  geom_line(linewidth=0.8,color="coral")+
  labs(x="Year",y="BTC ('000 US Dollars)")+
  labs(caption="Created by @DavidUbilava | Data: CoinMarketCap via crypto2 package")+
  theme_guess()

# add logo
gg_ts <- ggdraw(gg_ts) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("bitcoin.png",gg_ts,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("bitcoin.eps",gg_ts,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


# obtain autocorrelations
maxlag <- 90
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(btc_dt$BTC,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="coral")+
  geom_hline(yintercept=0,linewidth=.6,linetype=3)+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(btc_dt)),1.96/sqrt(nrow(btc_dt))),linewidth=.8,linetype=5,col="darkgray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  labs(x="k",y=expression(hat(rho)))+
  coord_cartesian(ylim=c(0,1),xlim=c(4,maxlag-3))+
  labs(caption="Created by @DavidUbilava | Data: CoinMarketCap via crypto2 package")+
  theme_guess()

# add logo
gg_acf <- ggdraw(gg_acf) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("autocorrelogram.png",gg_acf,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("autocorrelogram.eps",gg_acf,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


# perform a simple forecasting exercise
btc_dt[,`:=`(BTC_lag=shift(BTC,1),BTC_lag2=shift(BTC,2))]
btc_dt[,`:=`(RW=BTC_lag,MO=BTC_lag+(BTC_lag-BTC_lag2))]

btc_dt[,`:=`(e_RW=RW-BTC,e_MO=MO-BTC)]

btc_dt[,`:=`(a_RW=abs(e_RW),a_MO=abs(e_MO))]

btc_dt <- btc_dt[complete.cases(btc_dt)]

# forecast errors
e_dt <- btc_dt[,.(RW=a_RW,MO=a_MO)]

# long table (for plotting convenience)
e_lg <- melt(e_dt,measure.vars=c("RW","MO"))

e_lg$variable <- factor(e_lg$variable,levels=c("RW","MO"),labels = c("Random Walk","Momentum"))

# plot the densities of absolute forecast errors
gg_den <- ggplot(e_lg,aes(x=value,color=variable,fill=variable))+
  geom_density(alpha=.25,na.rm=T)+
  scale_color_manual(values=c("coral","darkgray"))+
  scale_fill_manual(values=c("coral","darkgray"))+
  geom_vline(xintercept = c(mean(e_dt$RW),mean(e_dt$MO)),color=c("coral","darkgray"),linewidth=1,linetype=5)+
  labs(x="Absolute forecast error ('000 US Dollars)",y="Density",caption="Created by @DavidUbilava using CoinMarketCap data via crypto2 package")+
  xlim(c(0,5))+
  theme_guess()+
  theme(legend.position="top")

# add logo
gg_den <- ggdraw(gg_den) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("absolute_densities.png",gg_den,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("absolute_densities.eps",gg_den,width=6.5,height=4.5,dpi="retina",device=cairo_ps)
