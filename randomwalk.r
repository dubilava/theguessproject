# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(crypto2)

# load all active coins
coins <- crypto_list()

# store as the data.table object
coins_dt <- data.table(coins)

# select the coin of interest -- Bitcoin
coins_sub_dt <- coins_dt[symbol=="BTC"]

# fetch the historical data beginning from 1 Jan 2020
btc_tb <- crypto_history(coin_list = coins_sub_dt,start_date="20200101")

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
  geom_line(size=0.8,color="coral")+
  labs(x="Year",y="BTC ('000 US Dollars)")+
  theme_classic()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))

ggsave("figures/bitcoin.png",gg_ts,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/bitcoin.eps",gg_ts,width=6.5,height=4.5,dpi="retina",device="eps")


# obtain autocorrelations
maxlag <- 90
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(btc_dt$BTC,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_segment(aes(xend=k,yend=0),size=0.8,col="coral")+
  geom_hline(yintercept=0,size=.6,linetype=3)+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(btc_dt)),1.96/sqrt(nrow(btc_dt))),size=.8,linetype=5,col="darkgray")+
  scale_x_continuous(breaks=seq(5,maxlag,5),labels=seq(5,maxlag,5))+
  labs(x="k",y=expression(hat(rho)))+
  coord_cartesian(ylim=c(0,1),xlim=c(4,maxlag-3))+
  theme_classic()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))

ggsave("figures/autocorrelogram.png",gg_acf,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/autocorrelogram.eps",gg_acf,width=6.5,height=4.5,dpi="retina",device="eps")


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
  geom_density(alpha=.25)+
  scale_color_manual(values=c("coral","darkgray"))+
  scale_fill_manual(values=c("coral","darkgray"))+
  geom_vline(xintercept = c(mean(e_dt$RW),mean(e_dt$MO)),color=c("coral","darkgray"),size=1,linetype=5)+
  labs(x="Absolute forecast error ('000 US Dollars)",y="Density")+
  xlim(c(0,5))+
  theme_classic()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10),legend.position = "top",legend.title = element_blank())

ggsave("figures/absolute_densities.png",gg_den,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/absolute_densities.eps",gg_den,width=6.5,height=4.5,dpi="retina",device="eps")
