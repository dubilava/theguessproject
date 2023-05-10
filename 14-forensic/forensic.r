# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(magick)
library(Cairo)
library(crypto2)
library(fredr)

"%!in%" <- Negate("%in%")
fredr_set_key("7a1db535f59c2ac4382b9c22a15b5f06")

# check

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
      plot.margin=unit(c(0.25,0.25,0.25,1.75),"lines"),
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

# interest rate spread
spread_dt <- as.data.table(fredr(
  series_id = "T10Y3M",
  observation_start = as.Date("1982-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = "m",
  aggregation_method = "avg"
))


# plot the time series
gg_ts <- ggplot(spread_dt,aes(x=date,y=value))+
  geom_line(linewidth=0.8,color="coral")+
  labs(title="10-year maturity minus 3-months maturity",x="Year",y="Interest Rate Spread (%)",caption="Created by @DavidUbilava | Data: Federal Reserve Bank of St. Louis via fredr package")+
  theme_guess()

# add logo
gg_ts <- ggdraw(gg_ts) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("spread.png",gg_ts,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("spread.eps",gg_ts,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


# obtain autocorrelations
maxlag <- 96
acf_dt <- data.table(k=c(1:maxlag),rho=c(acf(spread_dt$value,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_acf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="coral")+
  geom_hline(yintercept=0,linewidth=.6,linetype=3)+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(spread_dt)),1.96/sqrt(nrow(spread_dt))),linewidth=.8,linetype=5,col="darkgray")+
  labs(title="Autocorrelations",x="k",y=expression(hat(rho)),caption="Created by @DavidUbilava | Data: Federal Reserve Bank of St. Louis via fredr package")+
  theme_guess()

# add logo
gg_acf <- ggdraw(gg_acf) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("acf.png",gg_acf,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("acf.eps",gg_acf,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


maxlag <- 96
acf_dt <- data.table(k=c(1:maxlag),rho=c(pacf(spread_dt$value,lag.max=maxlag,plot=F)[1:maxlag]$acf))

# plot the autocorrelogram
gg_pacf <- ggplot(acf_dt,aes(x=k,y=rho))+
  geom_segment(aes(xend=k,yend=0),linewidth=0.8,col="coral")+
  geom_hline(yintercept=0,linewidth=.6,linetype=3)+
  geom_hline(yintercept=c(-1.96/sqrt(nrow(spread_dt)),1.96/sqrt(nrow(spread_dt))),linewidth=.8,linetype=5,col="darkgray")+
  labs(title="Partial Autocorrelations",x="k",y=expression(hat(rho)),caption="Created by @DavidUbilava | Data: Federal Reserve Bank of St. Louis via fredr package")+
  theme_guess()

# add logo
gg_pacf <- ggdraw(gg_pacf) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("pacf.png",gg_pacf,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("pacf.eps",gg_pacf,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


dt <- spread_dt[,.(date,y=value)]
dt[,`:=`(y1=shift(y,1),y2=shift(y,2),y3=shift(y,3),y4=shift(y,4))]

dt <- dt[complete.cases(dt)]

r1 <- lm(y~y1,data=dt)
r2 <- lm(y~y1+y2,data=dt)
r3 <- lm(y~y1+y2+y3,data=dt)
r4 <- lm(y~y1+y2+y3+y4,data=dt)

sic1 <- log(sum(r1$residuals^2))+log(length(r1$residuals))*length(r1$coefficients)/length(r1$residuals)
sic2 <- log(sum(r2$residuals^2))+log(length(r1$residuals))*length(r2$coefficients)/length(r2$residuals)
sic3 <- log(sum(r3$residuals^2))+log(length(r1$residuals))*length(r3$coefficients)/length(r3$residuals)
sic4 <- log(sum(r4$residuals^2))+log(length(r1$residuals))*length(r4$coefficients)/length(r4$residuals)

# perform a simple forecasting exercise
dt[,`:=`(rw=y1,ar3=as.numeric(NA))]

R <- round(nrow(dt)*.75)
P <- nrow(dt)-R

for(i in 1:P){
  r3 <- lm(y~y1+y2+y3,data=dt[i:(R-1+i)])
  dt$ar3[R+i] <- r3$coefficients["(Intercept)"]+r3$coefficients["y1"]*dt$y1[R+i]+r3$coefficients["y2"]*dt$y2[R+i]+r3$coefficients["y3"]*dt$y3[R+i]
}

dt[,`:=`(e_rw=y-rw,e_ar3=y-ar3)]

dt[,`:=`(a_rw=abs(e_rw),a_ar3=abs(e_ar3))]

dt <- dt[complete.cases(dt)]

# forecast errors
e_dt <- dt[,.(rw=a_rw,ar3=a_ar3)]

# long table (for plotting convenience)
e_lg <- melt(e_dt,measure.vars=c("rw","ar3"))

e_lg$variable <- factor(e_lg$variable,levels=c("rw","ar3"),labels = c("RW","AR(3)"))

# plot the densities of absolute forecast errors
gg_den <- ggplot(e_lg,aes(x=value,color=variable,fill=variable))+
  geom_density(alpha=.25,na.rm=T)+
  scale_color_manual(values=c("darkgray","coral"))+
  scale_fill_manual(values=c("darkgray","coral"))+
  geom_vline(xintercept = c(mean(e_dt$rw),mean(e_dt$ar3)),color=c("coral","darkgray"),linewidth=1,linetype=5)+
  labs(title="Forecast accuracy of the interest rate spread using RW and AR(3) models",x="Absolute forecast error (%)",y="Density",caption="Created by @DavidUbilava | Data: Federal Reserve Bank of St. Louis via fredr package")+
  theme_guess()+
  theme(legend.position="top")

# add logo
gg_den <- ggdraw(gg_den) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("error_densities.png",gg_den,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("error_densities.eps",gg_den,width=6.5,height=4.5,dpi="retina",device=cairo_ps)
