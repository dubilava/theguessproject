# load the libraries (install them if needed)
library(data.table)
library(ggplot2)

# generate sample time series
n <- 200
set.seed(1)
y <- rnorm(n)
for(i in 2:n){
  y[i] <- .8*y[i-1]+y[i]
}

# store in a data.table
dt <- data.table(date=1:n,y=y)

# plot the time series
gg_ts <- ggplot(dt,aes(x=date,y=y))+
  geom_line(linewidth=0.8,color="coral")+
  labs(x="t",y=expression(y[t]))+
  theme_classic()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))

# less information
dt[,`:=`(y1=shift(y,1),less=ifelse(date>100 & date<=150,y,NA),more=ifelse(date<=150,y,NA))]

reg_less <- lm(less~y1,data=dt)
reg_more <- lm(more~y1,data=dt)

sig_less <- summary(reg_less)$sigma


pow <- function(x,i){
  x <- x^i
  return(x)
}

multi_less <- pow(reg_less$coefficients["y1"],0:49)

var_less <- sig_less*cumsum(multiplier)



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
