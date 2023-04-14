# slack




for(m in 1:M){
  set.seed(m)
  y <- rnorm(n)
  for(i in 2:n){
    y[i] <- .5*y[i-1]+y[i]
  }
  
  # store in a data.table
  dt <- data.table(date=1:n,y=y)
  
  # lagged dependent variable and two forecast windows
  dt[,`:=`(y1=shift(y,1),exp=as.numeric(NA),rol=as.numeric(NA))]
  
  
  
  for(i in 1:P){
    # regressions
    reg_exp <- lm(y~y1,data=dt[1:(R-1+i)])
    reg_rol <- lm(y~y1,data=dt[i:(R-1+i)])
    
    # forecasts
    dt$exp[R+i] <- reg_exp$coefficients["(Intercept)"]+reg_exp$coefficients["y1"]*dt$y1[R+i]
    dt$rol[R+i] <- reg_rol$coefficients["(Intercept)"]+reg_rol$coefficients["y1"]*dt$y1[R+i]
  }
  
  # forecast errors
  dt[,`:=`(exp_e=y-exp,rol_e=y-rol)]
  
  rmsfe_dt$exp[m] <- sqrt(mean(dt$exp_e^2,na.rm=T))
  rmsfe_dt$rol[m] <- sqrt(mean(dt$rol_e^2,na.rm=T))
}






reg_less <- lm(less~y1,data=dt)
reg_more <- lm(more~y1,data=dt)


sig_less <- summary(reg_less)$sigma
sig_more <- summary(reg_more)$sigma


pow <- function(x,i){
  x <- (x^2)^i
  return(x)
}

multi_less <- pow(reg_less$coefficients["y1"],0:49)
var_less <- sig_less^2*cumsum(multi_less)

multi_more <- pow(reg_more$coefficients["y1"],0:49)
var_more <- sig_more^2*cumsum(multi_more)


dt[,`:=`(less_f=NA,more_f=NA)]
dt$less_f[150] <- dt$less[150]
dt$more_f[150] <- dt$more[150]

for(i in 151:n){
  dt$less_f[i] <- reg_less$coefficients["(Intercept)"]+reg_less$coefficients["y1"]*dt$less_f[i-1]
  dt$more_f[i] <- reg_more$coefficients["(Intercept)"]+reg_more$coefficients["y1"]*dt$more_f[i-1]
}

dt$less_f[150] <- NA
dt$more_f[150] <- NA

dt[,`:=`(less_lo=less_f-1.96*sqrt(var_less),less_up=less_f+1.96*sqrt(var_less),more_lo=more_f-1.96*sqrt(var_more),more_up=more_f+1.96*sqrt(var_more))]


# plot the time series gg_ts <- 
ggplot(dt,aes(x=date,y=y))+
  geom_line(linewidth=0.8,color=ifelse(dt$date<=100,"darkgray","coral"))+
  geom_ribbon(aes(ymin=less_lo,ymax=less_up),fill="darkgray",alpha=.2)+
  geom_line(aes(y=less_f),linewidth=0.8,color="darkgray",na.rm=T)+
  labs(x="t",y=expression(y[t]))+
  theme_classic()+
  theme(axis.title = element_text(size=12),axis.text = element_text(size=10))


# plot the time series gg_ts <- 
ggplot(dt,aes(x=date,y=y))+
  geom_line(linewidth=0.8,color="coral")+
  geom_ribbon(aes(ymin=more_lo,ymax=more_up),fill="darkgray",alpha=.2)+
  geom_line(aes(y=more_f),linewidth=0.8,color="darkgray",na.rm=T)+
  labs(x="t",y=expression(y[t]))+
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
