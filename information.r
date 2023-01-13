# load the libraries (install them if needed)
library(data.table)
library(ggplot2)

# a function that simulates a time series with no structural change
sim <- function(m,n,sc=FALSE){
  
  # the size of the first window and of the out-of-sample segment
  R <- round(.5*n)
  P <- n-R
  
  set.seed(m)
  y <- rnorm(n)
  for(i in 2:n){
    if(sc==FALSE){
      y[i] <- .7*y[i-1]+y[i]
    }else{
      y[i] <- .5*(1/n)*i+(.9-.4*(1/n)*i)*y[i-1]+y[i]
    }
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
  
  rmsfe_exp <- sqrt(mean(dt$exp_e^2,na.rm=T))
  rmsfe_rol <- sqrt(mean(dt$rol_e^2,na.rm=T))
  
  return(c(rmsfe_exp,rmsfe_rol))
  
}

# the length of the time series
n <- 480

# the number of iterations
M <- 500

# no structural change (will take a few minutes)
rmsfe1_dt <- data.table(t(sapply(1:M,sim,n=n,sc=F)))
colnames(rmsfe1_dt) <- c("exp","rol")

rmsfe1_dt[,`:=`(dif=log(exp/rol))]

# structural change (will take a few minutes)
rmsfe2_dt <- data.table(t(sapply(1:M,sim,n=n,sc=T)))
colnames(rmsfe2_dt) <- c("exp","rol")

rmsfe2_dt[,`:=`(dif=log(exp/rol))]

# a 'density' plot of relative RMSFE (no structural change)
gg_nc <- ggplot(rmsfe1_dt,aes(x=dif))+
  geom_dotplot(binwidth=.00055,dotsize=.6,color="coral",fill="coral",method="histodot")+
  labs(x="Relative Difference in RMSFE of Expanding vs Rolling Windows",y="",title="Constant Data-Generating Process",caption="Created by @DavidUbilava using simulated data")+
  theme_classic()+
  theme(axis.title = element_text(size=12,colour="dimgray"),axis.text = element_text(size=10,colour="dimgray"),panel.background=element_rect(fill=NA,color=NA),plot.background=element_rect(fill=NA,color=NA),legend.background=element_rect(fill="transparent",color=NA),axis.line=element_line(colour="darkgray"),axis.ticks=element_line(colour="darkgray"),axis.line.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank(),plot.caption = element_text(colour="slategray"))

ggsave("figures/nochange.png",gg_nc,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/nochange.eps",gg_nc,width=6.5,height=4.5,dpi="retina",device="eps")

# a 'density' plot of relative RMSFE (structural change)
gg_sc <- ggplot(rmsfe2_dt,aes(x=dif))+
  geom_dotplot(binwidth=.00145,dotsize=.55,color="coral",fill="coral",method="histodot")+
  labs(x="Relative Difference in RMSFE of Expanding vs Rolling Windows",y="",title="Changing Data-Generating Process",caption="Created by @DavidUbilava using simulated data")+
  theme_classic()+
  theme(axis.title = element_text(size=12,colour="dimgray"),axis.text = element_text(size=10,colour="dimgray"),panel.background=element_rect(fill=NA,color=NA),plot.background=element_rect(fill=NA,color=NA),legend.background=element_rect(fill="transparent",color=NA),axis.line=element_line(colour="darkgray"),axis.ticks=element_line(colour="darkgray"),axis.line.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_blank(),plot.caption = element_text(colour="slategray"))

ggsave("figures/change.png",gg_sc,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/change.eps",gg_sc,width=6.5,height=4.5,dpi="retina",device="eps")
