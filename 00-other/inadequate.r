# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(Cairo)
library(crypto2)
library(fredr)

# plot aesthetics
theme_guess <- function(){
  theme(
    panel.background=element_rect(fill="transparent",color=NA),
    plot.background=element_rect(fill="transparent",color=NA),
    legend.background=element_rect(fill="transparent",color=NA),
    plot.title=element_text(size=12,colour="dimgray"),
    axis.title=element_text(size=12,colour="dimgray"),
    axis.text=element_text(size=10,colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line=element_line(colour="darkgray"),
    axis.ticks=element_line(colour="darkgray"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="dimgray"),
    legend.key.size=unit(.75,'lines'),
    plot.caption = element_text(colour="slategray"),
    strip.background=element_blank(),
    strip.text=element_text(size=10,colour="dimgray",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}

# load 10-year real interest rate
dt <- data.table(fredr(series_id = "REAINTRATREARAT10Y",observation_start = as.Date("1982-01-01"),observation_end = as.Date("2022-12-31")))

# dt <- data.table(fredr(series_id = "UNRATE",observation_start = as.Date("1948-01-01"),observation_end = as.Date("2022-12-31")))

dt <- dt[,.(date,rate=value)]

# plot the time series
gg_ts <- ggplot(dt,aes(x=date,y=rate))+
  geom_line(linewidth=0.8,color="coral")+
  labs(x="Year",y="10-year real interest rate")+
  labs(caption="Created by @DavidUbilava using Federal Reserve Bank of Cleveland\ndata retrieved from FRED [REAINTRATREARAT10Y] via fredr package")+
  theme_classic()+
  theme_guess()

ggsave("figures/fred10r.png",gg_ts,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/fred10r.eps",gg_ts,width=6.5,height=4.5,dpi="retina",device="eps")


# perform a simple forecasting exercise
dt[,`:=`(rate_l=shift(rate,1),tr=1:nrow(dt))]
dt[,`:=`(rate_d=rate-rate_l)]

# plot the densities of absolute forecast errors
gg_mz1 <- ggplot(dt[date<=as.Date("2019-12-31")],aes(x=rate_l,y=rate))+
  geom_vline(xintercept = c(mean(dt$rate_l,na.rm=T)),color="dimgray",size=.6,linetype=5)+
  geom_hline(yintercept = c(mean(dt$rate,na.rm=T)),color="dimgray",size=.6,linetype=5)+
  geom_abline(slope=1,intercept=0,color="dimgray",size=.6,linetype=5)+
  geom_point(size=1.6,stroke=1.2,color="coral",fill="coral",alpha=.5,na.rm=T)+
  stat_smooth(method="lm",formula=y~x,se=F,color="coral",size=.8,linetype=5,na.rm=T)+
  labs(x=expression(y['t']),y=expression(y['t+1']),caption="Created by @DavidUbilava using Federal Reserve Bank of Cleveland\ndata retrieved from FRED [REAINTRATREARAT10Y] via fredr package")+
  coord_cartesian(xlim=c(-.5,8),ylim=c(-.5,8))+
  theme_classic()+
  theme_guess()

ggsave("figures/fred10efficient.png",gg_mz1,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/fred10efficient.eps",gg_mz1,width=6.5,height=6.5,dpi="retina",device=cairo_ps)

# perform a simple forecasting exercise
dt[,`:=`(rate_l12=shift(rate,12))]
dt[,`:=`(rate_d12=rate-rate_l12)]

# plot the densities of absolute forecast errors
gg_mz2 <- ggplot(dt[date<=as.Date("2019-12-31")],aes(x=rate_l12,y=rate))+
  geom_vline(xintercept = c(mean(dt$rate_l,na.rm=T)),color="dimgray",size=.6,linetype=5)+
  geom_hline(yintercept = c(mean(dt$rate,na.rm=T)),color="dimgray",size=.6,linetype=5)+
  geom_abline(slope=1,intercept=0,color="dimgray",size=.6,linetype=5)+
  geom_point(size=1.6,stroke=1.2,color="coral",fill="coral",alpha=.5,na.rm=T)+
  stat_smooth(method="lm",formula=y~x,se=F,color="coral",size=.8,linetype=5,na.rm=T)+
  labs(x=expression(y['t-11']),y=expression(y['t+1']),caption="Created by @DavidUbilava using Federal Reserve Bank of Cleveland\ndata retrieved from FRED [REAINTRATREARAT10Y] via fredr package")+
  coord_cartesian(xlim=c(-.5,8),ylim=c(-.5,8))+
  theme_classic()+
  theme_guess()

ggsave("figures/fred10inefficient.png",gg_mz2,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/fred10inefficient.eps",gg_mz2,width=6.5,height=6.5,dpi="retina",device=cairo_ps)
