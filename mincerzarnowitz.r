# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(Cairo)

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

# generate simulated data: AR(1) with b=0.5
n <- 240
set.seed(3)
y <- rnorm(n)
for(i in 2:n){
  y[i]=.5*y[i-1]+y[i]
}

R <- round(n/2)
P <- n-R

dt <- data.table(t=1:n,y)

dt[,`:=`(y1=shift(y,1))]

# plot realizations against forecasts with 'mincer-zarnowitz lines'
gg_mz <- ggplot(dt,aes(x=y1,y=y))+
  geom_hline(yintercept = mean(dt$y,na.rm=T),linewidth=.5,linetype=5,color="dimgray")+
  geom_vline(xintercept = mean(dt$y1,na.rm=T),linewidth=.5,linetype=5,color="dimgray")+
  geom_abline(intercept=0,slope=1,linewidth=.5,linetype=5,color="dimgray")+
  geom_point(size=2,stroke=.8,color="coral",fill="coral",alpha=.5,na.rm=T)+
  stat_smooth(method="lm",formula=y~x,se=F,color="coral",linewidth=1,linetype=5,na.rm=T)+
  labs(x=expression(hat(y)["t+1|t"]),y=expression(y["t+1"]),caption="Created by @DavidUbilava using simulated data")+
  coord_cartesian(xlim=c(-3,3),ylim=c(-3,3))+
  theme_classic()+
  theme_guess()

ggsave("figures/mz.png",gg_mz,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/mz.eps",gg_mz,width=6.5,height=6.5,dpi="retina",device=cairo_ps)

