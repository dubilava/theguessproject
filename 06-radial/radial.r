# load packages
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

# load the data
load("data/coeftab.RData")

# generate the radial plot
gg_coef <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="coral",alpha=.25)+
  geom_line(color="coral",linewidth=.6)+
  geom_hline(yintercept = seq(-12,10,4),color="gray60",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray40",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,10,4))+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence",caption="Created by @DavidUbilava using ACLED data and based on Ubilava et al. (2023)")+
  theme_classic()+
  theme_guess()+
  theme(axis.line=element_blank())

ggsave("figures/radial_violence.png",gg_coef,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/radial_violence.eps",gg_coef,width=6.5,height=6.5,dpi="retina",device=cairo_ps)
