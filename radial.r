# load packages
library(data.table)
library(ggplot2)
library(Cairo)

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
  labs(x="months after harvest (H)",y="% change in violence")+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_text(size=14),axis.text = element_text(size=12),plot.background = element_rect(fill="white",color=NA),panel.background = element_rect(fill="white",color=NA))

ggsave("figures/radial_violence.png",gg_coef,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/radial_violence.eps",gg_coef,width=6.5,height=6.5,dpi="retina",device=cairo_ps)
