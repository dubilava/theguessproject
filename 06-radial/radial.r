# load packages
library(data.table)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(magick)
library(Cairo)

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

# load the data
load("coeftab.RData")

# generate the radial plot
gg_coef <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="coral",alpha=.25)+
  geom_line(color="coral",linewidth=.6)+
  geom_hline(yintercept = seq(-12,10,4),color="gray60",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray40",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,16,4))+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence",caption="Created by @DavidUbilava | Data: ACLED (https://acleddata.com/); adopted from Ubilava et al. (2023)")+
  theme_guess()+
  theme(axis.line=element_blank(),panel.grid.major.y = element_blank())

# add logo
gg_coef <- ggdraw(gg_coef) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("radial_violence.png",gg_coef,width=6.5,height=6.25,dpi="retina",device="png")
ggsave("radial_violence.eps",gg_coef,width=6.5,height=6.25,dpi="retina",device=cairo_ps)
