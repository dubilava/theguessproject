# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(magick)

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
      plot.caption = element_text(family=base_family,size=rel(0.7),colour="slategray",hjust=0,margin=margin(t=1,r=1,b=1,l=1)),
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

# set up the coordinates for nodes and edges
nudge <- .05
dt <- data.table(x=c(-.5,0,.5),y=c(0,1,0),xbeg=c(0-nudge,0+nudge,-.5+1.5*nudge),xend=c(-.5+nudge,.5-nudge,.5-nudge),ybeg=c(1-2*nudge,1-2*nudge,0),yend=c(0+2*nudge,0+2*nudge,0),labels=c("B[t-1]","P[t-1]","S[t]"))

# plot the graph
gg_dag <- ggplot(dt)+
  geom_text(aes(x=x,y=y,label=labels),size=8,parse=T,color=c("coral","darkgray","coral"))+
  geom_segment(aes(x=xbeg,y=ybeg,xend=xend,yend=yend),lineend = c("round"),linejoin = c("round"),arrow=arrow(type="closed",angle=20,length=unit(0.2,"inches")),linewidth=1,linetype=c(1,1,5),color=c("darkgray","darkgray","coral"))+
  geom_segment(data=dt[3,],aes(x=xend-.5*nudge,y=yend,xend=xend,yend=yend),lineend = c("round"),linejoin = c("round"),arrow=arrow(type="closed",angle=20,length=unit(0.2,"inches")),linewidth=1,linetype=1,color="coral")+
  labs(x="",y="",caption="Created by @DavidUbilava")+
  coord_cartesian(clip="off")+
  theme_guess()+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.line=element_blank(),panel.grid.major = element_blank())

# add logo
gg_dag <- ggdraw(gg_dag) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("dag_birds.png",gg_dag,width=6.5,height=3.5,dpi="retina",device="png")
ggsave("dag_birds.eps",gg_dag,width=6.5,height=3.5,dpi="retina",device=cairo_ps)

