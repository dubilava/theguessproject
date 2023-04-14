# load the libraries (install them if needed)
library(data.table)
library(ggplot2)

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

# set up the coordinates for nodes and edges
nudge <- .05
dt <- data.table(x=c(-.5,0,.5),y=c(0,1,0),xbeg=c(0-nudge,0+nudge,-.5+1.5*nudge),xend=c(-.5+nudge,.5-nudge,.5-nudge),ybeg=c(1-2*nudge,1-2*nudge,0),yend=c(0+2*nudge,0+2*nudge,0),labels=c("B[t-1]","P[t-1]","S[t]"))

# plot the graph
gg_dag <- ggplot(dt)+
  geom_text(aes(x=x,y=y,label=labels),size=8,parse=T,color=c("coral","darkgray","coral"))+
  geom_segment(aes(x=xbeg,y=ybeg,xend=xend,yend=yend),lineend = c("round"),linejoin = c("round"),arrow=arrow(type="closed",angle=20,length=unit(0.2,"inches")),size=1,linetype=c(1,1,5),color=c("darkgray","darkgray","coral"))+
  geom_segment(data=dt[3,],aes(x=xend-.5*nudge,y=yend,xend=xend,yend=yend),lineend = c("round"),linejoin = c("round"),arrow=arrow(type="closed",angle=20,length=unit(0.2,"inches")),size=1,linetype=1,color="coral")+
  theme_void()+
  coord_cartesian(clip="off")+
  theme_void()+
  theme_guess()+
  theme(axis.title = element_blank(),axis.text = element_blank(),axis.line = element_blank(),plot.margin = margin(.5,.5,.5,.5, "cm"))

ggsave("figures/dag_birds.png",gg_dag,width=6.5,height=3.5,dpi="retina",device="png")
ggsave("figures/dag_birds.eps",gg_dag,width=6.5,height=3.5,dpi="retina",device="eps")

