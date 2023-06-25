library(data.table)
library(ggplot2)
library(magick)
library(stringr)
library(fredr)

theme_eg <- function(base_size=12,base_family="sans",border=F){
  theme(
    panel.background=element_rect(fill="white",color=NA),
    panel.grid=element_line(colour=NULL,linetype=3),
    panel.grid.major=element_line(colour="dimgray"),
    panel.grid.major.x=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(family=base_family,size=rel(1.2),colour="dimgray"),
    plot.caption=element_text(family=base_family,colour="darkgray"),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    axis.title=element_text(family=base_family,face="bold",size=rel(1.3),colour="dimgray"),
    axis.text=element_text(family=base_family,size=rel(1.1),colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line=element_line(colour="dimgray"),
    axis.line.y=element_blank(),
    axis.ticks=element_blank(),
    legend.background=element_rect(fill="transparent",color=NA),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(family=base_family,size=rel(1.1),colour="dimgray"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(family=base_family,size=rel(0.9),colour="dimgray",face="bold",margin=margin(.5,0,.5,0,"lines"))
  )
}


set.seed(1)
dt <- data.table(e=rnorm(100))

dev <- c(seq(0,1.5,.05),seq(1.4,-1.5,-.05),seq(-1.4,0,.05))

for(i in 1:length(dev)){
  
  dt[,e1:=e-dev[i]]
  
  dt[,`:=`(loss_quadratic=e1^2,loss_absolute=abs(e1))]
  
  el <- round(mean(dt$loss_quadratic),2)
  
  gg <- ggplot(data=dt)+
    geom_line(aes(x=e1+dev[i],y=loss_quadratic),color="dimgray",linewidth=1)+
    geom_point(aes(x=e1+dev[i],y=loss_quadratic),color="dimgray",size=2)+
    geom_point(aes(x=e,y=0),color="dimgray",fill="lightgray",stroke=1,shape=21,size=2)+
    geom_point(aes(x=dev[i],y=loss_quadratic),color="coral",fill="coral",stroke=1,shape=21,size=2)+
    geom_hline(yintercept=el,linetype=5,linewidth=1,color="coral")+
    annotate(geom="text",x=-3,y=el+.5,label=el,parse=T,hjust=0,vjust=0,color="coral",family="mono")+
    coord_cartesian(xlim=c(-3,3),ylim=c(0,14))+
    labs(title="Optimal forecast under quadratic loss",x=expression(e),y=expression(e^2))+
    theme_eg()
  
  ggsave(paste0("99-temp/files/w",str_pad(i,3,pad="0"),".png"),width=800,height=500,unit="px",dpi=100)
  
}

# this next few lines create a gif from the pre-generated images

## create a list of the png file names in the temp folder
the_list <- paste0("99-temp/files/",list.files("99-temp/files/"))

## store the graphs as a list in frames
frames <- lapply(the_list,image_read)

## generate a gif
animation <- image_animate(image_join(frames),fps=10)

## save the gif
image_write(animation,"99-temp/quadratic-loss.gif")

