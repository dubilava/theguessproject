# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(Cairo)
library(cowplot)
library(magick)
library(tidyverse)
# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# plot aesthetics
theme_guess <- function(base_size=10,base_family="sans",title_family="sans",border=F){
  theme_foundation(base_size=base_size,base_family=base_family) +
    theme(
      line = element_line(linetype=1,colour="black"),
      rect = element_rect(linetype=0,colour=NA),
      text = element_text(colour="black"),
      panel.grid = element_line(colour=NULL,linetype=3),
      panel.grid.major = element_line(colour="darkgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title=element_text(colour="black",hjust=0,size=rel(1.1)),
      plot.caption = element_text(family=base_family,size=rel(0.7),colour="slategray",hjust=0,margin=margin(t=5,r=1,b=1,l=1)),
      plot.margin=unit(c(0.25,0.25,0.25,1.25),"lines"),
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

# load data
batesclark_dt <- fread("batesclark.csv")

dt <- make_long(batesclark_dt,PhD,Institution)

gg_batesclark <- ggplot(dt,aes(x=x,next_x=next_x,node=node,next_node=next_node,fill=factor(node),label=node))+
  geom_sankey(flow.alpha=0.5,node.color=NA,show.legend=F,width=.05)+
  geom_sankey_text(size=2,color="black",hjust=c(rep(1,13),rep(0,11)))+  
  scale_fill_viridis_d(option="H",begin=.2,end=.8)+
  labs(title="Bates Clark Medalists' Whereabouts",caption="Created by @DavidUbilava | Data: Wikipedia")+
  coord_cartesian(clip = 'off')+
  theme_guess()+
  theme(axis.text.y=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),axis.line.x=element_blank(),plot.margin=unit(c(0.25,2.75,0.25,2.75),"lines"))

# add logo
gg_batesclark <- ggdraw(gg_batesclark) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("batesclark_sankey.png",gg_batesclark,width=6.5,height=3.75,dpi="retina",device="png")



batesclark_dt[,`:=`(Years_to_Nobel=Nobel-Year,Years_to_Death=ifelse(!is.na(Died),Died,2023)-Year)]

# batesclark_dt[order(Years_to_Death)]

# batesclark_dt[is.na(Years_to_Death)]$Years_to_Death <- 99

gg_batestonobel <- ggplot(batesclark_dt)+
  geom_rect(aes(xmin=14.5,xmax=26.5,ymin=-Inf,ymax=Inf),fill=NA,color="coral",linewidth=.4,linetype=2)+
  annotate("text",family="mono",fontface="bold",label="Nobel\nTerritory",x=20.5,y=2020,size=4,colour="coral")+
  geom_segment(aes(x=0,xend=Years_to_Death,y=Year,yend=Year),color=ifelse(is.na(batesclark_dt$Died),"coral","darkgray"),linewidth=.6)+
  geom_point(aes(x=Years_to_Nobel,y=Year),color="coral",size=1.2,na.rm=T)+
  labs(title="Bates Clark Medalists",x="Years from the Award",y="Year of the Award",caption = "Created by @DavidUbilava | Data: Wikipedia")+
  coord_cartesian(xlim=c(2,62))+
  theme_guess()+
  theme(axis.line=element_blank(),panel.grid.major = element_blank(),axis.ticks=element_blank(),plot.margin=margin(.25,.25,.25,1.75,"lines"))

# add logo
gg_batestonobel <- ggdraw(gg_batestonobel) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

# save the graph
ggsave("batestonobel.png",gg_batestonobel,width=6.5,height=3.75,dpi="retina",device="png")


batesclark_dt[,Years_to_Date:=(2023-Year)]

batesclark_dt[Years_to_Date>14 & Years_to_Date<27]

load("../12-nobelecon/nobel_economics.RData")

nobel_dt <- dt



dt <- merge(batesclark_dt[,.(Name,Bates=Year)],nobel_dt[,.(Name,Nobel=Year)],by=c("Name"),all=T)

# dt[is.na(dt$Bates)]$Bates <- min(dt$Nobel,na.rm=T)-1
# dt[is.na(dt$Nobel)]$Nobel <- min(dt$Bates,na.rm=T)-1

dt <- dt[complete.cases(dt)]

dt[,Time_to_Nobel:=Nobel-Bates]

ggplot(dt,aes(x=Bates,y=Nobel))+
  geom_point()+
  geom_abline(intercept=0,slope=1,linewidth=0.6,linetype=3,color="dimgray")+
  geom_abline(intercept=0+mean(dt$Time_to_Nobel),slope=1,linewidth=0.6,linetype=5,color="coral")+
  coord_cartesian(xlim=c(1945,2035),ylim=c(1945,2035))+
  labs(x="Bates Clark Medal",y="Nobel Prize")+
  theme_guess()+
  theme(axis.line.x=element_blank())


