# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(Cairo)
library(cowplot)
library(magick)
# library(showtext)
# font_add_google(name="Syne Mono")
# showtext_auto()
# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(tidyverse)

# install.packages("ggalluvial")
library(ggalluvial)

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
      panel.grid.major = element_line(colour="dimgray"),
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
      legend.text=element_text(family=base_family,size=rel(0.9),colour="dimgray"),
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
  labs(title="Bates Clark Medalists' Whereabouts",caption="Created by @DavidUbilava | Data: Wikipedia")+
  coord_cartesian(clip = 'off')+
  theme_guess()+
  theme(axis.text.y=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),axis.line.x=element_blank(),plot.margin=unit(c(0.25,2.75,0.25,2.75),"lines"))

ggsave("batesclark_sankey.png",gg_batesclark,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("batesclark_sankey.eps",gg_batesclark,width=6.5,height=3.75,dpi="retina",device=cairo_ps)

batesclark_dt[,`:=`(Years_to_Nobel=Nobel-Year,Years_to_Death=ifelse(!is.na(Died),Died,2023)-Year)]

# batesclark_dt[is.na(Years_to_Death)]$Years_to_Death <- 99

ggplot(batesclark_dt)+
  geom_rect(aes(xmin=14.5,xmax=26.5,ymin=-Inf,ymax=Inf),fill="lightgray",alpha=.5)+
  annotate("text",family="mono",fontface="bold",label="Nobel\nTerritory",x=20.5,y=2021,size=5,colour="coral")+
  geom_segment(aes(x=0,xend=Years_to_Death,y=Year,yend=Year),color=ifelse(is.na(batesclark_dt$Died),"coral","darkgray"),linewidth=.5)+
  geom_point(aes(x=Years_to_Nobel,y=Year),color="coral",size=2,na.rm=T)+
  labs(title="Bates Clark Medalists",x="Years from the Award",y="",caption = "Created by @DavidUbilava | Data: Wikipedia")+
  theme_guess()+
  theme(axis.line=element_blank(),panel.grid.major = element_blank(),axis.ticks=element_blank(),plot.margin=margin(.25,.25,.25,1.75,"lines"))





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

# institutions ----

# degree-granting institutions (top-10 list)
almamater_dt <- dt[,.(.N),by=.(PhD)]
almamater_dt <- almamater_dt[order(-N)]
almamater_dt$PhD <- factor(almamater_dt$PhD,levels=almamater_dt$PhD)

almamater_dt[,`:=`(PhD_label=paste0(PhD," (",N,")"),location=ifelse(N>6,0.5,N+0.5),labelcol=ifelse(N>6,"white","coral"))]

gg_schools <- ggplot(almamater_dt[N>=4],aes(x=PhD,y=N))+
  geom_bar(stat="identity",fill="coral")+
  geom_text(aes(y=location,label=PhD_label,color=labelcol),size=3,hjust=0,vjust=0.5,angle=90)+
  scale_color_manual(values=c("coral","white"))+
  labs(title="Top 10 PhD-granting universities of the Nobel Prize laureates in Economics",x="",y="",caption = "Created by @DavidUbilava | Data: Wikipedia and other online sources")+
  theme_guess()+
  theme(axis.line=element_blank(),axis.text.x=element_blank())

# add logo
gg_schools <- ggdraw(gg_schools) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/nobel_schools.png",gg_schools,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/nobel_schools.eps",gg_schools,width=6.5,height=3.75,dpi="retina",device=cairo_ps)



# age and children ----

dt[,`:=`(Age=Year-Born)]

dt[,id := seq_len(.N),by=Year]

# plot ages and children of nobel prize laureates in economics
gg_age <- ggplot(dt,aes(x=Year,y=Age,fill=factor(id)))+
  geom_bar(stat="identity",width=.8,position=position_dodge(width=.9))+
  scale_fill_manual(values=c("coral","goldenrod1","indianred"))+
  coord_cartesian(ylim=c(0,90))+
  labs(title="The age at the time of the award")+
  theme_guess()+
  theme(axis.line=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank(),plot.margin=margin(.25,.25,.25,3.25,"lines"))

gg_agedots <- ggplot(dt,aes(x=Age))+
  geom_dotplot(fill="coral",color="lightgray",binwidth=2.2,stackratio=1.1)+
  coord_flip(xlim=c(0,90))+
  labs(title="",x="",y="",caption="")+
  theme_guess()+
  theme(axis.line = element_blank(),axis.text.x=element_blank(),axis.title = element_blank(),plot.margin=margin(.25,.25,.25,.25,"lines"))


gg_children <- ggplot(dt,aes(x=Year,y=Children,group=factor(id)))+
  geom_bar(stat="identity",width=.8,position=position_dodge(width=.9),fill="dimgray")+
  scale_y_reverse()+
  coord_cartesian(ylim=c(6,0))+
  labs(title="Number of children of the laureate",x="",y="",caption="Created by @DavidUbilava | Data: Wikipedia and other online sources")+
  theme_guess()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title=element_blank(),plot.margin=margin(.25,.25,.25,3.25,"lines"))

gg_childrendots <- ggplot(dt,aes(x=Children))+
  geom_dotplot(fill="lightgray",color="dimgray",binwidth=.22,stackratio=1.1)+
  scale_x_reverse()+
  coord_flip(xlim=c(6,0))+
  labs(title="",x="",y="",caption="")+
  theme_guess()+
  theme(axis.line = element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),plot.margin=margin(.25,.25,.25,.25,"lines"))

gg_combined1 <- plot_grid(gg_age,gg_agedots,gg_children,gg_childrendots,nrow=2,align="h",rel_heights = c(3,2),rel_widths = c(7,3))

gg_combined1 <- ggdraw(gg_combined1) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

gg_combined1


ggsave("figures/nobels.png",gg_combined1,width=6.5,height=4.75,dpi="retina",device="png")
ggsave("figures/nobels.eps",gg_combined1,width=6.5,height=4.75,dpi="retina",device=cairo_ps)


# lives after nobel ----

life_dt <- dt[order(Born,Year,Died)]
life_dt[,id := seq_len(.N)]

life_dt[,`:=`(Life=Died-Born,Life_After=Died-Year)]

mean(life_dt$Life,na.rm=T)
mean(life_dt$Life_After,na.rm=T)

life_dt[is.na(Died)]$Died <- 2023

gg_lives <- ggplot(life_dt)+
  geom_segment(aes(x=Born,xend=Died,y=id,yend=id),color="gray",linewidth=.5)+
  geom_point(aes(x=Year,y=id),color="coral",size=.8)+
  labs(title="Lives of Nobel Prize laureates in Economics",x="Year",y="",caption = "Created by @DavidUbilava | Data: Wikipedia and other online sources")+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text.y = element_blank(),plot.margin=margin(.25,.25,.25,1.75,"lines"))

gg_afterlives <- ggplot(life_dt)+
  geom_segment(aes(x=0,xend=Life_After,y=id,yend=id),color="coral",linewidth=.5,na.rm=T)+
  labs(title="Years after the Nobel",x="",y="",caption="")+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text.y = element_blank(),axis.title = element_blank(),plot.margin=margin(.25,.25,.25,.25,"lines"))

gg_combined2 <- plot_grid(gg_lives,gg_afterlives,ncol=2,align="h",rel_widths = c(5,3))

gg_combined2 <- ggdraw(gg_combined2) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

gg_combined2

ggsave("figures/nobel_lives.png",gg_combined2,width=6.5,height=3.75,dpi="retina",device="png")
ggsave("figures/nobel_lives.eps",gg_combined2,width=6.5,height=3.75,dpi="retina",device=cairo_ps)
