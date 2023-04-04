# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(Cairo)
library(cowplot)
library(magick)

# plot aesthetics
theme_guess <- function(){
  theme(
    panel.background=element_rect(fill="transparent",color=NA),
    plot.background=element_rect(fill="transparent",color=NA),
    plot.title=element_text(size=12,colour="dimgray"),
    plot.caption = element_text(colour="slategray",hjust=0),
    plot.margin=unit(c(0,0,0,1.5),"lines"),
    axis.title=element_text(size=12,colour="dimgray"),
    axis.text=element_text(size=10,colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line=element_line(colour="darkgray"),
    axis.ticks=element_line(colour="darkgray"),
    legend.background=element_rect(fill="transparent",color=NA),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="dimgray"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(size=10,colour="dimgray",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}

# load the logo (for branding)
logo <- image_read("logo.png")

# load data
load("data/nobel_economics.RData")

# institutions ----

# degree-granting institutions (top-10 list)
almamater_dt <- dt[,.(.N),by=.(PhD)]
almamater_dt <- almamater_dt[order(-N)]
almamater_dt$PhD <- factor(almamater_dt$PhD,levels=almamater_dt$PhD)

almamater_dt[,`:=`(PhD_label=paste0(PhD," (",N,")"),location=ifelse(N>6,0.5,N+0.5),labelcol=ifelse(N>6,"white","coral"))]

gg_schools <- ggplot(almamater_dt[N>=4],aes(x=PhD,y=N))+
  geom_bar(stat="identity",fill="coral")+
  geom_text(aes(y=location,label=PhD_label,color=labelcol),hjust=0,vjust=0.5,angle=90)+
  scale_color_manual(values=c("coral","white"))+
  labs(title="Top 10 PhD granting universities for the Nobel Prize laureates in Economics",x="",y="",caption = "Created by @DavidUbilava\nData from Wikipedia and other online sources")+
  theme_classic()+
  theme_guess()+
  theme(axis.line=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank())

# add logo
gg_schools <- ggdraw() +
  draw_image(logo,scale=.15,x=1,hjust=1,halign=0,valign=0,clip="off") +
  draw_plot(gg_schools)

ggsave("figures/nobel_schools.png",gg_schools,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/nobel_schools.eps",gg_schools,width=6.5,height=4.5,dpi="retina",device=cairo_ps)



# age and children ----

dt[,`:=`(Age=Year-Born)]

dt[,id := seq_len(.N),by=Year]

# plot ages and children of nobel prize laureates in economics
gg_age <- ggplot(dt,aes(x=Year,y=Age,fill=factor(id)))+
  geom_bar(stat="identity",width=.8,position=position_dodge(width=.9))+
  scale_fill_manual(values=c("coral","goldenrod1","indianred"))+
  coord_cartesian(ylim=c(0,90))+
  labs(title="Age of the laureate at the time of award of the Nobel Prize in Economics")+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank(),plot.margin=margin(0,0,0,3.5,"lines"))

gg_agedots <- ggplot(dt,aes(x=Age))+
  geom_dotplot(fill="coral",color="lightgray",binwidth=3,method="histodot")+
  coord_flip(xlim=c(0,90))+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(),axis.title = element_blank(),plot.margin=margin(0,0,0,0,"lines"))


gg_children <- ggplot(dt,aes(x=Year,y=Children,group=factor(id)))+
  geom_bar(stat="identity",width=.8,position=position_dodge(width=.9),fill="darkgray")+
  scale_y_reverse()+
  theme_classic()+
  theme_guess()+
  labs(title="Number of children of the laureate",caption = "Created by @DavidUbilava\nData from Wikipedia and other online sources")+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),axis.title = element_blank(),plot.margin=margin(0,0,0,3.5,"lines"))

gg_childrendots <- ggplot(dt,aes(x=Children))+
  geom_dotplot(fill="lightgray",color="darkgray",binwidth=.4)+
  scale_x_reverse()+
  coord_flip()+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks=element_blank(),plot.margin=margin(0,0,0,0,"lines"))

gg_combined1 <- plot_grid(gg_age,gg_agedots,gg_children,gg_childrendots,nrow=2,align="h",rel_heights = c(3,2),rel_widths = c(5,3))

gg_combined1 <- ggdraw() +
  draw_image(logo,scale=.15,x=1,hjust=1,halign=0,valign=0,clip="off") +
  draw_plot(gg_combined1)


ggsave("figures/nobels.png",gg_combined1,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/nobels.eps",gg_combined1,width=6.5,height=4.5,dpi="retina",device=cairo_ps)





# lives after nobel ----

life_dt <- dt[order(Born,Year,Died)]
life_dt[,id := seq_len(.N)]

life_dt[,`:=`(Life=Died-Born,Life_After=Died-Year)]

mean(life_dt$Life,na.rm=T)
mean(life_dt$Life_After,na.rm=T)

life_dt[is.na(Died)]$Died <- 2023

gg_lives <- ggplot(life_dt)+
  geom_segment(aes(x=Born,xend=Died,y=id,yend=id),color="darkgray",linewidth=.5)+
  geom_point(aes(x=Year,y=id),color="coral",size=1)+
  labs(title="Lives of the Nobel Prize laureates in Economics",x="Year",y="",caption = "Created by @DavidUbilava\nData from Wikipedia and other online sources")+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text.y = element_blank(),plot.margin=margin(0,0,0,2.5,"lines"))

gg_afterlives <- ggplot(life_dt)+
  geom_segment(aes(x=0,xend=Life_After,y=id,yend=id),color="coral",linewidth=.5,na.rm=T)+
  labs(title="Years after the Nobel")+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text.y = element_blank(),axis.title = element_blank(),plot.margin=margin(0,0,0,0,"lines"))

gg_combined2 <- plot_grid(gg_lives,gg_afterlives,ncol=2,align="h",rel_widths = c(5,3))

gg_combined2 <- ggdraw() +
  draw_image(logo,scale=.15,x=1,hjust=1,halign=0,valign=0,clip="off") +
  draw_plot(gg_combined2)

gg_combined2

ggsave("figures/nobel_lives.png",gg_combined2,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/nobel_lives.eps",gg_combined2,width=6.5,height=4.5,dpi="retina",device=cairo_ps)
