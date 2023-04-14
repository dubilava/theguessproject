# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(Cairo)
library(cowplot)

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

women_dt <- fread("data/nobel_women.csv")
econ_dt <- fread("data/nobel_economics.csv")


women_dt[,`:=`(Age=Year-Born)]
econ_dt[,`:=`(Age=Year-Born)]


# the feature graph
women_dt[,`:=`(Field_id=ifelse(Field=="Economics",1,ifelse(Field=="Peace",2,ifelse(Field=="Literature",3,ifelse(Field=="Medicine",4,ifelse(Field=="Chemistry",5,6))))))]

life_dt <- women_dt[order(Field_id,Born,Year,Died)]
life_dt[,id := seq_len(.N)]

life_dt[,`:=`(Life=Died-Born,Life_After=Died-Year)]

life_dt[is.na(Died)]$Died <- 2023

gg_lives <- ggplot(life_dt)+
  geom_segment(aes(x=Born,xend=Died,y=id,yend=id,color=as.factor(Field_id)),linewidth=.5)+
  scale_color_manual(values=c("coral","steelblue","goldenrod","indianred","seagreen","darkgray"))+
  geom_point(aes(x=Year,y=id),color="coral",size=1)+
  annotate(geom="text", x=1800, y=2, label="Economics",color="coral",hjust=0)+
  annotate(geom="text", x=1800, y=20, label="Peace",color="steelblue",hjust=0)+
  annotate(geom="text", x=1800, y=37, label="Literature",color="goldenrod",hjust=0)+
  annotate(geom="text", x=1800, y=49, label="Medicine",color="indianred",hjust=0)+
  annotate(geom="text", x=1800, y=57, label="Chemistry",color="seagreen",hjust=0)+
  annotate(geom="text", x=1800, y=61, label="Physics",color="darkgray",hjust=0)+
  labs(title="Women Nobel Prize Laureates",x="Year",y="",caption = "Created by @DavidUbilava\nBased on data from Wikipedia and other online sources")+
  coord_cartesian(xlim=c(1800,2023))+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text.y = element_blank(),plot.margin=margin(0,0,0,0,"cm"))

gg_children <- ggplot(life_dt)+
  geom_segment(aes(x=0,xend=Children,y=id,yend=id),color="darkgray",linewidth=.5)+
  geom_point(aes(x=Children,y=id),color="darkgray",size=1)+
  labs(title="Children")+
  coord_cartesian(xlim=c(0,6))+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text.y = element_blank(),axis.title = element_blank(),plot.margin=margin(0,0,0,0,"cm"))


gg_livedots <- ggplot(life_dt,aes(x=Year))+
  geom_dotplot(fill="coral",color="coral",binwidth=1,stackdir="down")+
  coord_cartesian(xlim=c(1800,2023))+
  labs(caption = "Created by @DavidUbilava\nBased on data from Wikipedia and other online sources")+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),axis.title = element_blank(),plot.margin=margin(0,0,0,0,"cm"))

gg_childrendots <- ggplot(life_dt,aes(x=Children))+
  geom_dotplot(fill="darkgray",color="darkgray",binwidth=.2,stackdir="down")+
  coord_cartesian(xlim=c(0,6))+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),axis.title = element_blank(),plot.margin=margin(0,0,0,0,"cm"))


gg_combined <- plot_grid(gg_lives,gg_children,ncol=2,align="hv",rel_widths = c(5,1))


ggsave("figures/nobel_women.png",gg_combined,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/nobel_women.eps",gg_combined,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


children_econ_dt <- econ_dt[,.N,by=Children]
children_econ_dt <- children_econ_dt[order(Children)]
children_econ_dt[,`:=`(CDF=cumsum(N)/sum(N))]

children_women_dt <- women_dt[,.N,by=Children]
children_women_dt <- children_women_dt[order(Children)]
children_women_dt[,`:=`(CDF=cumsum(N)/sum(N))]

combined_dt 

gg_childrendots <- ggplot(women_dt,aes(x=Children))+
  geom_dotplot(fill="darkgray",color="darkgray",binwidth=.1)+
  coord_cartesian(xlim=c(0,6))+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),axis.title = element_blank(),plot.margin=margin(0,0,0,0,"cm"))




