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


dt <- fread("data/nobel_economics.csv")

dt[,`:=`(Age=Year-Born)]

dt[, id := seq_len(.N), by = Year]
dt[, id := rowid(Year)]

# plot realizations against forecasts with 'mincer-zarnowitz lines'
gg1 <- ggplot(dt,aes(x=Year,y=Age,fill=factor(id)))+
  geom_bar(stat="identity",width=.8,position=position_dodge(width=.9))+
  scale_fill_manual(values=c("coral","goldenrod1","indianred"))+
  labs(title="Nobel Prize Laureates in Economics")+
  theme_classic()+
  theme_guess()+
  theme(axis.line.x = element_blank(),axis.ticks.x=element_blank(),plot.margin=margin(.5,0,0,0,"cm"))

gg2 <- ggplot(dt,aes(x=Year,y=Children,group=factor(id)))+
  geom_bar(stat="identity",width=.8,position=position_dodge(width=.9),fill="darkgray")+
  scale_y_reverse()+
  theme_classic()+
  theme_guess()+
  labs(caption = "Created by @DavidUbilava using data from Wikipedia and other online sources")+
  theme(axis.line.x = element_blank(),axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x=element_blank(),plot.margin=margin(0,0,0,0,"cm"))

gg_combined <- plot_grid(gg1,gg2,nrow=2,align="v",rel_heights = c(3,2))

ggsave("figures/nobels.png",gg_combined,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/nobels.eps",gg_combined,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


almamater_dt <- dt[,.(.N),by=.(PhD)]
almamater_dt <- almamater_dt[order(-N)]
almamater_dt$PhD <- factor(almamater_dt$PhD,levels=almamater_dt$PhD)

almamater_dt[,`:=`(PhD_label=paste0(PhD," (",N,")"),location=ifelse(N>6,0.5,N+0.5),labelcol=ifelse(N>6,"white","coral"))]

gg_schools <- ggplot(almamater_dt[N>=4],aes(x=PhD,y=N))+
  geom_bar(stat="identity",fill="coral")+
  geom_text(aes(y=location,label=PhD_label,color=labelcol),hjust=0,vjust=0.5,angle=90)+
  scale_color_manual(values=c("coral","white"))+
  labs(title="Top 10 universities that are alma mater to Nobel prize laureates in economics",x="",y="",caption = "Created by @DavidUbilava using data from Wikipedia and other online sources")+
  theme_classic()+
  theme_guess()+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.text.x = element_blank())

ggsave("figures/nobel_PhD.png",gg_schools,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/nobel_PhD.eps",gg_schools,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


