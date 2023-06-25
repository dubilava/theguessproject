# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(cowplot)
library(magick)
library(stringr)

# plot aesthetics
theme_eg <- function(base_size=12,border=F){
  theme(
    panel.background=element_rect(fill="white",color=NA),
    panel.grid=element_line(colour=NULL,linetype=3),
    panel.grid.major=element_line(colour="dimgray"),
    panel.grid.major.x=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill="white",color=NA),
    plot.title=element_text(size=rel(1.2),colour="dimgray"),
    plot.subtitle=element_text(family="mono",size=rel(1.1),colour="black"),
    plot.caption = element_text(colour="darkgray"),
    plot.margin=margin(.25,.25,.25,.25,"lines"),
    axis.title=element_text(size=rel(1.4),colour="dimgray"),
    axis.text=element_text(size=rel(1.2),colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line = element_line(colour="dimgray"),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    legend.background=element_rect(fill="transparent",color=NA),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=rel(1.2),colour="dimgray"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(size=rel(0.8),colour="dimgray",face="bold",margin=margin(.5,0,.5,0,"lines"))
  )
}


# load the data, originally fetched via fredr
set.seed(2)
n <- 600
e <- rnorm(n)
y <- cumsum(e)
y_dt <- data.table(t=1:n,y=y)

gg_wn <- ggplot(y_dt,aes(x=t,y=y))+
  geom_line(linewidth=.5,color="black")+
  ylim(-5,65)+
  xlim(0,n)+
  labs(x="t",y=expression(y[t]))+
  theme_eg()

# graph the dot-density of the series
gg_dots <- ggplot(y_dt,aes(x=y))+
  geom_dotplot(binwidth=.5,color="black",fill="gray",stroke=1,method="histodot",stackratio=1.1)+
  xlim(-5,65)+
  coord_flip()+
  theme_eg()+
  theme(axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank())

# combine the two graphs
gg_comb <- plot_grid(gg_wn,gg_dots,align="hv",ncol=2,rel_widths = c(3,1))
# gg_comb

# the size of the window
w <- 120

# loop through the plots
for(i in 1:(n-w+1)){
  
  y_dt[,highlight:=ifelse(t>=i & t<=w-1+i,"Y","N")]
  
  gg_wn <- ggplot(y_dt,aes(x=t,y=y))+
    geom_line(linewidth=.5,color=ifelse(y_dt$highlight=="Y","coral","gray"))+
    ylim(-5,65)+
    xlim(0,n*1.05)+
    labs(x="t",y=expression(y[t]))+
    theme_eg()
  
  # graph the dot-density of the series
  gg_dots <- ggplot(y_dt[highlight=="Y"],aes(x=y))+
    geom_dotplot(binwidth=1,color="coral",fill="coral",stroke=1,method="histodot",stackratio=1.1)+
    xlim(-5,65)+
    coord_flip()+
    theme_eg()+
    theme(axis.title=element_blank(),axis.title.y=element_blank(),axis.text=element_blank(),axis.line=element_blank())
  
  # combine the two graphs
  gg_comb <- plot_grid(gg_wn,gg_dots,align="hv",ncol=2,rel_widths = c(3,1))
  
  ggsave(paste0("temp/w",str_pad(i,4,pad="0"),".png"),width=800,height=500,unit="px",dpi=100)
  
}

# this next few lines create a gif from the pre-generated images

## create a list of the png file names in the temp folder
the_list <- paste0("temp/",list.files("temp/"))

## store the graphs as a list in frames
frames <- lapply(the_list,image_read)

## generate a gif
animation <- image_animate(image_join(frames),fps=20)

## save the gif
image_write(animation,"nonstationary.gif")




