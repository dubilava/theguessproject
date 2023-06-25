library(ggplot2)
library(data.table)
library(magick)
library(cowplot)
library(showtext)
font_add_google(name="Syne Mono")
showtext_auto()

circle <- function(center=c(0,0),diameter=1,npoints=1000){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  x <- center[1] + r * cos(tt)
  y <- center[2] + r * sin(tt)
  return(data.table(x,y))
}

square <- function(center=c(0,0),height=1,npoints=50){
  r = height / 2
  x <- c(seq(center[1]-r,center[1]+r,length.out=npoints),rep(center[1]+r,npoints),seq(center[1]+r,center[1]-r,length.out=npoints),rep(center[1]-r,npoints))
  y <- c(rep(center[2]-r,npoints),seq(center[2]-r,center[2]+r,length.out=npoints),rep(center[2]+r,npoints),seq(center[2]+r,center[2]-r,length.out=npoints))
  return(data.table(x,y))
}

c1_dt <- circle()
c2_dt <- circle(diameter=.95)

c1_dt$n <- 1:1000
c2_dt$n <- 1:1000

c2_dt <- c2_dt[order(-n)]

c_dt <- rbind(c1_dt,c2_dt)
c_dt <- rbind(c_dt,c_dt[1,])

s1_dt <- square(center=c(0,-.1),height=.8)
s2_dt <- square(center=c(0,-.1),height=.75)

s1_dt$n <- 1:200
s2_dt$n <- 1:200

s2_dt <- s2_dt[order(-n)]

s_dt <- rbind(s1_dt,s2_dt)
s_dt <- rbind(s_dt,s_dt[1,])

gg <- ggplot()+
  geom_polygon(data=c_dt,aes(x=x,y=y),linewidth=1,color=NA,fill="coral")+
  geom_polygon(data=s_dt,aes(x=x,y=y),linewidth=1,color=NA,fill="coral")+
  annotate("text",x=0,y=-.03,label="The\nGuess\nProject",size=42,color="coral",lineheight=.3,family="Syne Mono")+
  theme_void()

ggsave("logo.png",gg,width=2^10,height=2^10,units="px")

logo <- image_read("logo.png")

gg_blank <- ggplot(data=data.table(x=c(0,1),y=c(0,1))) +
  geom_blank()+
  theme_void()+
  theme(panel.background=element_rect(fill="white",color=NA))

# add logo
gg_blank <- ggdraw(gg_blank) +
  draw_image(logo,scale=1,x=0,y=0,clip="off")

ggsave("logo_wide.png",gg_blank,width=1280,height=640,units="px")

