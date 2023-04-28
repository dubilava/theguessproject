# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(magick)
library(stringr)
library(fredr)
fredr_set_key("your-api-key") # you need to obtain an API key from FRED

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


# load the data from fredr

# u.s. unemployment
ur_dt <- as.data.table(fredr(
  series_id = c("UNRATE"),
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = "m",
  aggregation_method = "avg"
))

ur_dt <- ur_dt[,.(date,ur=value)]

# u.s. cpi
cpi_dt <- as.data.table(fredr(
  series_id = c("CPIAUCSL"),
  observation_start = as.Date("1989-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = "m",
  aggregation_method = "avg"
))

ir_dt <- cpi_dt[,.(date,inf=round((value/shift(value,12)-1)*100,2))] 

# combine the series
rates_dt <- merge(ur_dt,ir_dt,by="date")

# vector of dates
date_vec <- rates_dt$date

# the size of the window
w <- 60

# loop through the plots
for(i in 1:(length(date_vec)-w+1)){
  
  rates_dt[,highlight:=ifelse(date>=date_vec[i] & date<=date_vec[w-1+i],"Y","N")]
  
  gg <- ggplot(rates_dt,aes(x=ur,y=inf,color=highlight))+
    geom_point()+
    geom_smooth(data=rates_dt[highlight=="Y"],method="lm",formula=y~x,se=F,linetype=5,color="coral")+
    scale_color_manual(values=c("lightgray","dimgray"))+
    labs(title="The Ever Revolving Phillips Curve",subtitle=paste0(format(date_vec[i],format="%b %Y")," \U2012 ",format(date_vec[w-1+i],format="%b %Y")),x="Unemployment Rate (%)",y="Inflation Rate (Annual, %)",caption = "Created by @DavidUbilava | Data: U.S. Bureau of Labor Statistics retrieved from FRED via fredr package")+
    theme_eg()
  
  ggsave(paste0("temp/w",str_pad(i,3,pad="0"),".png"),width=800,height=500,unit="px",dpi=100)
  
}

# this next few lines create a gif from the pre-generated images

## create a list of the png file names in the temp folder
the_list <- paste0("temp/",list.files("temp/"))

## store the graphs as a list in frames
frames <- lapply(the_list,image_read)

## generate a gif
animation <- image_animate(image_join(frames),fps=10)

## save the gif
image_write(animation,"revolving-phillips.gif")
