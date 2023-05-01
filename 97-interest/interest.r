# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(magick)
library(stringr)
library(fredr)
fredr_set_key("Your-API-Key") # you need to obtain an API key from FRED

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
fr_dt <- as.data.table(fredr(
  series_id = c("DFF"),
  observation_start = as.Date("1972-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = "m",
  aggregation_method = "avg"
))

fr_dt <- fr_dt[,.(date,ffr=value)]

# u.s. cpi
cpi_dt <- as.data.table(fredr(
  series_id = c("CPIAUCSL"),
  observation_start = as.Date("1971-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = "m",
  aggregation_method = "avg"
))

ir_dt <- cpi_dt[,.(date,inf=round((value/shift(value,12)-1)*100,2))] 

# combine the series
rates_dt <- merge(fr_dt,ir_dt,by="date")
rates_dt[,period:=ifelse(date<"1992-01-01","1977-1991",ifelse(date<"2002-01-01","1992-2001","2002-2022"))]

# the size of the window
lag <- 60

# loop through the plots
for(i in 0:lag){
  
  rates_dt[,ffl:=shift(ffr,i)]
  
  gg <- ggplot(rates_dt[date>="1977-01-01"],aes(x=ffl,y=inf,color=period))+
    geom_point(alpha=.5)+
    geom_smooth(method="lm",formula=y~x,se=F,linetype=5)+
    scale_color_manual(values=c("darkgray","goldenrod","indianred"))+
    labs(title="Interest Rate and Inflation",x=paste0("Federal Funds Rate (Lag ",i,", %)"),y="Inflation Rate (Annual, %)",caption = "Created by @DavidUbilava | Data: U.S. BLS and Board of Governors of the FRS retrieved from FRED via fredr package")+
    theme_eg()+
    theme(legend.position="top",legend.key=element_rect(fill="transparent"))
  
  ggsave(paste0("temp/l",str_pad(i,2,pad="0"),".png"),width=1600,height=1000,unit="px",dpi=200)
  
}

# this next few lines create a gif from the pre-generated images

## create a list of the png file names in the temp folder
the_list <- paste0("temp/",list.files("temp/"))

## store the graphs as a list in frames
frames <- lapply(the_list,image_read)

## generate a gif
animation <- image_animate(image_join(frames),fps=5)

## save the gif
image_write(animation,"interest_inflation.gif")
