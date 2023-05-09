# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
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
load("phillips.RData")

rates_dt[,`:=`(ur1=shift(ur,1),inf1=shift(inf,1),ur_f=as.numeric(NA),inf_f=as.numeric(NA),ur_g=as.numeric(NA),inf_g=as.numeric(NA))]


## regression

summary(lm(inf~ur,data=rates_dt))

summary(lm(inf~ur1+inf1,data=rates_dt))
summary(lm(ur~ur1+inf1,data=rates_dt))

# vector of dates
date_vec <- rates_dt$date

# the size of the window
w <- 120

# loop through the plots
for(i in 1:(length(date_vec)-w+1)){
  
  rates_dt[,highlight:=ifelse(date>=date_vec[i] & date<=date_vec[w-1+i],"Y","N")]
  
  # gg <- ggplot(rates_dt,aes(x=ur,y=inf,color=highlight))+
  #   geom_point()+
  #   geom_smooth(data=rates_dt[highlight=="Y"],method="lm",formula=y~x,se=F,linetype=5,color="coral")+
  #   scale_color_manual(values=c("lightgray","dimgray"))+
  #   labs(title="The Ever Revolving Phillips Curve",subtitle=paste0(format(date_vec[i],format="%b %Y")," \U2012 ",format(date_vec[w-1+i],format="%b %Y")),x="Unemployment Rate (%)",y="Inflation Rate (Annual, %)",caption = "Created by @DavidUbilava | Data: U.S. Bureau of Labor Statistics retrieved from FRED via fredr package")+
  #   theme_eg()
  # 
  # ggsave(paste0("temp/w",str_pad(i,3,pad="0"),".png"),width=800,height=500,unit="px",dpi=100)
  
  r_ur <- lm(ur~ur1,data=rates_dt[highlight=="Y"])
  r_inf <- lm(inf~inf1,data=rates_dt[highlight=="Y"])
  
  rates_dt[date==date_vec[w-1+i]]$ur_f <- r_ur$coefficients["(Intercept)"]+r_ur$coefficients["ur1"]*rates_dt[date==date_vec[w-1+i]]$ur
  rates_dt[date==date_vec[w-1+i]]$inf_f <- r_inf$coefficients["(Intercept)"]+r_inf$coefficients["inf1"]*rates_dt[date==date_vec[w-1+i]]$inf
  
  r_ur <- lm(ur~ur1+inf1,data=rates_dt[highlight=="Y"])
  r_inf <- lm(inf~ur1+inf1,data=rates_dt[highlight=="Y"])
  
  rates_dt[date==date_vec[w-1+i]]$ur_g <- r_ur$coefficients["(Intercept)"]+r_ur$coefficients["ur1"]*rates_dt[date==date_vec[w-1+i]]$ur+r_ur$coefficients["inf1"]*rates_dt[date==date_vec[w-1+i]]$inf
  rates_dt[date==date_vec[w-1+i]]$inf_g <- r_inf$coefficients["(Intercept)"]+r_inf$coefficients["ur1"]*rates_dt[date==date_vec[w-1+i]]$ur+r_inf$coefficients["inf1"]*rates_dt[date==date_vec[w-1+i]]$inf
  
}


rates_dt[,`:=`(ur_fe=ur-ur_f,ur_ge=ur-ur_g,inf_fe=inf-inf_f,inf_ge=inf-inf_g)]

rmsfe_urf <- sqrt(mean(rates_dt$ur_fe^2,na.rm=T))
rmsfe_urg <- sqrt(mean(rates_dt$ur_ge^2,na.rm=T))
rmsfe_inff <- sqrt(mean(rates_dt$inf_fe^2,na.rm=T))
rmsfe_infg <- sqrt(mean(rates_dt$inf_ge^2,na.rm=T))

# # this next few lines create a gif from the pre-generated images
# 
# ## create a list of the png file names in the temp folder
# the_list <- paste0("temp/",list.files("temp/"))
# 
# ## store the graphs as a list in frames
# frames <- lapply(the_list,image_read)
# 
# ## generate a gif
# animation <- image_animate(image_join(frames),fps=10)
# 
# ## save the gif
# image_write(animation,"revolving-phillips.gif")




