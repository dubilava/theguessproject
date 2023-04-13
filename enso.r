# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(Cairo)
library(cowplot)
library(magick)
library(stringr)

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
      panel.grid.major = element_line(colour="darkgray"),
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
      legend.text=element_text(family=base_family,size=rel(0.9),colour="slategray"),
      legend.key = element_rect(fill="transparent"),
      legend.key.size=unit(.75,'lines'),
      strip.background=element_blank(),
      strip.text=element_text(size=rel(.8),colour="slategray",margin=margin(.1,0,.1,0,"cm"))
    )
}

# load the logo (for branding)
logo <- image_read("logo.png")

# load the data

# enso observations ----
enso_dt <- fread("https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt")

enso_dt[,`:=`(Date=as.Date(paste0(YR,"-",str_pad(MON,2,pad="0"),"-01")))]

gg_enso <- ggplot(enso_dt[Date>="1980-01-01"],aes(x=Date,y=ANOM,color=ANOM))+
  geom_line(linewidth=.8)+
  scale_color_gradient2(low="steelblue",mid="gray",high="coral")+
  labs(title="Sea Surface Temperature Anomaly in the Nino3.4 Region",x="Year",y="\u00B0C",caption="Created by @DavidUbilava | Data: NOAA Climate Prediction Center (https://www.cpc.ncep.noaa.gov/data/indices/)")+
  theme_guess()

gg_enso <- ggdraw(gg_enso) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/enso_ts.png",gg_enso,width=6.5,height=3.75)


# enso forecasts ----

# iri plumes (from these two sources)
# https://iri.columbia.edu/~forecast/ensofcst/Data/archive/ensofcst_ALL
# https://iri.columbia.edu/~forecast/ensofcst/Data/archive/ensofcst_cpc_ALL

plume_dt <- fread("data/plume.csv")
plume_dt <- plume_dt[Model!="CPC CONSOL"]

plume_lg <- melt(plume_dt,id.vars=c("Model","Type","Year"),variable.name="Season",value.name="Forecast")

plume_lg <- plume_lg[order(Year,Type,Model,Season)]
plume_lg$Season <- factor(plume_lg$Season,levels=colnames(plume_dt)[1:9])

plume_lg[,`:=`(Forecast=Forecast/100)]
plume_ave <- data.table(Model="Average",Type="A",plume_lg[Model!="CPC CONSOL",.(Forecast=mean(Forecast,na.rm=T)),by=.(Year,Season)])

plume_lg <- rbind(plume_lg,plume_ave)


## generate ONI index from the enso data

enso_dt[,`:=`(ONI=round(frollmean(ANOM,n=3,algo="exact",align="center"),2))]

month_to_season <- data.table(MON=1:12,Season=c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ"))

enso_dt <- merge(enso_dt,month_to_season,by="MON",all.x=T)
enso_dt <- enso_dt[order(Date)]

oni_dt <- enso_dt[YR%in%c(2015,2017) & MON %in% c(4:12),.(Model="Actual",Type="A",Year=YR,Season,Forecast=ONI)]

plume_lg <- rbind(plume_lg,oni_dt)

plume_lg[,`:=`(Legend=ifelse(Type%in%c("D","S"),"Model Forecasts",ifelse(Model=="Average","Average Forecast","Realized Value")))]

plume_lg$Legend <- factor(plume_lg$Legend,levels=unique(plume_lg$Legend))

gg_plume <- ggplot(plume_lg[Year==2023],aes(x=Season,y=Forecast,group=Model))+
  geom_line(data=plume_lg[Year==2023 & Legend=="Model Forecasts"],aes(color=Legend,linetype=Legend),na.rm=T,linewidth=.8)+
  geom_line(data=plume_lg[Year==2023 & Legend=="Average Forecast"],aes(color=Legend,linetype=Legend),na.rm=T,linewidth=.8)+
  scale_color_manual(values = c("black","darkgray"),guide=guide_legend(reverse=T))+
  scale_linetype_manual(values = c(5,1),guide=guide_legend(reverse=T))+
  coord_cartesian(ylim=c(-1,2.5))+
  labs(title="2023 ENSO Forecasts",x="Season",y="\u00B0C",caption="Created by @DavidUbilava | Data: International Research Institute for Climate and Society (https://iri.columbia.edu/)")+
  theme_guess()+
  theme(legend.position = "top")

gg_plume <- ggdraw(gg_plume) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/enso_plume.png",gg_plume,width=6.5,height=3.75)


gg_plume15 <- ggplot(plume_lg[Year==2015],aes(x=Season,y=Forecast,group=Model))+
  geom_line(data=plume_lg[Year==2015 & Legend=="Model Forecasts"],aes(color=Legend,linetype=Legend),na.rm=T,linewidth=.8)+
  geom_line(data=plume_lg[Year==2015 & Legend=="Average Forecast"],aes(color=Legend,linetype=Legend),na.rm=T,linewidth=.8)+
  geom_line(data=plume_lg[Year==2015 & Legend=="Realized Value"],aes(color=Legend,linetype=Legend),na.rm=T,linewidth=.8)+
  scale_color_manual(values = c("black","darkgray","coral"),guide=guide_legend(reverse=T))+
  scale_linetype_manual(values = c(5,1,1),guide=guide_legend(reverse=T))+
  coord_cartesian(ylim=c(-1,2.5))+
  labs(title="2015 ENSO Forecasts",x="Season",y="\u00B0C",caption="Created by @DavidUbilava | Data: International Research Institute for Climate and Society (https://iri.columbia.edu/)")+
  theme_guess()+
  theme(legend.position = "top")

gg_plume15 <- ggdraw(gg_plume15) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/enso_plume15.png",gg_plume15,width=6.5,height=3.75)


gg_plume17 <- ggplot(plume_lg[Year==2017],aes(x=Season,y=Forecast,group=Model))+
  geom_line(data=plume_lg[Year==2017 & Legend=="Model Forecasts"],aes(color=Legend,linetype=Legend),na.rm=T,linewidth=.8)+
  geom_line(data=plume_lg[Year==2017 & Legend=="Average Forecast"],aes(color=Legend,linetype=Legend),na.rm=T,linewidth=.8)+
  geom_line(data=plume_lg[Year==2017 & Legend=="Realized Value"],aes(color=Legend,linetype=Legend),na.rm=T,linewidth=.8)+
  scale_color_manual(values = c("black","darkgray","coral"),guide=guide_legend(reverse=T))+
  scale_linetype_manual(values = c(5,1,1),guide=guide_legend(reverse=T))+
  coord_cartesian(ylim=c(-1,2.5))+
  labs(title="2017 ENSO Forecasts",x="Season",y="\u00B0C",caption="Created by @DavidUbilava | Data: International Research Institute for Climate and Society (https://iri.columbia.edu/)")+
  theme_guess()+
  theme(legend.position = "top")

gg_plume17 <- ggdraw(gg_plume17) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/enso_plume17.png",gg_plume17,width=6.5,height=3.75)

