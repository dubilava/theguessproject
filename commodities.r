# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(Cairo)
library(cowplot)
library(magick)
library(stringr)
library(fredr)

"%!in%" <- Negate("%in%")
# fredr_set_key("7a1db535f59c2ac4382b9c22a15b5f06")

# check

fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01")
)

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

# read data

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

## generate ONI index

enso_dt[,`:=`(ONI=round(frollmean(ANOM,n=3,algo="exact",align="center"),2))]

month_to_season <- data.table(MON=1:12,Season=c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ"))

enso_dt <- merge(enso_dt,month_to_season,by="MON",all.x=T)
enso_dt <- enso_dt[order(Date)]

sub_dt <- enso_dt[YR%in%c(2009,2015) & MON %in% c(4:12),.(Model="Actual",Type="A",Year=YR,Season,Forecast=ONI)]

## load plumes
plume_dt <- fread("data/plume.csv")
plume_dt <- plume_dt[Model!="CPC CONSOL"]

plume_lg <- melt(plume_dt,id.vars=c("Model","Type","Year"),variable.name="Season",value.name="Forecast")

plume_lg <- plume_lg[order(Year,Type,Model,Season)]
plume_lg$Season <- factor(plume_lg$Season,levels=colnames(plume_dt)[1:9])

plume_lg[,`:=`(Forecast=Forecast/100)]
plume_ave <- data.table(Model="Average",Type="A",plume_lg[Model!="CPC CONSOL",.(Forecast=mean(Forecast,na.rm=T)),by=.(Year,Season)])

plume_lg <- rbind(plume_lg,plume_ave)

plume_lg <- rbind(plume_lg,sub_dt)

gg_plume <- ggplot(plume_lg[Type!="A" & Year==2023],aes(x=Season,y=Forecast,group=Model))+
  geom_line(color="darkgray",linewidth=.8,na.rm=T)+
  geom_line(data=plume_lg[Model=="Average" & Year==2023],color="black",linewidth=.8,linetype=5)+
  # geom_line(data=plume_lg[Model=="Average"],color="coral",linewidth=.8,linetype=1)+
  # coord_cartesian(ylim=c(-2,2))+
<<<<<<< HEAD
  labs(title="ENSO Forecasts",x="Season",y="\u00B0C",caption="Created by @DavidUbilava | Data: International Research Institute for Climate and Society (https://iri.columbia.edu/)")+
=======
  labs(title="2023 ENSO Forecasts",x="Season",y="\u00B0C",caption="Created by @DavidUbilava | Data: International Research Institute for Climate and Society (https://iri.columbia.edu/)")+
>>>>>>> d4418d94e9172f1d71a7a1c83782c140582ddeb2
  theme_guess()

gg_plume <- ggdraw(gg_plume) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/enso_plume.png",gg_plume,width=6.5,height=3.75)


<<<<<<< HEAD

## enso
enso_dt <- fread("https://www.cpc.ncep.noaa.gov/data/indices/ersst5.nino.mth.91-20.ascii")

colnames(enso_dt)[c(4,6,8,10)] <- c("ANOM1+2","ANOM3","ANOM4","ANOM3.4")

enso_dt[,`:=`(Date=as.Date(paste0(YR,"-",str_pad(MON,2,pad="0"),"-01")))]

gg_enso <- ggplot(enso_dt[Date>="1980-01-01"],aes(x=Date,y=ANOM3.4))+
  geom_line(color="coral",linewidth=.8)+
  labs(title="Sea Surface Temperature Anomaly in the Nino3.4 Region",x="Year",y="\u00B0C",caption="Created by @DavidUbilava | Data: Climate Prediction Center of NOAA (https://www.cpc.ncep.noaa.gov/)")+
=======
gg_plume15 <- ggplot(plume_lg[Type!="A" & Year==2015],aes(x=Season,y=Forecast,group=Model))+
  geom_line(color="darkgray",linewidth=.8,na.rm=T)+
  geom_line(data=plume_lg[Model=="Average" & Year==2015],color="black",linewidth=.8,linetype=5)+
  geom_line(data=plume_lg[Model=="Actual" & Year==2015],color="coral",linewidth=.8,linetype=1)+
  # coord_cartesian(ylim=c(-2,2))+
  labs(title="2015 ENSO Forecasts",x="Season",y="\u00B0C",caption="Created by @DavidUbilava | Data: International Research Institute for Climate and Society (https://iri.columbia.edu/)")+
>>>>>>> d4418d94e9172f1d71a7a1c83782c140582ddeb2
  theme_guess()

gg_plume15 <- ggdraw(gg_plume15) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/enso_plume15.png",gg_plume15,width=6.5,height=3.75)



gg_plume09 <- ggplot(plume_lg[Type!="A" & Year==2009],aes(x=Season,y=Forecast,group=Model))+
  geom_line(color="darkgray",linewidth=.8,na.rm=T)+
  geom_line(data=plume_lg[Model=="Average" & Year==2009],color="black",linewidth=.8,linetype=5)+
  geom_line(data=plume_lg[Model=="Actual" & Year==2009],color="coral",linewidth=.8,linetype=1)+
  # coord_cartesian(ylim=c(-2,2))+
  labs(title="2009 ENSO Forecasts",x="Season",y="\u00B0C",caption="Created by @DavidUbilava | Data: International Research Institute for Climate and Society (https://iri.columbia.edu/)")+
  theme_guess()

gg_plume09 <- ggdraw(gg_plume09) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/enso_plume09.png",gg_plume09,width=6.5,height=3.75)





## commodities
prices_dt <- fread("data/CMO-Historical-Data-Monthly.csv")

## enso
enso_dt <- fread("https://www.cpc.ncep.noaa.gov/data/indices/ersst5.nino.mth.91-20.ascii")

colnames(enso_dt)[c(4,6,8,10)] <- c("ANOM1+2","ANOM3","ANOM4","ANOM3.4")

# adjust dates
prices_dt[,`:=`(Date=as.Date(paste0(substr(Date,1,4),"-",substr(Date,6,7),"-01")))]

enso_dt[,`:=`(Date=as.Date(paste0(YR,"-",str_pad(MON,2,pad="0"),"-01")))]

dt <- merge(enso_dt[,.(Date,ENSO=ANOM3.4)],prices_dt,all.y=T)

# j=j+1
# a <- c(1,2,j)
# sub_dt <- dt[,..a]
# lab <- colnames(sub_dt)[3]
# colnames(sub_dt) <- c("Date","ENSO","Price")
# # sub_lg <- melt(sub_dt,id.vars="Date",variable.name="Series",value.name="Values")
# 
# ggplot(sub_dt[Date>="1980-01-01"],aes(x=Date)) +
#   geom_line(aes(y = ENSO),colour="coral") +
#   geom_line(aes(y = Price / mean(sub_dt$Price,na.rm=T)), colour="steelblue") +
#   labs(title=lab)+
#   scale_y_continuous(sec.axis = ~.*mean(sub_dt$Price,na.rm=T))+
#   theme(axis.text.y.left = element_text(color = "coral"),
#         axis.text.y.right = element_text(color = "steelblue"))


# ENSO years
years <- c(1982,1987,1992,1997,2009,2015)


events <- paste0(years,"-12-01")

prices_ls <- list()

for(i in 1:length(events)){
  prices_ls[[i]] <- dt[Date>=paste0(years[i],"-01-01") & Date<=paste0(years[i]+2,"-12-01"),.(Year=years[i],Period=-11:24,PALM_OIL,COCONUT_OIL,SOYBEAN_OIL,SUGAR_WLD)]
}

combined_dt <- Reduce(rbind,prices_ls)

ggplot(combined_dt,aes(x=Period,y=PALM_OIL,group=Year))+
  geom_line(color=ifelse(combined_dt$Year%in%c(1982,1997,2015),"indianred","darkgray"))+
  geom_text_repel(data=combined_dt[Period==12],aes(label=Year),color=ifelse(combined_dt[Period==24]$Year%in%c(1982,1997,2015),"indianred","darkgray"))

# centroids (2023) ----

centroid <- function(x,y){
  pt <- st_multipoint(cbind(x,y))
  ct <- st_centroid(pt)
  ct_dt <- data.table(st_coordinates(ct))
  return(ct_dt)
}

dt[,c("Longitude_c","Latitude_c"):=centroid(Longitude,Latitude),by=.(Journal,Year)]
dt <- dt[order(Journal,Year,LastName,FirstName)]

dt_array <- Reduce(rbind,dt_list)
dt_array <- dt_array[,.(N=sum(N)),by=.(X,Y)]

ct_array <- Reduce(rbind,ct_list)

locations_dt <- dt[,.(.N),by=.(Longitude,Latitude,Year)]
centroids_dt <- unique(dt[,.(Longitude_c,Latitude_c,Journal,Year)])


crs <- st_crs("+proj=longlat +datum=WGS84")
world_crs <- st_set_crs(world,crs)
lakes_crs <- st_set_crs(lakes,crs)

gg_map <- ggplot()+
  geom_sf(data=world_crs,fill="white",color="slategray",linewidth=.2)+
  geom_sf(data=lakes_crs,fill="lightcyan",color="slategray",linewidth=.1)+
  geom_point(data=locations_dt[Year==2020],aes(x=Longitude,y=Latitude,size=factor(N,ordered=T)),shape=21,color="indianred",fill="indianred",alpha=.6)+
  geom_point(data=centroids_dt[Year==2020],aes(x=Longitude_c,y=Latitude_c),shape=21,size=2,color="black",fill="goldenrod")+
  geom_text_repel(data=centroids_dt[Year==2020],aes(x=Longitude_c,y=Latitude_c,label=Journal),family="sans",color="slategray",nudge_x=c(0,0,.5,-.5,0),nudge_y=c(-.5,.75,0,0,-.5),seed=1)+
  labs(title="Top-5 Economics Journal Editor Locations and Journal Centroids",x="",y="",caption="Created by @DavidUbilava")+
  coord_sf(xlim=c(-128,22),ylim=c(18,66),expand=FALSE)+
  theme_guess()+
  theme(legend.position="top",panel.background=element_rect(fill="lightcyan"))+ 
  guides(size=guide_legend(nrow=1))

gg_map <- ggdraw(gg_map) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("figures/geolocations.png",gg_map,width=6.5,height=3.75)




# gender ----

jlist <- unique(dt$Journal)
ylist <- unique(dt$Year)
glist <- unique(dt$Gender)

d <- CJ(jlist,ylist,glist)
colnames(d) <- c("Journal","Year","Gender")

gender_dt <- dt[,.(.N),by=.(Journal,Year,Gender)]
gender_dt <- gender_dt[order(Journal,Gender,Year)]
gender_dt <- merge(d,gender_dt,all.x=T)
gender_dt[is.na(gender_dt), ] <- 0 
gender_dt[,`:=`(tot=sum(N)),by=.(Journal,Year)]
gender_dt[,`:=`(Share_Women=N/tot)]

gg_gender <- ggplot(gender_dt[Gender=="female"],aes(x=Year,y=Share_Women,color=Journal,linetype=Journal,shape=Journal))+
  geom_line(linewidth=1)+
  geom_point(size=3)+
  coord_cartesian(ylim=c(0,1),xlim=c(2020,2023))+
  labs(title="Proportion of Women Editors at Top-5 Economics Journals",x="",y="",caption="Created by @DavidUbilava")+
  theme_guess()+
  theme(legend.position = "top")

gg_gender <- ggdraw(gg_gender) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

gg_gender

ggsave("figures/prop_women.png",gg_gender,width=6.5,height=3.75)






uni_dt <- dt[Year==2023,.(.N),by=Affiliation]
uni_dt <- uni_dt[order(-N)]
uni_dt

women_dt <- dt[Year==2023,.(.N),by=Gender]

country_dt <- dt[Year==2023,.(.N),by=Country]
country_dt <- country_dt[order(-N)]
country_dt

# obtain distance (degrees)
distance_d <- st_distance(dt_sf,ct_sf)

# obtain distance (metres)
dt_transformed <- st_transform(dt_sf_crs,"+proj=longlat +datum=WGS84 +units=m")
ct_transformed <- st_transform(ct_sf_crs,"+proj=longlat +datum=WGS84 +units=m")

distance_m <- st_distance(dt_transformed,ct_transformed)

#standard distance
sdist_d <- mean(as.numeric(distance_d))
sdist_m <- mean(as.numeric(distance_m))
                
                

mat <- matrix(nrow=length(jlist),ncol=length(ylist))

for(i in 1:length(jlist)){
  for(j in 1:length(ylist)){
    
    sub_dt <- dt[Journal==jlist[i] & Year==ylist[j]]
    
    # convert the data.table object to a sf object
    dt_sf <- st_as_sf(sub_dt,coords=c("Longitude","Latitude"))
    
    # save coordinates as a multipoint object -- so that weighting works
    pt_sf <- st_multipoint(as.matrix(sub_dt[,.(Longitude,Latitude)]))
    
    # get the centroid of the multipoint object
    ct_sf <- st_centroid(pt_sf)
    
    # save the centroid as a data.table object than as a sf object
    ct_dt <- data.table(st_coordinates(ct_sf))
    colnames(ct_dt) <- c("longitude","latitude")
    
    ct_sf <- st_as_sf(ct_dt,coords=c("longitude","latitude"))
    
    # add a crs to the objects
    crs <- st_crs("+proj=longlat +datum=WGS84")
    
    dt_sf_crs <- st_set_crs(dt_sf,crs)
    ct_sf_crs <- st_set_crs(ct_sf,crs)
    
    # obtain distance (degrees)
    distance_d <- st_distance(dt_sf,ct_sf)
    
    # obtain distance (metres)
    dt_transformed <- st_transform(dt_sf_crs,"+proj=longlat +datum=WGS84 +units=m")
    ct_transformed <- st_transform(ct_sf_crs,"+proj=longlat +datum=WGS84 +units=m")
    
    distance_m <- st_distance(dt_transformed,ct_transformed)
    
    #standard distance
    sdist_d <- mean(as.numeric(distance_d))
    sdist_m <- mean(as.numeric(distance_m))
    
    mat[i,j] <- sdist_d
    
  }
}

rownames(mat) <- jlist
colnames(mat) <- ylist

mat_dt <- data.table(mat)
mat_dt$Journal <- jlist
mat_lg <- melt(mat_dt,id.vars="Journal")
colnames(mat_lg) <- c("Journal","Year","Distance")
mat_lg$Year <- as.numeric(as.character(mat_lg$Year))

gg_geography <- ggplot(mat_lg,aes(x=Year,y=Distance,color=Journal,linetype=Journal,shape=Journal,group=Journal))+
  geom_line(linewidth=1)+
  geom_point(size=3)+
  coord_cartesian(ylim=c(0,40),xlim=c(2020,2023))+
  labs(title="Standard Distance of Editors at Top-5 Economics Journals",x="",y="")+
  theme_guess()+
  theme(legend.position = "top",legend.text = element_text(size=28),axis.text=element_text(size=32))

ggsave("figures/std_distance.png",gg_geography,width=6.5,height=3.75)



# print the distances
distance_d
distance_m


world_crs <- st_set_crs(world,crs)

dt_locations <- data.table(st_coordinates(dt_sf_crs))
dt_locations <- dt_locations[,.(.N),by=.(X,Y)]

ct_location <- data.table(st_coordinates(ct_sf_crs))

ggplot(world)+
  geom_sf(fill=NA,color="slategray")+
  geom_point(data=dt_locations,aes(x=X,y=Y),shape=21,size=2,color="darkgray",fill="indianred")+
  geom_point(data=ct_location,aes(x=X,y=Y),shape=21,size=3,color="darkgray",fill="goldenrod")+
  coord_sf(xlim=c(-130,35),ylim=c(20,65),expand=FALSE)+
  theme_guess()




# standardized distance
sdist_d <- mean(as.numeric(distance_d))
sdist_m <- mean(as.numeric(distance_m))



gg_schools <- ggplot(almamater_dt[N>=4],aes(x=PhD,y=N))+
  geom_bar(stat="identity",fill="coral")+
  geom_text(aes(y=location,label=PhD_label,color=labelcol,family="mono"),size=10,hjust=0,vjust=0.5,angle=90)+
  scale_color_manual(values=c("coral","white"))+
  labs(title="Top 10 PhD granting universities for the Nobel Prize laureates in Economics",x="",y="",caption = "Created by @DavidUbilava\nData from Wikipedia and other online sources")+
  theme_guess()+
  theme(axis.line=element_blank(),axis.text.x=element_blank())

# add logo
gg <- ggdraw(gg) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

gg

ggsave("figures/top5.png",gg,width=6.5,height=3.65,dpi="retina",device="png")
ggsave("figures/top5.eps",gg,width=6.5,height=3.65,dpi="retina",device=cairo_ps)



