# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(Cairo)
library(cowplot)
library(magick)
library(sf)
# library(showtext)
# font_add_google(name="Syne Mono")
# showtext_auto()
# devtools::install_github("ropensci/rnaturalearth")
# devtools::install_github("ropensci/rnaturalearthdata")
# devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearth")
library("rnaturalearthdata")
library(gender)

world <- ne_countries(scale="medium",returnclass="sf")
lakes <- ne_download(scale="medium",type="lakes",category="physical")
lakes <- st_as_sf(lakes)

# plot aesthetics
theme_guess <- function(
    base_size=10,
    base_family = "sans",
    title_family = "sans",
    border=FALSE
){
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

# # load data
# dt <- fread("data/editors.csv")
# 
# unique_dt <- unique(dt[,.(FirstName)])
# 
# gender_dt <- data.table(gender(unique_dt$FirstName,method = "genderize"))
# 
# dt <- merge(dt,gender_dt[,.(FirstName=name,Gender=gender)],by="FirstName",all.x=T)
# 
# save(dt,file="data/editors.RData")

load("data/editors.RData")

dt[,`:=`(Longitude=round(Longitude,3),Latitude=round(Latitude,3))]



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


