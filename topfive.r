# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(Cairo)
library(cowplot)
library(magick)
library(sf)
library(showtext)
font_add_google(name="Syne Mono")
showtext_auto()
library("rnaturalearth")
library("rnaturalearthdata")
# devtools::install_github("ropensci/rnaturalearthhires")
library(gender)


world <- ne_countries(scale="medium",returnclass="sf")

# for transformations
world_crs <- '+proj=robin'

usa_crs <- '+proj=aeqd +lat_0=45 +lon_0=-105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

eu_crs <- '+proj=aeqd +lat_0=54 +lon_0=10 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

atl_crs <- '+proj=aeqd +lat_0=50 +lon_0=-70 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

world_tr <- st_transform(world,crs=world_crs)

# plot aesthetics
theme_guess <- function(
    base_size=12,
    base_family = "Syne Mono",
    title_family = "Syne Mono",
    border=FALSE
){
  theme_foundation(base_size=base_size,base_family=base_family) +
    theme(
      line = element_line(linetype=1,colour="darkgray"),
      rect = element_rect(linetype=0,colour=NA),
      text = element_text(colour="darkgray"),
      title = element_text(family=title_family,size=rel(2.2)),
      panel.background=element_rect(fill="transparent",color=NA),
      panel.grid = element_line(colour=NULL,linetype=3,linewidth=.4),
      panel.grid.major = element_line(colour = "darkgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background=element_rect(fill="transparent",color=NA),
      plot.title=element_text(face="bold",colour="slategray",hjust=0,lineheight=.5),
      plot.caption = element_text(family="Syne Mono",colour="slategray",hjust=0,size=rel(1),lineheight=.5,margin=margin(t=1,r=1,b=1,l=1)),
      plot.margin=unit(c(.0,.0,0.25,2.0),"lines"),
      # axis.title = element_blank(),
      axis.text = element_text(face = "bold", size = rel(2),margin=margin(t=1,r=1,b=1,l=1)),
      axis.text.x = element_text(colour = NULL),
      axis.text.y = element_text(colour = NULL),
      axis.ticks = element_blank(),
      axis.line = element_line(),
      axis.line.y = element_blank(),
      legend.background=element_rect(fill="transparent",color=NA),
      legend.position="none",
      legend.title=element_blank(),
      legend.text=element_text(family="Syne Mono",size=12,colour="dimgray"),
      legend.key = element_rect(fill="transparent"),
      legend.key.size=unit(.75,'lines'),
      strip.background=element_blank(),
      strip.text=element_text(family="Syne Mono",size=12,colour="dimgray",face="bold",margin=margin(.1,0,.1,0,"cm"))
    )
}


# load the logo (for branding)
logo <- image_read("logo.png")

# load data
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

d <- CJ(c("AER","ECTA","JPE","QJE","REStud"),2020:2023,c("male","female"))
colnames(d) <- c("Journal","Year","Gender")

gender_dt <- dt[,.(.N),by=.(Journal,Year,Gender)]
gender_dt <- gender_dt[order(Journal,Gender,Year)]
gender_dt[,`:=`(tot=sum(N)),by=.(Journal,Year)]
gender_dt[,`:=`(Share_Women=N/tot)]

gender_dt <- merge(d,gender_dt,all=T)
gender_dt[is.na(gender_dt), ] <- 0 

gg_gender <- ggplot(gender_dt[Gender=="female"],aes(x=Year,y=Share_Women,color=Journal,linetype=Journal,shape=Journal))+
  geom_line(linewidth=1)+
  geom_point(size=3)+
  coord_cartesian(ylim=c(0,1),xlim=c(2020,2023))+
  labs(title="Proportion of Women Editors at Top-5 Economics Journals",x="",y="")+
  theme_guess()+
  theme(legend.position = "top",legend.text = element_text(size=28),axis.text=element_text(size=32))

ggsave("figures/prop_women.png",gg_gender,width=6.5,height=3.75)

# dt <- data.table(name=c("Arnold","Bob","Corey","David","Ethan","Fergus"),uni=c("MIT","Harvard","Harvard","Yale","MIT","Harvard"),longitude=c(-71.092003,-71.116943,-71.116943,-72.922585,-71.092003,-71.116943),latitude=c(42.360001,42.374443,42.374443,41.316307,42.360001,42.374443),journal=c("AER","AER","AER","AER","QJE","QJE"))

# dt <- data.table(name=c("Arnold","Bob","Corey","David","Ethan","Fergus","Garry"),uni=c("MIT","Harvard","Harvard","Yale","MIT","Harvard","Harvard"),longitude=c(-71.092003,-71.116943,-71.116943,-72.922585,-71.092003,-71.116943,-71.116943),latitude=c(42.360001,42.374443,42.374443,41.316307,42.360001,42.374443,42.374443),journal=c("AER","AER","AER","AER","QJE","QJE","QJE"))

dt[,`:=`(Longitude=round(Longitude,3),Latitude=round(Latitude,3))]

# yearsum_dt <- dt[,.(.N),by=.(Year,Affiliation,Longitude,Latitude)]
# yearsum_dt <- yearsum_dt[order(Year,-N,Affiliation)]
# 
# yearsum_dt[,`:=`(Total=sum(N)),by=Year]
# yearsum_dt[,`:=`(S=N/Total)]
# yearsum_dt[,`:=`(HHI=sum(S^2)),by=Year]

jlist <- unique(dt$Journal)
ylist <- unique(dt$Year)

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
  geom_text(aes(y=location,label=PhD_label,color=labelcol,family="Syne Mono"),size=10,hjust=0,vjust=0.5,angle=90)+
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



