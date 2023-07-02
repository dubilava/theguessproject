# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(scales)
library(ggsankey)
library(Cairo)
library(cowplot)
library(magick)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(gender)

world <- ne_countries(scale="medium",returnclass="sf")
lakes <- ne_download(scale="medium",type="lakes",category="physical")
lakes <- st_as_sf(lakes)

# plot aesthetics
theme_guess <- function(base_size=10,base_family="sans",title_family="sans",border=F){
  theme_foundation(base_size=base_size,base_family=base_family) +
    theme(
      line = element_line(linetype=1,colour="black"),
      rect = element_rect(linetype=0,colour=NA),
      text = element_text(colour="black"),
      panel.grid = element_line(colour=NULL,linetype=3),
      panel.grid.major = element_line(colour="darkgray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title=element_text(colour="black",hjust=0,size=rel(1.1)),
      plot.caption = element_text(family=base_family,size=rel(0.7),colour="slategray",hjust=0,margin=margin(t=5,r=1,b=1,l=1)),
      plot.margin=unit(c(0.25,0.25,0.25,1.25),"lines"),
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
logo <- image_read("../logo.png")

## this function makes sure that the centroid is obtained on
## locations and is not that of a polygon
centroid <- function(x,y){
  pt <- st_multipoint(cbind(x,y))
  ct <- st_centroid(pt)
  ct_dt <- data.table(st_coordinates(ct))
  return(ct_dt)
}

# load the data
load("editors.RData")

# round the coordinates to three decimal places, just in case
dt[,`:=`(Longitude=round(Longitude,3),Latitude=round(Latitude,3))]


# affiliation ranking (not using) ----

sub_dt <- dt[Year==2023]
sub_dt <- sub_dt[order(Journal,Affiliation)]

# degree-granting institutions (top-10 list)
affiliation_dt <- sub_dt[,.(.N),by=.(Affiliation)]
affiliation_dt <- affiliation_dt[order(-N,Affiliation)]
affiliation_dt$Affiliation <- factor(affiliation_dt$Affiliation,levels=affiliation_dt$Affiliation)

affiliation_dt[,`:=`(Affiliation_label=paste0(Affiliation," (",N,")"),location=ifelse(N>6,0.5,N+0.5),labelcol=ifelse(N>6,"white","coral"))]

gg_schools <- ggplot(affiliation_dt[N>=2],aes(x=Affiliation,y=N))+
  geom_bar(stat="identity",fill="coral")+
  geom_text(aes(y=location,label=Affiliation_label,color=labelcol),size=3,hjust=0,vjust=0.5,angle=90)+
  scale_color_manual(values=c("coral","white"))+
  scale_y_continuous(breaks=pretty_breaks(4))+
  labs(title="Affiliations of the top 5 economics journal editors in 2023",x="",y="",caption = "Created by @DavidUbilava | Data: The journal websites")+
  theme_guess()+
  theme(axis.line=element_blank(),axis.text.x=element_blank())

# add logo
gg_schools <- ggdraw(gg_schools) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("affiliations.png",gg_schools,width=6.5,height=3.75)


# affiliation to journal ----

sankey_dt <- data.table(make_long(sub_dt,Affiliation,Journal))

gg_sankey <- ggplot(sankey_dt,aes(x=x,next_x=next_x,node=node,next_node=next_node,fill=factor(node),label=node))+
  geom_sankey(flow.alpha=0.5,node.color=NA,show.legend=F,width=0)+
  geom_sankey_text(size=3,color="dimgray",hjust=c(rep(1,24),rep(0,5)))+
  scale_fill_viridis_d(option="H")+
  labs(title="Affiliations of the top 5 economics journal editors in 2023",x="",y="",caption = "Created by @DavidUbilava | Data: The journal websites")+
  coord_cartesian(clip='off',xlim=c(1.15,1.55))+
  theme_guess()+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),axis.line.x=element_blank(),plot.margin=unit(c(0.25,0.25,0.25,0.25),"lines"),plot.caption = element_text(hjust=.15))

# add logo
gg_sankey <- ggdraw(gg_sankey) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("sankey.png",gg_sankey,width=6.5,height=5.5)



# journals combined, map 2023 ----

dt[,c("Longitude_c","Latitude_c"):=centroid(Longitude,Latitude),by=.(Year)]
dt <- dt[order(Journal,Year,LastName,FirstName)]

locations_dt <- dt[,.(.N),by=.(Longitude,Latitude,Year)]
centroids_dt <- unique(dt[,.(Longitude_c,Latitude_c,Year)])

crs <- st_crs("+proj=longlat +datum=WGS84")
world_crs <- st_set_crs(world,crs)
lakes_crs <- st_set_crs(lakes,crs)

gg_map <- ggplot()+
  geom_sf(data=world_crs,fill="white",color="slategray",linewidth=.2)+
  geom_sf(data=lakes_crs,fill="lightcyan",color="slategray",linewidth=.1)+
  geom_point(data=locations_dt[Year==2023],aes(x=Longitude,y=Latitude,size=N),shape=21,color="coral",fill=NA,stroke=.8)+
  geom_point(data=centroids_dt[Year==2023],aes(x=Longitude_c,y=Latitude_c),shape=21,size=3,color="dimgray",fill="goldenrod",stroke=.6,alpha=.7)+
  scale_size_continuous(range=c(2,8),breaks=c(1,2,8,9))+
  labs(title="The top 5 economics journal editor locations in 2023",x="",y="",caption="Created by @DavidUbilava | Data: The journal websites")+
  coord_sf(xlim=c(-128,22),ylim=c(18,66),expand=FALSE)+
  theme_guess()+
  guides(size=guide_legend(order=1),shape=guide_legend(order=2))+
  theme(legend.position="top",panel.background=element_rect(fill="lightcyan"))+ 
  guides(size=guide_legend(nrow=1))

gg_map <- ggdraw(gg_map) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=.025,valign=0,clip="off")

ggsave("locations_combined.png",gg_map,width=6.5,height=3.75)



# journals, map 2023 ----

dt[,c("Longitude_c","Latitude_c"):=centroid(Longitude,Latitude),by=.(Journal,Year)]
dt <- dt[order(Journal,Year,LastName,FirstName)]

locations_dt <- dt[,.(.N),by=.(Longitude,Latitude,Year)]
centroids_dt <- unique(dt[,.(Longitude_c,Latitude_c,Journal,Year)])

crs <- st_crs("+proj=longlat +datum=WGS84")
world_crs <- st_set_crs(world,crs)
lakes_crs <- st_set_crs(lakes,crs)

gg_map <- ggplot()+
  geom_sf(data=world_crs,fill="white",color="slategray",linewidth=.2)+
  geom_sf(data=lakes_crs,fill="lightcyan",color="slategray",linewidth=.1)+
  geom_point(data=locations_dt[Year==2023],aes(x=Longitude,y=Latitude,size=N),shape=21,color="coral",fill=NA,stroke=.8)+
  geom_point(data=centroids_dt[Year==2023],aes(x=Longitude_c,y=Latitude_c),shape=21,size=3,color="dimgray",fill="goldenrod",stroke=.6,alpha=.7)+
  scale_size_continuous(range=c(2,8),breaks=c(1,2,8,9))+
  geom_text_repel(data=centroids_dt[Year==2023],aes(x=Longitude_c,y=Latitude_c,label=Journal),family="sans",color="dimgray",nudge_x=c(0,0,1,-1,0),nudge_y=c(-1,1,0,0,-1),seed=1,box.padding=.8,force=.2)+
  labs(title="The top 5 economics journal editor locations and journal centroids in 2023",x="",y="",caption="Created by @DavidUbilava | Data: The journal websites")+
  coord_sf(xlim=c(-128,22),ylim=c(18,66),expand=FALSE)+
  theme_guess()+
  theme(legend.position="top",panel.background=element_rect(fill="lightcyan"))+ 
  guides(size=guide_legend(nrow=1))

gg_map <- ggdraw(gg_map) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=.025,valign=0,clip="off")

ggsave("locations.png",gg_map,width=6.5,height=3.75)



# standard distances ----
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

mat <- round(mat,2)

mat_dt <- data.table(mat)
mat_dt$Journal <- jlist
mat_lg <- melt(mat_dt,id.vars="Journal")
colnames(mat_lg) <- c("Journal","Year","Distance")
mat_lg$Year <- as.numeric(as.character(mat_lg$Year))

gg_geography <- ggplot(mat_lg,aes(x=Year,y=Distance,color=Journal,linetype=Journal,shape=Journal,group=Journal))+
  geom_line(linewidth=.8)+
  geom_point(size=3,stroke=.8)+
  scale_shape_manual(values=c(0,17,6,1,18))+
  scale_linetype_manual(values=c(1,2,4,5,6))+
  scale_color_manual(values=c("indianred","dimgray","steelblue","goldenrod","seagreen"))+
  coord_cartesian(ylim=c(0,40),xlim=c(2020,2023))+
  labs(title="Standard Distance of the top 5 economics journal editors",x="Year",y="Degrees",caption="Created by @DavidUbilava | Data: The journal websites")+
  theme_guess()+
  theme(legend.position="top",legend.key.width=unit(.4,"in"))

gg_geography <- ggdraw(gg_geography) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("distance.png",gg_geography,width=6.5,height=3.75)



# gender ----
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
  geom_line(linewidth=.8)+
  geom_point(size=3,stroke=.8)+
  scale_shape_manual(values=c(0,17,6,1,18))+
  scale_linetype_manual(values=c(1,2,4,5,6))+
  scale_color_manual(values=c("indianred","dimgray","steelblue","goldenrod","seagreen"))+
  coord_cartesian(ylim=c(0,.6),xlim=c(2020,2023))+
  labs(title="Proportion of women among the top 5 economics journal editors",x="Year",y="Proportion",caption="Created by @DavidUbilava | Data: The journal websites")+
  theme_guess()+
  theme(legend.position="top",legend.key.width=unit(.4,"in"))

gg_gender <- ggdraw(gg_gender) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("women.png",gg_gender,width=6.5,height=3.75)



# some general stats

uni_dt <- dt[Year==2023,.(.N),by=Affiliation]
uni_dt <- uni_dt[order(-N)]
uni_dt

women_dt <- dt[Year==2023,.(.N),by=Gender]
women_dt <- women_dt[order(-N)]
women_dt

country_dt <- dt[Year==2023,.(.N),by=Country]
country_dt <- country_dt[order(-N)]
country_dt
