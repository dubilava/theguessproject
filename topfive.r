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
      axis.text = element_text(face = "bold", size = rel(1.8),margin=margin(t=1,r=1,b=1,l=1)),
      axis.text.x = element_text(colour = NULL),
      axis.text.y = element_text(colour = NULL),
      axis.ticks = element_blank(),
      axis.line = element_line(),
      axis.line.y = element_blank(),
      legend.background=element_rect(fill="transparent",color=NA),
      legend.position="none",
      legend.title=element_blank(),
      legend.text=element_text(family="Syne Mono",size=12,colour="dimgray"),
      legend.key.size=unit(.75,'lines'),
      strip.background=element_blank(),
      strip.text=element_text(family="Syne Mono",size=12,colour="dimgray",face="bold",margin=margin(.1,0,.1,0,"cm"))
    )
}


# load the logo (for branding)
logo <- image_read("logo.png")

# load data
# dt <- data.table(name=c("Arnold","Bob","Corey","David","Ethan","Fergus"),uni=c("MIT","Harvard","Harvard","Yale","MIT","Harvard"),longitude=c(-71.092003,-71.116943,-71.116943,-72.922585,-71.092003,-71.116943),latitude=c(42.360001,42.374443,42.374443,41.316307,42.360001,42.374443),journal=c("AER","AER","AER","AER","QJE","QJE"))

dt <- data.table(name=c("Arnold","Bob","Corey","David","Ethan","Fergus","Garry"),uni=c("MIT","Harvard","Harvard","Yale","MIT","Harvard","Harvard"),longitude=c(-71.092003,-71.116943,-71.116943,-72.922585,-71.092003,-71.116943,-71.116943),latitude=c(42.360001,42.374443,42.374443,41.316307,42.360001,42.374443,42.374443),journal=c("AER","AER","AER","AER","QJE","QJE","QJE"))


# convert the data.table object to a sf object
dt_sf <- st_as_sf(dt,coords=c("longitude","latitude"))

# save coordinates as a multipoint object -- so that weighting works
pt_sf <- st_multipoint(as.matrix(dt[,.(longitude,latitude)]))

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

# print the distances
distance_d
distance_m


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



