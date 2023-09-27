# load packages
library(data.table)
library(ggplot2)
library(cowplot)
library(ggthemes)
library(sf)
library(magick)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")

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

world <- ne_countries(returnclass="sf",scale="large")
africa <- ne_countries(returnclass="sf",scale="large",continent="Africa")
europe <- ne_countries(returnclass="sf",scale="large",continent="Europe")
asia <- ne_countries(returnclass="sf",scale="large",continent="Asia")

crs <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

sf_use_s2(FALSE)

world <- st_set_crs(world,crs)
africa <- st_set_crs(africa,crs)
europe <- st_set_crs(europe,crs)
asia <- st_set_crs(asia,crs)

lakes <- ne_download(returnclass="sf",scale="large",type="lakes",category="physical")
lakes <- st_set_crs(lakes,crs)
lakes <- st_make_valid(lakes)
lakes <- st_intersection(africa,lakes)

rivers <- ne_download(returnclass="sf",scale="large",type="rivers_lake_centerlines",category="physical")
rivers <- st_set_crs(rivers,crs)
rivers <- st_make_valid(rivers)
rivers <- st_intersection(africa,rivers)

ocean <- ne_download(returnclass="sf",scale="large",type="ocean",category="physical")
ocean <- st_set_crs(ocean,crs)


load("acled_africa_aggregate.RData")

dt <- dt[order(yearmo)]

sf_use_s2(TRUE)

# first just one figure, to illustrate the density map
msf <- st_as_sf(dt[yearmo==yrmo],coords=c("longitude","latitude"),remove=F)
msf <- st_set_crs(msf,crs)

gg_map <- ggplot(data=africa) +
  geom_density_2d_filled(data=dt[yearmo=="2023-08"],aes(x=longitude,y=latitude),bins=100)+
  xlim(-20,53)+
  ylim(-35,38)+
  geom_sf(color="gray",fill=NA,linewidth=.1)+
  geom_sf(data=lakes,fill="white",color="white",linewidth=.1)+
  geom_sf(data=rivers,fill="white",color="white",linewidth=.01)+
  geom_sf(data=ocean,fill="white",color="white",linewidth=.01)+
  geom_sf(data=europe,fill="white",color="white",linewidth=.01)+
  geom_sf(data=asia,fill="white",color="white",linewidth=.01)+
  geom_sf(data=msf,size=.2,color="white")+
  scale_fill_viridis_d(option="B")+
  coord_sf(xlim=c(-20,53),ylim=c(-35,38))+
  labs(title="Conflict in Africa: 2023 August",x="",y="",caption="Created by @DavidUbilava | Data: ACLED (https://acleddata.com/)")+
  theme_guess()+
  theme(legend.position="none",axis.title=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.line=element_blank(),plot.caption = element_text(hjust=1),plot.margin=unit(c(0.25,0.25,0.25,0.25),"lines"))

# add logo
gg_map <- ggdraw(gg_map) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

# save the graph
ggsave("density.png",gg_map,width=6.5,height=6.85,dpi="retina",device="png")


# now all figure, to generate density maps for gif
for(i in 1:length(unique(dt$yearmo))){
  
  yrmo <- unique(dt$yearmo)[i]
  
  msf <- st_as_sf(dt[yearmo==yrmo],coords=c("longitude","latitude"),remove=F)
  msf <- st_set_crs(msf,crs)
  
  gg <- ggplot(data=africa) +
    geom_density_2d_filled(data=dt[yearmo==yrmo],aes(x=longitude,y=latitude),bins=100)+
    xlim(-20,53)+
    ylim(-35,38)+
    geom_sf(color="gray",fill=NA,linewidth=.1)+
    geom_sf(data=lakes,fill="white",color="white",linewidth=.1)+
    geom_sf(data=rivers,fill="white",color="white",linewidth=.01)+
    geom_sf(data=ocean,fill="white",color="white",linewidth=.01)+
    geom_sf(data=europe,fill="white",color="white",linewidth=.01)+
    geom_sf(data=asia,fill="white",color="white",linewidth=.01)+
    geom_sf(data=msf,size=.2,color="white")+
    scale_fill_viridis_d(option="B")+
    annotate(geom="text",x=5,y=-10,label=yrmo,color="black",size=6,family="mono",fontface="bold",hjust=1,vjust=0)+
    coord_sf(xlim=c(-20,53),ylim=c(-35,38))+
    theme_void()+
    theme(legend.position="none")
  
  ggsave(paste0("temp/",yrmo,".png"),gg,width=5,height=5,dpi="screen")
  
}


## create a list of the png file names in the temp folder
the_list <- paste0("temp/",list.files("temp/"))

## store the graphs as a list in frames
frames <- lapply(the_list,image_read)

## generate a gif
animation <- image_animate(image_join(frames),fps=10,dispose="previous")

## save the gif
image_write(animation,"density.gif")

