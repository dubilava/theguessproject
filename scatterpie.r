# load packages
library(data.table)
library(ggplot2)
library(scatterpie)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")

# load the map of Africa
africa <- ne_countries(scale="large",continent="africa",returnclass="sf")
africa <- st_set_crs(africa, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# load the data
load("data/conflict_actors.RData")

# re-order by actor type
datasum_dt$actor <- factor(datasum_dt$actor,levels=c("rebel","state","ident","polit"))

# reshape to wide
datasum_dt <- dcast(datasum_dt,longitude+latitude~actor)
datasum_dt[is.na(datasum_dt)] <- 0

# create a radius variable
datasum_dt[,`:=`(conflict_radius=(log(state+rebel+polit+ident)+.1)*.075)]

# rename actors
colnames(datasum_dt)[3:6] <- c("Rebel groups","State forces","Identity militias","Political militias")

# generate the map
gg_map <- ggplot(data = africa) +
  geom_sf(color="gray",fill=NA,size=.25)+
  geom_scatterpie(data=datasum_dt,aes(x=longitude,y=latitude,r=conflict_radius),cols=c("Rebel groups","State forces","Identity militias","Political militias"),color=NA)+
  scale_fill_manual(values=c("indianred","goldenrod","forestgreen","steelblue"))+
  scale_size(range=c(.2,2.6),name="Incidents")+
  coord_sf(xlim=c(-16,51),ylim=c(-34,36))+
  theme_void()+
  theme(axis.line=element_blank(),axis.title=element_blank(),axis.text=element_blank(),legend.position=c(.88,.92),legend.text=element_text(hjust=0,size=8),legend.title = element_blank())

ggsave("figures/conflict_africa.png",gg_map,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/conflict_africa.eps",gg_map,width=6.5,height=6.5,dpi="retina",device="eps")
