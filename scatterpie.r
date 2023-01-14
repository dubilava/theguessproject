# load packages
library(data.table)
library(ggplot2)
library(scatterpie)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")

# plot aesthetics
theme_guess <- function(){
  theme(
    panel.background=element_rect(fill="transparent",color=NA),
    plot.background=element_rect(fill="transparent",color=NA),
    legend.background=element_rect(fill="transparent",color=NA),
    plot.title=element_text(size=12,colour="dimgray"),
    axis.title=element_text(size=12,colour="dimgray"),
    axis.text=element_text(size=10,colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line=element_line(colour="darkgray"),
    axis.ticks=element_line(colour="darkgray"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="dimgray"),
    legend.key.size=unit(.75,'lines'),
    plot.caption = element_text(colour="slategray"),
    strip.background=element_blank(),
    strip.text=element_text(size=10,colour="dimgray",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}

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
  geom_sf(color="darkgray",fill=NA,size=.25)+
  geom_scatterpie(data=datasum_dt,aes(x=longitude,y=latitude,r=conflict_radius),cols=c("Rebel groups","State forces","Identity militias","Political militias"),color=NA)+
  scale_fill_manual(values=c("indianred","goldenrod","forestgreen","steelblue"))+
  scale_size(range=c(.2,2.6),name="Incidents")+
  coord_sf(xlim=c(-16,51),ylim=c(-34,36))+
  labs(caption="Created by @DavidUbilava using ACLED data")+
  theme_void()+
  theme_guess()+
  theme(axis.line=element_blank(),axis.title=element_blank(),axis.text=element_blank(),legend.position=c(.88,.92),legend.text=element_text(hjust=0,size=8,colour="darkgray"))


ggsave("figures/conflict_africa.png",gg_map,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/conflict_africa.eps",gg_map,width=6.5,height=6.5,dpi="retina",device="eps")
