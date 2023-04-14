# load packages
library(data.table)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(magick)
library(scatterpie)
library(sf)
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
logo <- image_read("../logo.png")

# load the map of Africa
africa <- ne_countries(scale="medium",continent="africa",returnclass="sf")
africa <- st_set_crs(africa,"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# load the data
load("conflict_actors.RData")

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
  geom_sf(color="darkgray",fill=NA,linewidth=.25)+
  geom_scatterpie(data=datasum_dt,aes(x=longitude,y=latitude,r=conflict_radius),cols=c("Rebel groups","State forces","Identity militias","Political militias"),color=NA)+
  scale_fill_manual(values=c("indianred","goldenrod","forestgreen","steelblue"))+
  scale_size(range=c(.2,2.6),name="Incidents")+
  coord_sf(xlim=c(-16,51),ylim=c(-34,36))+
  labs(title="Political violence by conflict actor",x="",y="",caption="Created by @DavidUbilava | Data: ACLED (https://acleddata.com/)")+
  theme_guess()+
  theme(legend.position="top")

# add logo
gg_map <- ggdraw(gg_map) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

# save the graph
ggsave("conflict_africa.png",gg_map,width=6.5,height=7.15,dpi="retina",device="png")
ggsave("conflict_africa.eps",gg_map,width=6.5,height=7.15,dpi="retina",device=cairo_ps)
