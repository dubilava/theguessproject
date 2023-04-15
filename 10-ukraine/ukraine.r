# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(magick)
library(Cairo)
library(sf)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)

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
      plot.caption = element_text(family=base_family,size=rel(0.7),colour="slategray",hjust=0,margin=margin(t=5,r=1,b=1,l=1)),
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

# load data
load("ukraine.RData")

# wheat futures ----

gg_wheat <- ggplot(wheat_sub,aes(x=Date,y=Close))+
  geom_line(linewidth=.8,color="coral")+
  geom_segment(aes(x=Event_Date,xend=Event_Date,y=Timeline,yend=0),linewidth=0.8,col="black",na.rm=T)+
  geom_point(aes(x=Event_Date,y=Timeline),shape=21,size=3,stroke=1,col="black",fill="darkgray",na.rm=T)+
  geom_text(aes(x=Event_Date,y=Timeline,label=Event),nudge_y=35,na.rm=T)+
  geom_text(aes(x=Event_Date,y=Timeline,label=Date),size=3,nudge_x=30,na.rm=T)+
  coord_cartesian(ylim=c(500,1300),xlim=c(as.Date("2022-01-01"),as.Date("2023-01-15")))+
  labs(title="Wheat Futures and Geopolitical Events in the Black Sea Region",x="",y="Price (c/bu)",caption="Created by @DavidUbilava | Data: Nasdaq Data Link (https://data.nasdaq.com/)")+
  theme_guess()

# add logo
gg_wheat <- ggdraw(gg_wheat) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("wheat.png",gg_wheat,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("wheat.eps",gg_wheat,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


# wheat production & export ----

max_val <- sum(prodexp_sub[Year==2021]$Production)
min_inc <- pretty(0:max_val,n=10)[2]

gg_prod <- ggplot(prodexp_sub,aes(x=Year,y=Production,fill=Country,group=Country)) + 
  geom_area(color="white",linewidth=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("dimgray",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y="Production (million mt)",caption="Created by @DavidUbilava | Data: USDA/FAS PSD Online (https://apps.fas.usda.gov/psdonline/app/index.html)")+
  coord_cartesian(ylim=c(0,840))+
  theme_guess()+
  theme(legend.position = "right")

# add logo
gg_prod <- ggdraw(gg_prod) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("production.png",gg_prod,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("production.eps",gg_prod,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


max_val <- sum(prodexp_sub[Year==2021]$Exports)
min_inc <- pretty(0:max_val,n=10)[2]

gg_expr <- ggplot(prodexp_sub,aes(x=Year,y=Exports,fill=Country,group=Country)) + 
  geom_area(color="white",linewidth=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("dimgray",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y="Exports (million mt)",caption="Created by @DavidUbilava | Data: USDA/FAS PSD Online (https://apps.fas.usda.gov/psdonline/app/index.html)")+
  coord_cartesian(ylim=c(0,210))+
  theme_guess()+
  theme(legend.position = "right")

# add logo
gg_expr <- ggdraw(gg_expr) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("exports.png",gg_expr,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("exports.eps",gg_expr,width=6.5,height=4.5,dpi="retina",device=cairo_ps)


# Conflict ----

# organize the conflict data
aggregate_sub <- conflict_sub[,.(events=sum(events)),by=.(country,year)]

pre_dt <- aggregate_sub[year%in%c(2017:2019),.(events=mean(events)),by=.(country)]

pan_dt <- aggregate_sub[year%in%c(2020:2021),.(events=mean(events)),by=.(country)]

war_dt <- aggregate_sub[year%in%c(2022),.(events=mean(events)),by=.(country)]

combined_dt <- Reduce(function(...) merge(...,by=c("country"),all=T),list(pre_dt,pan_dt,war_dt))

colnames(combined_dt)[2:4] <- c("pre","pan","war")

combined_dt[is.na(combined_dt)] <- 0

## load the map of africa
africa <- ne_countries(scale="large",continent=c("africa"),returnclass="sf")
africa <- st_set_crs(africa, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

combined_dt$name_long <- combined_dt$country

combined_dt[country=="Democratic Republic of Congo"]$name_long <- "Democratic Republic of the Congo"
combined_dt[country=="Republic of Congo"]$name_long <- "Republic of the Congo"
combined_dt[country=="Ivory Coast"]$name_long <- "Côte d'Ivoire"

# add western sahara
ws <- combined_dt[1,]
ws$country <- "Western Sahara"
ws$name_long <- "Western Sahara"
ws[,c(2:4)] <- 0

combined_dt <- rbind(combined_dt,ws)

combined_dt[,`:=`(war_ch=(war-pan))]

# incorporate conflict stats into the africa sf object
africaplus <- merge(africa,combined_dt,by="name_long",all.x=T)

# organize events by lon-lat
africa2022_dt <- conflict_sub[year==2022,.(events=sum(events)),by=.(longitude,latitude)]

africa2022_dt[,`:=`(longitude=round(as.numeric(longitude),1),latitude=round(as.numeric(latitude),1))]

africa2022_dt <- africa2022_dt[,.(events=sum(events)),by=.(longitude,latitude)]

# plot the map
gg_ch <- ggplot(data = africaplus) +
  geom_sf(aes(fill=war_ch),color="dimgray",size=.2)+
  geom_point(data=africa2022_dt,aes(x=longitude,y=latitude,size=events,alpha=events),color="indianred")+
  coord_sf(xlim=c(-15,55),ylim=c(-35,37))+
  scale_fill_gradient2(low="powderblue",high="coral",midpoint=0,name="Change from\n2020-21 Avg")+
  scale_size_continuous(name="Incidents\nin 2022")+
  scale_alpha_continuous(name="Incidents\nin 2022")+
  labs(title="2022 Conflict and Change from 2020-2021 Average",x="",y="",caption="Created by @DavidUbilava | Data: ACLED (https://acleddata.com/)")+
  theme_guess()+
  theme(axis.line.x=element_blank(),legend.title=element_text(hjust=0,size=rel(1.1)),legend.text = element_text(hjust=1,size=rel(1)),legend.position = c(.22,.23),legend.key.height=unit(.75,'cm'),legend.key.width=unit(.5,'cm'),legend.direction = "vertical", legend.box = "horizontal")

# add logo
gg_ch <- ggdraw(gg_ch) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("conflict2022.png",gg_ch,width=6.5,height=6.65,dpi="retina",device="png")
ggsave("conflict2022.eps",gg_ch,width=6.5,height=6.65,dpi="retina",device=cairo_ps)
