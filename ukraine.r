# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(Cairo)
library(sf)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)

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

# load data
load("data/ukraine.RData")

# wheat futures ----

gg_wheat <- ggplot(wheat_sub,aes(x=Date,y=Close))+
  geom_line(linewidth=.8,color="coral")+
  geom_segment(aes(x=Event_Date,xend=Event_Date,y=Timeline,yend=0),linewidth=0.8,col="black",na.rm=T)+
  geom_point(aes(x=Event_Date,y=Timeline),shape=21,size=3,stroke=1,col="black",fill="darkgray",na.rm=T)+
  geom_text(aes(x=Event_Date,y=Timeline,label=Event),nudge_y=35,na.rm=T)+
  geom_text(aes(x=Event_Date,y=Timeline,label=Date),size=3,nudge_x=30,na.rm=T)+
  coord_cartesian(ylim=c(500,1300),xlim=c(as.Date("2022-01-01"),as.Date("2023-01-15")))+
  labs(y="Wheat Futures Price (cents/bu)",caption="Created by @DavidUbilava using data from https://data.nasdaq.com/")+
  theme_classic()+
  theme_guess()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))

ggsave("figures/wheat.png",gg_wheat,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/wheat.eps",gg_wheat,width=6.5,height=4.5,dpi="retina",device="eps")


# wheat production & export ----

max_val <- sum(prodexp_sub[Year==2021]$Production)
min_inc <- pretty(0:max_val,n=10)[2]

gg_prod <- ggplot(prodexp_sub,aes(x=Year,y=Production,fill=Country,group=Country)) + 
  geom_area(color="white",size=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("dimgray",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y="Production (million mt)",caption="Created by @DavidUbilava using data from USDA/FAS PSD Online\nhttps://apps.fas.usda.gov/psdonline/app/index.html")+
  coord_cartesian(ylim=c(0,840))+
  theme_classic()+
  theme_guess()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=16),axis.text = element_text(size=14),legend.text = element_text(size=14),legend.key.size = unit(0.8,"cm"))

ggsave("figures/production.png",gg_prod,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/production.eps",gg_prod,width=6.5,height=4.5,dpi="retina",device="eps")


max_val <- sum(prodexp_sub[Year==2021]$Exports)
min_inc <- pretty(0:max_val,n=10)[2]

gg_expr <- ggplot(prodexp_sub,aes(x=Year,y=Exports,fill=Country,group=Country)) + 
  geom_area(color="white",size=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("dimgray",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y="Exports (million mt)",caption="Created by @DavidUbilava using data from USDA/FAS PSD Online\nhttps://apps.fas.usda.gov/psdonline/app/index.html")+
  coord_cartesian(ylim=c(0,210))+
  theme_classic()+
  theme_guess()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=16),axis.text = element_text(size=14),legend.text = element_text(size=14),legend.key.size = unit(0.8,"cm"))

ggsave("figures/exports.png",gg_expr,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("figures/exports.eps",gg_expr,width=6.5,height=4.5,dpi="retina",device="eps")


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
  labs(caption="Created by @DavidUbilava using data from https://acleddata.com/")+
  theme_void()+
  theme_guess()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=.5,size=16,colour="gray35",face="bold"),legend.title=element_text(hjust=.5,size=14),legend.text = element_text(hjust=1,size=14),legend.position = c(.17,.27),legend.key.height=unit(.75,'cm'),legend.key.width=unit(.5,'cm'),legend.direction = "vertical", legend.box = "horizontal")

ggsave("figures/conflict2022.png",gg_ch,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/conflict2022.eps",gg_ch,width=6.5,height=6.5,dpi="retina",device=cairo_ps)
