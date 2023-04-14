# load packages
library(data.table)
library(ggplot2)
library(Cairo)

n=100
set.seed(2)
y <- rnorm(n)
for(i in 2:n){
  if(abs(y[i-1])<1.5){
    y[i] <- y[i-1]+y[i]
  }else{
    y[i] <- .5*y[i-1]+y[i]
  }
}

dt <- data.table(x=1:n,y=y)

ggplot(dt,aes(x=x,y=y))+
  geom_line()




n=100
set.seed(2)
y <- rnorm(n)
for(i in 2:n){
  if(i %in% c(20:40,55:85)){
    if(abs(y[i-1])<2){
      y[i] <- y[i-1]+y[i]
    }else{
      y[i] <- .5*y[i-1]+y[i]
    }
  }else{
    if(abs(y[i-1])<1){
      y[i] <- y[i-1]+y[i]
    }else{
      y[i] <- .5*y[i-1]+y[i]
    }
  }
  
}

dt <- data.table(x=1:n,y=y,d=c(rep(0,20),rep(1,20),rep(0,15),rep(1,30),rep(0,15)))

segment_dt = data.table(
  x = c(0,20,40,55,85,0,20,40,55,85),
  xend = c(20,40,55,85,100,20,40,55,85,100), 
  y = c(1,2,1,2,1,-1,-2,-1,-2,-1),
  yend = c(1,2,1,2,1,-1,-2,-1,-2,-1)
)

gg1 <- ggplot(dt,aes(x=x,y=y))+
  annotate("rect",xmin=c(20,55),xmax=c(40,85),ymin=-Inf,ymax=Inf,fill="lightgray",alpha=.5)+
  geom_segment(data=segment_dt,aes(x=x,y=y,xend=xend,yend=yend),linetype=5,linewidth=.6,color="dimgray")+
  geom_line(linewidth=.8)+
  annotate("text",x=c(30,70),y=c(3.5,3.5),label="conflict",hjust=.5,vjust=.5)+
  labs(x="t",y=expression(p["i,t"]-p["j,t"]))+
  theme_classic()

gg1

ggsave("spatial.png",gg1,width=6.5,height=3.0,dpi="retina",device="png")






n=12
set.seed(6)
y <- rnorm(n,0,.1)
y1 <- y
y2 <- y
y1[1] <- 0
y2[1] <- 0
for(i in 2:n){
  y1[i] <- -.1*i+.02*i^2-0.0005*i^3+y1[i]
  y2[i] <- -.3*i+.05*i^2-0.001*i^3+y2[i]
}

dt <- data.table(x=factor(1:n),y1=y1*30,y2=y2*30)

dl <- melt(dt,id.vars="x")
dl$variable <- factor(dl$variable,levels=c("y1","y2"),labels=c("with storage","without storage"))

# segment_dt = data.table(
#   x = c(0,20,40,55,85,0,20,40,55,85),
#   xend = c(20,40,55,85,100,20,40,55,85,100), 
#   y = c(1,2,1,2,1,-1,-2,-1,-2,-1),
#   yend = c(1,2,1,2,1,-1,-2,-1,-2,-1)
# )

gg2 <- ggplot(dl,aes(x=x,y=value,linetype=variable,group=variable))+
  annotate("rect",xmin=8,xmax=12,ymin=-Inf,ymax=Inf,fill="lightgray",alpha=.5)+
  # geom_segment(data=segment_dt,aes(x=x,y=y,xend=xend,yend=yend),linetype=5,linewidth=.6,color="dimgray")+
  geom_line(linewidth=.8)+
  annotate("text",x=c(1.5,5.0,10),y=c(15,15,-5),label=c("harvest","postharvest season","lean season"),hjust=.5,vjust=.5)+
  scale_x_discrete(labels=c(0:11))+
  # coord_cartesian(ylim=c(0,3))+
  labs(x="m",y=expression(paste(100,"%\u00D7",(p[m]-p[0])/p[0])))+
  theme_classic()+
  theme(legend.position=c(.15,.85),legend.title=element_blank(),legend.text = element_text(size=10))

gg2

ggsave("storage.png",gg2,width=6.5,height=3.0,dpi="retina",device="png")







# Create test data.
dt <- data.table(category=c("Harvest","Post\nharvest","Pre\nharvest"),count=c(15,60,25))

# Compute percentages
dt$fraction = dt$count/sum(dt$count)

# Compute the cumulative percentages (top of each rectangle)
dt$ymax = cumsum(dt$fraction)

# Compute the bottom of each rectangle
dt$ymin = c(0,head(dt$ymax,n=-1))

# Compute label position
dt$labelPosition <- (dt$ymax + dt$ymin) / 2

# Compute a good label
dt$label <- paste0(dt$category)

# Make the plot
ggplot(dt,aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3.5, fill=category)) +
  geom_rect() +
  geom_text(x=1.6,aes(y=labelPosition, label=label, color=category), size=6) +
  scale_fill_manual(values=c("goldenrod","coral","indianred")) +
  scale_color_manual(values=c("goldenrod","coral","indianred")) +
  coord_polar(theta="y",start=pi*1.9) +
  xlim(c(-1,4)) +
  theme_void()+
  theme(legend.position="none")





df <- data.frame(Temperature = c(4.4, 4.6, 6.3, 8.7, 11.6, 14.1, 15.9, 15.5,
                                 13.1, 9.7, 6.7, 4.3, 3.6, 3.9, 6.4, 9.7, 13.2,
                                 15.8, 18, 17.8, 15.1, 11.2, 7.2, 4.4),
                 City = rep(c("Glasgow", "Amsterdam"), each = 12),
                 Month = factor(rep(month.name, 2), month.name))

ggplot(dt, aes(x=category,y=fraction)) +
  geom_col() +
  # geom_vline(xintercept = 1:13 - 0.5, color = "gray90") +
  # geom_hline(yintercept = 0:3 * 5, color = "gray90") +
  scale_fill_manual(values = c("darkorange", "dodgerblue4")) +
  coord_curvedpolar(theta="x",start=pi*1.9)+
  # theme_void() +
  theme(legend.position="none")







# generate the radial plot
gg_coef <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="coral",alpha=.25)+
  geom_line(color="coral",linewidth=.6)+
  geom_hline(yintercept = seq(-12,10,4),color="gray60",size=.3,linetype=3) +
  geom_hline(yintercept = 0,color="gray40",size=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,10,4))+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence",caption="Created by @DavidUbilava using ACLED data and based on Ubilava et al. (2023)")+
  theme_classic()+
  theme_guess()+
  theme(axis.line=element_blank())

ggsave("figures/radial_violence.png",gg_coef,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/radial_violence.eps",gg_coef,width=6.5,height=6.5,dpi="retina",device=cairo_ps)
