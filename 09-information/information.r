# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(magick)

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

# a function that simulates a time series with no structural change
sim <- function(m,n,sc=FALSE){
  
  # the size of the first window and of the out-of-sample segment
  R <- round(.5*n)
  P <- n-R
  
  set.seed(m)
  y <- rnorm(n)
  for(i in 2:n){
    if(sc==FALSE){
      y[i] <- .7*y[i-1]+y[i]
    }else{
      y[i] <- .5*(1/n)*i+(.9-.4*(1/n)*i)*y[i-1]+y[i]
    }
  }
  
  # store in a data.table
  dt <- data.table(date=1:n,y=y)
  
  # lagged dependent variable and two forecast windows
  dt[,`:=`(y1=shift(y,1),exp=as.numeric(NA),rol=as.numeric(NA))]
  
  for(i in 1:P){
    # regressions
    reg_exp <- lm(y~y1,data=dt[1:(R-1+i)])
    reg_rol <- lm(y~y1,data=dt[i:(R-1+i)])
    
    # forecasts
    dt$exp[R+i] <- reg_exp$coefficients["(Intercept)"]+reg_exp$coefficients["y1"]*dt$y1[R+i]
    dt$rol[R+i] <- reg_rol$coefficients["(Intercept)"]+reg_rol$coefficients["y1"]*dt$y1[R+i]
  }
  
  # forecast errors
  dt[,`:=`(exp_e=y-exp,rol_e=y-rol)]
  
  rmsfe_exp <- sqrt(mean(dt$exp_e^2,na.rm=T))
  rmsfe_rol <- sqrt(mean(dt$rol_e^2,na.rm=T))
  
  return(c(rmsfe_exp,rmsfe_rol))
  
}

# the length of the time series
n <- 480

# the number of iterations
M <- 500

# no structural change (will take a few minutes)
rmsfe1_dt <- data.table(t(sapply(1:M,sim,n=n,sc=F)))
colnames(rmsfe1_dt) <- c("exp","rol")

rmsfe1_dt[,`:=`(dif=log(exp/rol))]

# structural change (will take a few minutes)
rmsfe2_dt <- data.table(t(sapply(1:M,sim,n=n,sc=T)))
colnames(rmsfe2_dt) <- c("exp","rol")

rmsfe2_dt[,`:=`(dif=log(exp/rol))]

# a 'density' plot of relative RMSFE (no structural change)
gg_nc <- ggplot(rmsfe1_dt,aes(x=dif))+
  geom_dotplot(binwidth=.00055,dotsize=.6,color="coral",fill="coral",method="histodot")+
  labs(x="Relative Difference in RMSFE of Expanding vs Rolling Windows",y="",title="Constant Data-Generating Process",caption="Created by @DavidUbilava | Data: simulated")+
  theme_guess()

# add logo
gg_nc <- ggdraw(gg_nc) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("nochange.png",gg_nc,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("nochange.eps",gg_nc,width=6.5,height=4.5,dpi="retina",device=cairo_ps)

# a 'density' plot of relative RMSFE (structural change)
gg_sc <- ggplot(rmsfe2_dt,aes(x=dif))+
  geom_dotplot(binwidth=.00145,dotsize=.55,color="coral",fill="coral",method="histodot")+
  labs(x="Relative Difference in RMSFE of Expanding vs Rolling Windows",y="",title="Changing Data-Generating Process",caption="Created by @DavidUbilava | Data: simulated")+
  theme_guess()

# add logo
gg_sc <- ggdraw(gg_sc) +
  draw_image(logo,scale=.12,x=1,hjust=1,halign=0,valign=0,clip="off")

ggsave("change.png",gg_sc,width=6.5,height=4.5,dpi="retina",device="png")
ggsave("change.eps",gg_sc,width=6.5,height=4.5,dpi="retina",device=cairo_ps)
