# load packages
library(data.table)
library(ggplot2)

# generate data (point estimates and standard errors)

# do for a single group first

set.seed(1)
est <- c(rep(0,5),1:5)+rnorm(10,0,.2)

set.seed(2)
se <- rep(1,10)+rnorm(10,0,.1)

dt1 <- data.table(x=factor(c(-5:-2,0:5)),est,se)

gg1 <- ggplot(dt1,aes(x=x,y=est))+
  geom_hline(yintercept = 0,color="dimgray",linetype=5)+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),width=.2)+
  geom_point(size=2)+
  labs(x="prediod relative to the event",y="effect")+
  theme_classic()

gg1

# save the plot as a png file and as an eps file
ggsave("event1.png",gg1,width=6.5,height=3.5,dpi="retina",device="png")
ggsave("event1.eps",gg1,width=6.5,height=3.5,dpi="retina",device="eps")

# do for two groups (e.g., treatment and control)

dt1[,`:=`(grp="treatment")]

set.seed(3)
est <- rep(0,10)+rnorm(10,0,.3)

set.seed(4)
se <- rep(1,10)+rnorm(10,0,.2)

dt2 <- data.table(x=factor(c(-5:-2,0:5)),est,se,grp="control")

dt <- rbind(dt1,dt2)
dt$grp <- factor(dt$grp,levels=c("treatment","control")) # this is so that treatment group shows up first, control group next (for the legend on the graph)

gg2 <- ggplot(dt,aes(x=x,y=est,color=grp))+
  geom_hline(yintercept = 0,color="dimgray",linetype=5)+
  geom_errorbar(aes(ymin=est-1.96*se,ymax=est+1.96*se),position= position_dodge(width=0.3),width=.2)+
  geom_point(size=2,position= position_dodge(width=0.3))+
  scale_color_manual(values=c("black","gray"))+
  labs(x="prediod relative to the event",y="effect")+
  theme_classic()+
  theme(legend.position = c(.1,.9),legend.title=element_blank(),legend.text=element_text(size=10))

gg2

# save the plot as a png file and as an eps file
ggsave("event2.png",gg2,width=6.5,height=3.5,dpi="retina",device="png")
ggsave("event2.eps",gg2,width=6.5,height=3.5,dpi="retina",device="eps")
