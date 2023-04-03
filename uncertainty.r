# load the libraries (install them if needed)
library(data.table)
library(ggplot2)
library(ggridges)
library(Cairo)
library(cowplot)
library(magick)
library(readabs)
library(readrba)
library(tsibble)

# plot aesthetics
theme_guess <- function(){
  theme(
    panel.background=element_rect(fill="transparent",color=NA),
    plot.background=element_rect(fill="transparent",color=NA),
    plot.title=element_text(size=12,colour="dimgray"),
    plot.caption = element_text(colour="slategray"),
    # plot.margin=unit(c(.5,.5,1.0,.5),"lines"),
    axis.title=element_text(size=12,colour="dimgray"),
    axis.text=element_text(size=10,colour="dimgray",margin=margin(t=1,r=1,b=1,l=1)),
    axis.line=element_line(colour="darkgray"),
    axis.ticks=element_line(colour="darkgray"),
    legend.background=element_rect(fill="transparent",color=NA),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=10,colour="dimgray"),
    legend.key.size=unit(.75,'lines'),
    strip.background=element_blank(),
    strip.text=element_text(size=10,colour="dimgray",face="bold",margin=margin(.1,0,.1,0,"cm"))
  )
}

# load the logo (for branding)
logo <- image_read("logo.png")

# Estimate confidence intervals around RBA forecasts
# As per https://www.rba.gov.au/publications/rdp/2012/pdf/rdp2012-07.pdf


raw_forecasts <- read_forecasts()
raw_forecasts_dt <- data.table(raw_forecasts)
raw_forecasts_dt <- raw_forecasts_dt[series=="unemp_rate"]
raw_forecasts_dt[,`:=`(forecast_qtr=yearquarter(forecast_date),year_qtr=yearquarter(date))]
raw_forecasts_dt[,horizon:=year_qtr-forecast_qtr]

forecast_ur_dt <- raw_forecasts_dt[horizon>=0 & year(forecast_qtr) >= 1993,.(year_qtr,forecast_qtr,forecast_value=value,horizon)]

raw_actual_ur <- read_abs_series("A84423050A")
raw_actual_ur_dt <- data.table(raw_actual_ur)
raw_actual_ur_dt[,year_qtr:=yearquarter(date)]

actual_ur_dt <- raw_actual_ur_dt[year(year_qtr)>=1993,.(actual_value=mean(value)),by=year_qtr]


ur_errors_dt <- merge(forecast_ur_dt,actual_ur_dt,by="year_qtr")
ur_errors_dt <- ur_errors_dt[order(forecast_qtr,year_qtr)]
ur_errors_dt[,forecast_error:=actual_value-forecast_value]

ggplot(ur_errors_dt[horizon <= 8],aes(x=forecast_error,y=horizon,fill=factor(horizon),group=horizon)) +
  geom_density_ridges(quantile_lines=T,quantiles=0.5,    rel_min_height=0.005) +
  scale_y_continuous(breaks = 0:8) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  labs(y="Forecast horizon (quarters)",x="Forecast error (percentage points)")


ur_errors_dt[,abs_forecast_error := abs(forecast_error)]
ur_errors_dt[year(year_qtr) >= 1993,`:=`(ave_error = quantile(abs_forecast_error,probs=c(0.5, 0.7, 0.9))),by=horizon]

ur_errors_dt <- ur_errors_dt[year(year_qtr) >= 1993 & horizon <= 8,as.list(quantile(abs_forecast_error, c(0.5,0.7,0.9),na.rm=T)),by=horizon]

ur_errors_lg <- melt(ur_errors_dt,id.vars="horizon")

ggplot(ur_errors_lg,aes(x = horizon,
           y = value,
           fill = variable)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.1)) +
  labs(x = "Forecast horizon (quarters)",
       y = "Absolute forecast error (percentage points)")
