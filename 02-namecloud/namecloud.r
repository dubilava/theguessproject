# load packages
library(data.table)
library(ggplot2)
library(wordcloud2)
library(webshot2)
library(htmlwidgets)

# load the data
load("names.RData")

# tabulate the names of top5 published authors
authornames_dt <- as.data.table(table(namestop5_dt[name_length>1 | !is.na(name_length)]$firstname))
colnames(authornames_dt) <- c("Name","Count")
authornames_dt <- authornames_dt[order(-Count)]

# generate the wordcloud of names
author_wc <- wordcloud2(data=authornames_dt,color=ifelse(authornames_dt[, 2]>=c(authornames_dt[20,2]),'coral','darkgray'),backgroundColor="white",shuffle=F)

# save the wordcloud as an html object
saveWidget(author_wc,"author_wc.html",selfcontained=F)

# print the html object as the png file
webshot("author_wc.html","author_wc.png",delay=2,vwidth=1024,vheight=768)

# tabulate the names of those born between 1966-1975
babynames_dt <- ssa6675_dt

# generate the wordcloud of names
baby_wc <- wordcloud2(data=babynames_dt,color=ifelse(babynames_dt[, 2]>=c(babynames_dt[20,2]),'steelblue','darkgray'),backgroundColor="white",shuffle=F)

# save the wordcloud as an html object
saveWidget(baby_wc,"baby_wc.html",selfcontained=F)

# print the html object as the png file
webshot("baby_wc.html","baby_wc.png",delay=2,vwidth=1024,vheight=768)
