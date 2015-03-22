library(XLConnect)
library(dplyr)
library(ggplot2)
library(tidyr)

dane <- read.table('D:/Dropbox/uczelnia/R/dane.txt', header = T, sep='\t', dec=',', stringsAsFactors = F )
dane_liniowy <- dane %>% gather(rok,y, -Kategoria)

ggplot(data=dane_liniowy,
       aes(x=rok,
           y=y,
           colour = Kategoria,
           group = Kategoria))+
  geom_point(size=3)+
  geom_line()+
  theme_bw()+
  xlab('Rok spisu')+
  ylab('Udzia³ (%)')+
  ggtitle('Udzia³...')+
  geom_text(aes(label = y), vjust = -0.5, hjust=1, size=4)+
  geom_text(aes(label=Kategoria), vjust=1.1, size=3)
  

