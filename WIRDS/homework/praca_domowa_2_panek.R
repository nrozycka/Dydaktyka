library(XLConnect)
library(dplyr)
library(tidyr)
library(ggplot2)
dane2 <- read.table(file ='D:/UEP/Studia/IV rok/2 semestr/Wizualizacje/dane.txt',
                    header = T,
                    sep = '\t',
                    dec=',',
                    stringsAsFactors=F)

dane_liniowy <-dane2 %>% gather(rok, y, r2002, r2011)

ggplot(data=dane_liniowy,
       aes(x=rok,
           y=y,
           colour=Kategoria,
           group=Kategoria))+
  geom_point()+
  geom_line()+
  theme_bw()+ #tło czarnobiałe
  xlab('Rok spisu')+ #etykieta osi x
  ylab('Udział w (%)')+ #tykieta osi y
  ggtitle('Udział ludności według kategorii prawa do lokalu w zależności od roku spisu')+
  geom_text(aes(label=y), hjust=1.5, vjust=0, size=4)+
  geom_text(aes(label=Kategoria), hjust=0, vjust=1, size=4)