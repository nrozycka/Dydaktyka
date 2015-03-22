library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)


dane2 <-read.table(file="C:/Users/Aneta/Desktop/R/Danebudynki.txt",
                   header=T,
                   sep='\t',
                   dec=',',
                   stringsAsFactors=F)


dane2_liniowy <-dane2 %>% gather(rok, y- Kategoria)
dane2_liniowy <- dane2 %>% gather(rok,y,r2002,r2011)

ggplot(data=dane2_liniowy,
       aes(x=rok,
           y=y,
           colour=Kategoria,
           group=Kategoria,
           label=Kategoria))+
  geom_point(size=5)+
  geom_line()+
  theme_bw()+  
  xlab('Rok spisu')+
  ylab('Udzia³ (%)')+
  ggtitle('Udzia³...')+
  geom_text(aes(label=Kategoria), hjust=-0.2, vjust=0, size =3.5)+
  geom_text(aes(label=y), hjust=1.5, vjust=-1, size =3.5)