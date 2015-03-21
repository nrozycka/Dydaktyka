library(XLConnect)
library(dplyr)
library(tidyr)
library(ggplot2)

dane2<- read.table(file='D:/Studia/IVr 2 semestr/WiRDS/4-R-16.03-najem-kat nier/dane1.txt', 
                   header = T,
                   sep= '\t',
                   dec=',',
                   stringsAsFactors=F)

dane_liniowy <- dane2 %>% gather(rok,y,-Kategoria)


ggplot(data=dane_liniowy,
       aes(x=rok,
           y=y,
           colour=Kategoria,
           label=Kategoria,
           group=Kategoria))+#kolory uzale¿nione od kategorii
  geom_point(size=5)+
  geom_line()+ 
  theme_bw()+
  xlab('Rok spisu')+
  ylab('Udzia³ (%)')+
  geom_text(aes(y=y, 
                label=paste(Kategoria,"(",y, "%",")")),
            hjust=0.75, vjust=1.5,size=3)+
  ggtitle('Struktura gospodarstw domowych wg tytu³u 
    prawnego do zajmowanego mieszkania')