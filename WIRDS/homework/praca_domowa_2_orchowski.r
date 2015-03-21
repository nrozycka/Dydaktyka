library(XLConnect)
library(dplyr)
library(tidyr)
library(ggplot2)


wb <- loadWorkbook('./Dane_zaj3_wlasnosc.xlsx')
dane <- readWorksheet(wb,1)

dane_liniowy <- dane %>% gather(rok, y, -Kategoria)

ggplot(data = dane_liniowy,
       aes(x = rok,
           y = y,
           colour = Kategoria,
           group = Kategoria))+
  geom_point(size = 5) +
  geom_line() +
  theme_bw() + 
  xlab('Rok spisu')+
  ylab('Udział‚ (%)') +
  ggtitle('Udział poszczególnych kategorii własności u respondentów') +
  geom_text(aes(label=Kategoria), hjust=0, vjust=2, size = 3) +
  geom_text(aes(label=y), hjust=2, vjust=1, size =3)