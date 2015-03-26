setwd('D:/szkola/magisterka/drugi semestr/WiRDS')


library(XLConnect)
library(dplyr)
library(tidyr)
library(ggplot2)

wb <- loadWorkbook('dane.xlsx')
dane <- readWorksheet(wb,'Arkusz1')

dane <- tbl_df(dane)
dane_liniowy <- dane %>% gather(rok, y, r2002,r2011)



ggplot(data=dane_liniowy,
       aes(x=rok,
           y=y,
           colour=Kategoria,
           group=Kategoria))+
  geom_point(size=4)+
  geom_line()+
  theme_bw()+
  xlab('Rok')+
  ylab('Udział procentowy')+
  ggtitle('Udział bla bla...')+
  geom_text(aes(label=Kategoria), vjust=-1.8, hjust=1.1, size=3)+  ### dodaje nazwy
  geom_text(aes(label=y), vjust=-0.8, hjust=1.5, size=3)           ### dodaje wartości