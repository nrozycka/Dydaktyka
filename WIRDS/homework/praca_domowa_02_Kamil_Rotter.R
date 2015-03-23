#### wczytuje pakiety
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)

library(XLConnect)
fName <- "C:/Users/Super Kamil 3000/Desktop/R_workspace/WIRDS/zad3.xlsx"
wb <- loadWorkbook(fName)
getSheets(wb)
dane <- readWorksheet(wb, sheet = "Arkusz1")

dane$r2002 <- dane$X2002
dane$r2011 <- dane$X2011

dane_liniowy <- dane %>% gather(rok,y,-Kategoria)
dane_liniowy <- dane %>% gather(rok,y,r2002,r2011)

ggplot(data = dane_liniowy,
       aes( x = rok,
            y = y,
            colour = Kategoria,
            group = Kategoria)) +
  geom_point(size=5) +
  geom_line() +
  theme_bw() +
  xlab('Rok spisu') +
  ylab('Udzia³ (%)') +
  ggtitle('Udzia³ ...') +
  geom_text(aes(label=Kategoria), hjust=0.5, vjust=-1, size = 4) +
  geom_text(aes(label=y), hjust=1.5, vjust=0.5, size = 3)
