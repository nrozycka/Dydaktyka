library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
load('D:/Studia/IVr 2 semestr/WiRDS/Matury/matury_kraj.RData')

fac <- matury %>%
  mutate(publiczna=factor (x=publiczna,
                           levels= c(FALSE, TRUE, NA),
                           labels=c('szkola niepubliczna',
                                    'Szkola publiczna',
                                    'brak danych'),
                           exclude = NULL,
                           ordered=T))

fac%>%
  select(matura_nazwa,publiczna,wyniki_matur,rok)%>%
  filter (rok==2014)%>%
  filter (publiczna!="brak danych") %>%
  group_by(matura_nazwa,publiczna) %>%
  summarize(pktsr=mean(wyniki_matur)) %>%
  ggplot(data =.,
         aes( x = matura_nazwa,
              y = pktsr,
              group = publiczna,
              fill = publiczna)) +
  xlab("Nazwa matury")+
  ylab("Sredni wynik punktowy")+
  geom_bar(stat='identity',
           position = 'dodge')+
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=12))+
  scale_fill_discrete(name="Rodzaj szko³y")+
  ggtitle('Œredni wynik poszczególnych matur \ndla szkó³ publicznych i niepublicznych w 2014 roku')+
  geom_text(aes(label=round(pktsr, digits=1)), vjust=-0.5, position=position_dodge(.8), size=4)