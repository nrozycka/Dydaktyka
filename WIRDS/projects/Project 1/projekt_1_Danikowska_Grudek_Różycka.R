
setwd('D:/szkola/magisterka/drugi semestr/WiRDS')

library(dplyr)
library(ggplot2)
library(ggthemes)

load('matury_kraj.rdata')
matury <- tbl_df(matury)
dim(matury)
glimpse(matury)


tabela <- matury %>%
  select(wyniki_matur)%>%
  group_by(typ=matury$typ_szkoly, rok=matury$rok)%>%
  summarise(procent=if(matury$matura_nazwa=='geografia rozszerzona'){
    N=mean(wyniki_matur/60*100)}
    else{N=mean(wyniki_matur*2)
    })


tabela$typ <- factor(tabela$typ,
                            levels = c('LO', 'LOU', 'LP', 'T', 'TU'),
                            labels = c('Liceum Ogólnokształcące',
                                       'Lic. Og. Uzupełniające',
                                       'Liceum Profilowane',
                                       'Technikum',
                                       'Technikum Uzupełniające'))

ggplot(data=tabela,
       aes(x=rok,
           y=procent,
           colour=typ,
           group=typ))+
  geom_point(size=5)+
  geom_line(size=2)+
  theme_gray()+
  scale_colour_tableau(name='Typ szkoły')+
  geom_line(aes(x=rok,y=30), colour='black', linetype = 3, size=0.8)+
  xlab('Rok')+
  ylab('Procentowe wyniki matur')+
  ggtitle('Średnie procentowe wyniki matur dla poszczególnych \n typów szkół w latach 2010 - 2014 \n')+
  theme(plot.title=(element_text(face='bold', size = 20)))
  
