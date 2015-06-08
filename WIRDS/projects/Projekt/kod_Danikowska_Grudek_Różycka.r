library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)

matury%>%
  count(matura_nazwa, wyniki_matur)%>%
  group_by(matura_nazwa)%>%
  summarise(m=max(wyniki_matur))

load('matury_kraj.rdata')
matury <- tbl_df(matury)
glimpse(matury)

tapply(matury$wyniki_matur, matury$matura_nazwa, max)

pr_matury <- matury %>%
  mutate(procent_matury = ifelse(matura_nazwa %in% 
                                   c("geografia rozszerzona"),
                                 round(wyniki_matur/60*100,2),
                                 round(wyniki_matur/50*100,2)))

pr_matury$wojewodztwo <- as.factor(pr_matury$wojewodztwo)
pr_matury$matura_nazwa <- as.factor(pr_matury$matura_nazwa)
pr_matury$typ_szkoly <- as.factor(pr_matury$typ_szkoly)

pr_matury <- pr_matury%>%
  mutate(zdana = ifelse(procent_matury >= 30, TRUE, FALSE))

pr_matury <- pr_matury%>%
  mutate(zdana = factor( x = zdana,
                         levels = c(TRUE, FALSE),
                         labels = c('zdana',
                                    'niezdana')))

pr_matury <- pr_matury %>%
  mutate(podejscie = as.factor(ifelse(is.na(pop_podejscie),"pierwsze","poprawa")))

pr_matury <- pr_matury%>%
  mutate(typ_szkoly = factor( x = typ_szkoly,
                              levels = c('LO', 'LOU', 'LP', 'T', 'TU'),
                              labels = c('Liceum Ogólnokształcące',
                                         'Liceum Og. Uzupełniające',
                                         'Liceum Profilowane',
                                         'Technikum',
                                         'Technikum Uzupełniające')))



#############################################################################
##############################################################################
# 6 wykresów liniowych z procentem zdanych matur w podziale na tym szkoły i matury


xxx <- pr_matury%>%
  count(typ_szkoly, matura_nazwa, rok, zdana)%>%
  group_by(typ_szkoly,matura_nazwa,rok)%>%
  mutate(procent=round(n/sum(n)*100,2))
xxx

xxx%>%
  filter(zdana=='zdana')%>%
  ggplot(data=.,
         aes(x=rok,
             y=procent,
             colour=typ_szkoly))+
  geom_point(size=3)+
  geom_line(size=1)+
  facet_wrap(~matura_nazwa)

#################################################################################
#############################     WYKRES 1

pr_matury %>%
  #filter(matura_nazwa=='matematyka podstawowa')%>%
  filter(rok %in% c(2010,2014))%>%
  count(typ_szkoly, rok, zdana) %>%
  group_by(typ_szkoly, rok) %>%
  mutate(procent = n/sum(n)) %>%
  ggplot(data = .,
         aes(x = '',
             y = procent,
             group = zdana,
             fill = zdana)) +
  geom_bar(stat = 'identity',
           col='black') +
  xlab("")+
  facet_wrap(~typ_szkoly+rok, ncol=1) + 
  ggtitle('Procent zdanych i niezdanych matur \n w poszczególnych typach szkół w 2010 i 2014 roku\n')+
  coord_flip()+
  theme_calc(base_size = 14)+
  theme(plot.title=element_text(face = "bold", size=20))+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1))+
  #geom_text(aes(label=round(procent*100,0), y=(1-procent)))+
  scale_fill_manual(values = c("#2ca02c","#ff7f0e"), name="Matura:")
  

###########################################################################
k <- pr_matury%>%
  filter(matura_nazwa=='matematyka podstawowa', rok==2011, podejscie=='pierwsze')
na.omit(k)
(a1=anova(lm(procent_matury~wojewodztwo, data=na.omit(k))))
k%>%
  group_by(wojewodztwo)%>%
  summarise(m=mean(procent_matury))%>%
  ggplot( data = .,
          aes ( x = wojewodztwo,
                y = m)) +
  geom_bar(stat = 'identity')

##############################################################################
##############   WYKRES 3-8
pr_matury%>%
  filter(matura_nazwa %in% c("geografia podstawowa"),
         typ_szkoly=='Liceum Ogólnokształcące',
         rok %in% c(2010,2011))%>%
  count(rok,podejscie, zdana)%>%
  group_by(rok, podejscie)%>%
  mutate(procent = n/sum(n))%>%
  ggplot(data = .,
         aes(x = '',
             y = procent,
             group = zdana,
             fill = zdana
         )) +
  geom_bar(stat = 'identity',
           col='black') +
  ylab('procent')+
  facet_wrap(~rok+podejscie, nrow=1) + 
  scale_fill_manual(values = c("#1f83b4","#ff9c0e"), name="Matura:")+
  theme_calc(base_size = 14)+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1))+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  xlab("")+
  #geom_text(face='bold',aes(label=round(procent*100,0)))+
  ggtitle('Zdawalność matur w LO za pierwszym i kolejnym podejściem\n z geografii podstawowej w 2010 i 2011 roku\n')


pr_matury%>%
  filter(matura_nazwa %in% c("geografia rozszerzona"),
         typ_szkoly=='Liceum Ogólnokształcące',
         rok %in% c(2010,2011))%>%
  count(rok,podejscie, zdana)%>%
  group_by(rok, podejscie)%>%
  mutate(procent = n/sum(n))%>%
  ggplot(data = .,
         aes(x = '',
             y = procent,
             group = zdana,
             fill = zdana
         )) +
  geom_bar(stat = 'identity',
           col='black') +
  ylab('procent')+
  facet_wrap(~rok+podejscie, nrow=1) + 
  scale_fill_manual(values = c("#1f83b4","#ff9c0e"), name="Matura:")+
  theme_calc(base_size = 14)+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1))+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  xlab("")+
  #geom_text(face='bold',aes(label=round(procent*100,0)))+
  ggtitle('Zdawalność matur w LO za pierwszym i kolejnym podejściem\n z geografii rozszerzonej w 2010 i 2011 roku\n')

pr_matury%>%
  filter(matura_nazwa %in% c("informatyka podstawowa"),
         typ_szkoly=='Liceum Ogólnokształcące',
         rok %in% c(2010,2011))%>%
  count(rok,podejscie, zdana)%>%
  group_by(rok, podejscie)%>%
  mutate(procent = n/sum(n))%>%
  ggplot(data = .,
         aes(x = '',
             y = procent,
             group = zdana,
             fill = zdana
         )) +
  geom_bar(stat = 'identity',
           col='black') +
  ylab('procent')+
  facet_wrap(~rok+podejscie, nrow=1) + 
  scale_fill_manual(values = c("#1f83b4","#ff9c0e"), name="Matura:")+
  theme_calc(base_size = 14)+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1))+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  xlab("")+
  #geom_text(face='bold',aes(label=round(procent*100,0)))+
  ggtitle('Zdawalność matur w LO za pierwszym i kolejnym podejściem\n z informatyki podstawowej w 2010 i 2011 roku\n')


pr_matury%>%
  filter(matura_nazwa %in% c("informatyka rozszerzona"),
         typ_szkoly=='Liceum Ogólnokształcące',
         rok %in% c(2010,2011))%>%
  count(rok,podejscie, zdana)%>%
  group_by(rok, podejscie)%>%
  mutate(procent = n/sum(n))%>%
  ggplot(data = .,
         aes(x = '',
             y = procent,
             group = zdana,
             fill = zdana
         )) +
  geom_bar(stat = 'identity',
           col='black') +
  ylab('procent')+
  facet_wrap(~rok+podejscie, nrow=1) + 
  scale_fill_manual(values = c("#1f83b4","#ff9c0e"), name="Matura:")+
  theme_calc(base_size = 14)+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1))+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  xlab("")+
  #geom_text(face='bold',aes(label=round(procent*100,0)))+
  ggtitle('Zdawalność matur w LO za pierwszym i kolejnym podejściem\n z informatyki rozszerzonej w 2010 i 2011 roku\n')


pr_matury%>%
  filter(matura_nazwa %in% c("matematyka podstawowa"),
         typ_szkoly=='Liceum Ogólnokształcące',
         rok %in% c(2010,2011))%>%
  count(rok,podejscie, zdana)%>%
  group_by(rok, podejscie)%>%
  mutate(procent = n/sum(n))%>%
  ggplot(data = .,
         aes(x = '',
             y = procent,
             group = zdana,
             fill = zdana
         )) +
  geom_bar(stat = 'identity',
           col='black') +
  ylab('procent')+
  facet_wrap(~rok+podejscie, nrow=1) + 
  scale_fill_manual(values = c("#1f83b4","#ff9c0e"), name="Matura:")+
  theme_calc(base_size = 14)+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1))+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  xlab("")+
  #geom_text(face='bold',aes(label=round(procent*100,0)))+
  ggtitle('Zdawalność matur w LO za pierwszym i kolejnym podejściem\n z matematyki podstawowej w 2010 i 2011 roku\n')


pr_matury%>%
  filter(matura_nazwa %in% c("matematyka rozszerzona"),
         typ_szkoly=='Liceum Ogólnokształcące',
         rok %in% c(2010,2011))%>%
  count(rok,podejscie, zdana)%>%
  group_by(rok, podejscie)%>%
  mutate(procent = n/sum(n))%>%
  ggplot(data = .,
         aes(x = '',
             y = procent,
             group = zdana,
             fill = zdana
         )) +
  geom_bar(stat = 'identity',
           col='black') +
  ylab('procent')+
  facet_wrap(~rok+podejscie, nrow=1) + 
  scale_fill_manual(values = c("#1f83b4","#ff9c0e"), name="Matura:")+
  theme_calc(base_size = 14)+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1))+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  xlab("")+
  #geom_text(face='bold',aes(label=round(procent*100,0)))+
  ggtitle('Zdawalność matur w LO za pierwszym i kolejnym podejściem\n z matematyki rozszerzonej w 2010 i 2011 roku\n')




#################################################################################
############################# WYKRES9
pr_matury %>%
  filter(matura_nazwa %in% c('matematyka podstawowa',
                             'matematyka rozszerzona'),
         rok %in% c(2010,2011))%>%
  ggplot(data=.,aes(x=typ_szkoly,
                    y=procent_matury,
                    fill=typ_szkoly)) +
  geom_violin() +
  facet_wrap(~rok+matura_nazwa, nrow=2)+
  scale_fill_discrete(name="Typ szkoły:")+
  theme_calc(base_size = 15)+
  scale_x_discrete(breaks=NULL)+
  xlab("")+
  ylab("procent z matury")+
  scale_y_continuous(breaks=seq(0,100,10))+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  ggtitle("Częstości uzyskiwanych wyników z matematyki \nw podziale na typy szkół w latach 2010 i 2011\n")
 

############################################################################
####################### WYKRES10
pr_matury%>%
  count(typ_szkoly, rok, podejscie)%>%
  group_by(typ_szkoly, rok)%>%
  mutate(procent=n/sum(n))%>%
  filter(podejscie=='pierwsze')%>%
  ggplot(data=.,
         aes(
           x=rok,
           y=procent,
           fill=typ_szkoly))+
  geom_bar(stat="identity", position=position_dodge(), 
           colour='black', size=0.5)+
  scale_fill_discrete(name="Typ szkoły:")+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.1))+
  theme_calc(base_size = 15)+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  ggtitle("Procent osób piszących maturę pierwszy raz\n w podziale na typ szkoły w latach 2010-2014\n")



############################################################################
####################### WYKRES11
pr_matury%>%
  count(typ_szkoly, rok, podejscie)%>%
  group_by(typ_szkoly, rok)%>%
  mutate(procent=n/sum(n))%>%
  filter(podejscie=='poprawa')%>%
  ggplot(data=.,
         aes(
           x=rok,
           y=procent,
           fill=typ_szkoly))+
  geom_bar(stat="identity", position=position_dodge(), 
           colour='black', size=0.5)+
  scale_fill_discrete(name="Typ szkoły:")+
  scale_y_continuous(labels=percent, breaks=seq(0,0.5,0.1))+
  theme_calc(base_size = 15)+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  ggtitle("Procent osób poprawiających maturę \nw podziale na typ szkoły, w latach 2010-2014\n")


############################################################################
####################### WYKRES12
pr_matury%>%
  count(matura_nazwa, rok, podejscie)%>%
  group_by(matura_nazwa, rok)%>%
  mutate(procent=n/sum(n))%>%
  filter(podejscie=='poprawa')%>%
  ggplot(data=.,
         aes(
           x=rok,
           y=procent,
           fill=matura_nazwa))+
  geom_bar(stat="identity", position=position_dodge(), 
           colour='black', size=0.5)+
  scale_fill_discrete(name="Nazwa przedmiotu:")+
  scale_y_continuous(labels=percent, breaks=seq(0,0.15,0.05))+
  theme_calc(base_size = 15)+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90))+
  ggtitle("Procent osób poprawiających maturę \nw podziale na przedmioty w latach 2010-2014\n")

############################################################################
####################### WYKRES14

pr_matury%>%
  filter(rok %in% c(2011), matura_nazwa=='matematyka podstawowa', wojewodztwo %in% c(levels(as.factor(matury$wojewodztwo))))%>%
  select(wojewodztwo,procent_matury)%>%
  group_by(wojewodztwo)%>%
  summarise(procent=mean(procent_matury))%>%
  ggplot(data=.,
         aes(x=wojewodztwo,
             y=procent))+
  geom_bar(stat='identity',
           position='dodge',
           colour='black',
           fill='#b446b3')+
  scale_y_continuous(breaks=seq(0,100,10))+
  xlab('województwo')+
  theme_calc(base_size = 15)+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90),
        axis.text.x=element_text(angle=30,
                                 vjust=1,
                                 hjust=1,
                                 size=15))+
  geom_text(aes(x=wojewodztwo,y=(procent-3), label=round(procent,0)))+
  ggtitle('Średni wynik z matematyki podstawowej w 2011 roku \ndla poszczególnych województw\n')

tabela<-pr_matury%>%
  filter(rok %in% c(2011), matura_nazwa=='matematyka podstawowa', wojewodztwo %in% c(levels(as.factor(matury$wojewodztwo))))%>%
  select(wojewodztwo,procent_matury)%>%
  group_by(wojewodztwo)

anova(lm(procent_matury~wojewodztwo, data=tabela))

pr_matury%>%
  filter(rok %in% c(2011), matura_nazwa=='matematyka podstawowa', wojewodztwo %in% c(levels(as.factor(matury$wojewodztwo))))%>%
  select(wojewodztwo,procent_matury)%>%
  group_by(wojewodztwo)%>%
  summarise(procent=mean(procent_matury))%>%
  arrange(desc(procent))


############################################################################
####################### WYKRES13


pr_matury%>%
  filter(rok %in% c(2011), matura_nazwa=='matematyka podstawowa', wojewodztwo %in% c(levels(as.factor(matury$wojewodztwo))))%>%
  count(wojewodztwo,zdana)%>%
  group_by(wojewodztwo)%>%
  mutate(procent = n/sum(n))%>%  
  filter(zdana=='zdana')%>%
  ggplot(data=.,
         aes(x=wojewodztwo,
             y=procent))+
  geom_bar(stat='identity',
           position='dodge',
           colour='black',
           fill='#e377c2')+
  scale_y_continuous(labels=percent, breaks=seq(0,1,0.10))+
  xlab('województwo')+
  theme_calc(base_size = 15)+
  theme(plot.title=element_text(face = "bold", size=20),
        axis.title.y = element_text(angle=90),
        axis.text.x=element_text(angle=30,
                                 vjust=1,
                                 hjust=1,
                                 size=15))+
  geom_text(aes(x=wojewodztwo,y=(procent-0.05), label=round(procent*100,1)))+
  ggtitle('Procent zdanych matur z matematyki podstawowej w 2011 roku \nw podziale na województwa\n')
