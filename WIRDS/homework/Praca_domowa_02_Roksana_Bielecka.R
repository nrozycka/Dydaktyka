ggplot(data=dane_liniowy,
       aes(x=rok,
           y=y,
           label=kategoria,
           colour=kategoria,
           group=kategoria))+
  geom_point()+
  geom_text(hjust=0, vjust=-2, size=5, )+
  geom_line()+
  theme_bw()+ 
  xlab('Rok spisu')+ 
  ylab('Udzia³ w (%)')+ 
  ggtitle('Udzia³...')