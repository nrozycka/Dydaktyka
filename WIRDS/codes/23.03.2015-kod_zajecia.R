### zajęcia 
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readxl)
### wczytanie danych
load('WIRDS/datasets/MillwardBrown.RData')
### przetworzenie anscombe 
anscombe_transmuted <- anscombe %>% 
  gather(x,val,x1:x4) %>% 
  gather(y,val2,y1:y4) %>% 
  mutate(OX = extract_numeric(x),
         OY = extract_numeric(y)) %>%
  filter(OX == OY) %>% 
  mutate(group = as.factor(OX)) %>%
  tbl_df()
### 
anscombe_transmuted %>%
  group_by(group) %>%
  do(lm = summary(lm(val2~val,.))) %>%
  .$lm


ggplot(data = anscombe_transmuted,
        aes(x = val,
            y = val2)) +
  geom_point(size=10) +
  facet_wrap(~group) +
  geom_smooth(method ='loess')

# millwardbrown -----------
popis <- wyniki %>%
  filter(name %in%
           c('Platforma Obywatelska',
             'Prawo i Sprawiedliwość'))

### z podziałem wg koloru
ggplot(data = popis,
       aes(x = turnout,
           y = value,
           colour = name)) +
  geom_point(size=6) +
  geom_smooth(method = 'lm',
              se = F,
              size = 4)
### z podziałem wg kształtu
ggplot(data = popis,
       aes(x = turnout,
           y = value,
           shape = name)) +
  geom_point(size=6)  
### bez podziału
ggplot(data = popis,
       aes(x = turnout,
           y = value)) +
  geom_point(size=6,
             alpha = 0.3)  +
  geom_smooth(method ='lm')

###  
popis %>%
  tbl_df() %>%
  select(ID,value,name) %>%
  mutate(name = 
           ifelse(name =='Platforma Obywatelska','PO','PiS')) %>%
  spread(name,value) %>%
  ggplot(data = .,
         aes(x = PiS,
             y = PO)) +
  geom_point(size = 5) +
  geom_smooth(method = 'loess',se=F, col= 'red') +
  geom_smooth(method = 'lm',se = F)

# szereg czasowy -------
popis <- popis %>%
  mutate(collected_on = as.character(collected_on),
         collected_on = as.Date(collected_on,
                                '%d/%m/%Y')) %>%
  tbl_df()


ggplot(data = popis,
       aes( x = collected_on,
            y = value, 
            colour = name,
            size = turnout)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = 'loess',
              se = F,
              span = 0.6) +
  scale_colour_manual(values = c('red','blue'))

##### 

gosp <- read_excel(path = 'WIRDS/datasets/gospodarstwa.xls')

gosp

ggplot(data = gosp,
       aes(x = dochg,
           y = wydg)) +
  geom_point(size = 5,
             alpha = 0.5) +
  facet_wrap(~klm) +
  geom_smooth(method='lm')











