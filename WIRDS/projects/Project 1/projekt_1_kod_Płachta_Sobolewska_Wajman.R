# <---------------------------------- biblioteki --------------------------------------
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(scales)
library(gridExtra)

# <---------------------------------- filtrowanie danych ---------------------------------

matury2014mat <- matury %>%
  filter(rok == '2014', matura_nazwa == 'matematyka podstawowa', plec != 'NA')
matury2014mat$plec <- factor(matury2014mat$plec,
                             levels = c("k", "m"),
                             labels = c("kobieta", "mê¿czyzna"))
matury2014mat2 <- matury %>%
  filter(rok == '2014', matura_nazwa == 'matematyka podstawowa', plec != 'NA')
matury2014mat2$plec <- factor(matury2014mat2$plec,
                              levels = c("m", "k"),
                              labels = c("mê¿czyzna", "kobieta"),
                              ordered = T)

# <------------------------------------ MÊ¯CZYNI ----------------------------------------

p1 <- matury2014mat %>%
  count(wyniki_matur,plec) %>%
  group_by(plec) %>%
  mutate(procent = n/sum(n)) %>%
  ggplot(data =., aes ( x = wyniki_matur, y = procent, fill = plec)) +
  geom_bar(stat = 'identity', position ='identity', col ='black') +
  geom_abline(intercept = 14.5, slope = -1, colour = 'black', size = 0.8) +
  scale_fill_manual(values=c("red", "blue")) +
  theme_bw() +
  coord_flip() +
  ggtitle("Mê¿czyŸni") +
  geom_text(aes(14, 0.0335, label = "próg zdawalnoœci", size = 1)) +
  scale_y_continuous("odsetek maturzystów", labels = percent) +
  theme(legend.position="none", 
        axis.title.y = element_blank(),
        plot.margin=unit(c(0.5,0.2,0.1,-.1),"cm"))  # c(up,right,down,left)

# <------------------------------------ KOBIETY ------------------------------------------

p2 <- matury2014mat2 %>%
  count(wyniki_matur,plec) %>%
  group_by(plec) %>%
  mutate(procent = n/sum(n)) %>%
  ggplot(data =., aes ( x = wyniki_matur, y = procent, fill = plec)) +
  geom_bar(stat = 'identity', position ='identity', col = 'black') +
  geom_abline(intercept = 14.5, slope = -1, colour = 'black', size = 0.8) +
  scale_fill_manual(values=c("blue", "red")) +
  theme_bw() +
  coord_flip() +
  scale_y_reverse("odsetek maturzystów", labels = percent) +
  ggtitle("Kobiety") +
  geom_text(aes(14, 0.0335, label = "próg zdawalnoœci", size = 1)) +
  xlab("Wynik [pkt]") +
  theme(legend.position="none", 
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
                plot.margin=unit(c(0.5,0.02,0.1,0.05),"cm")) # c(up,right,down,left)

#<---------------------------------- WYKRES --------------------------------------------

grid.arrange(p2, p1, 
             ncol = 2, 
             widths = c(1,1), 
             main = textGrob("Wyniki podstawowej matury z matematyki wed³ug p³ci w 2014 r.", 
                             just = 'centre', 
                             gp = gpar(cex=1.1)))

