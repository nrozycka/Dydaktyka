load("matury_kraj.RData")
library(dplyr)
library(ggplot2)
matury <- tbl_df(matury)

# propozycja 3 ------------------------------------------------------------
matury %>%
  filter(wyniki_matur < 18) %>%
  count(typ_szkoly, rok) %>%
  group_by(typ_szkoly) %>%
  mutate(procent = n / sum(n)) %>%
  ggplot(data = ., aes(x = rok,
                       y = procent,
                       fill = typ_szkoly,
                       group = typ_szkoly,
                       label = round(procent, 2),
                       ymax = 1)) +
  geom_bar(stat = "identity", position = "fill") +
  ggtitle("Procentowy udział rok po roku \nniezaliczonych matur pod względem typu szkoły") +
  xlab("Rok pisania matury") +
  ylab("Procent niezaliczonych") +
  geom_text(position = "fill", vjust = 1.2, col = "white", size = 4) +
  scale_fill_discrete(name = "Typ szkoły")


