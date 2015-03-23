library(XLConnect)
library(dplyr)
fName <- "C:/Users/Super Kamil 3000/Desktop/R_workspace/gospodarstwa.xls"
wb <- loadWorkbook(fName)
gosp <- readWorksheet(wb, sheet = "gospodarstwa")



gosp$woj <- factor(gosp$woj,
                   levels = c("02", "04", "06", "08", "10", "12", "14", "16", "18", "20",
                              "22", "24", "26", "28", "30", "32"),
                   labels = c("dolnoœl¹skie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "³ódzkie",
                              "ma³opolskie", "mazowieckie", "opolskie", "podkarpackie", "podlaskie",
                              "pomorskie", "œl¹skie", "œwiêtokrzyskie", "warmiñsko-mazurskie",
                              "wielkopolskie", "zachodniopomorskie"))

gosp %>%
  count(klm,woj) %>%
  group_by(woj) %>%
  mutate(procent = n/sum(n)) %>%
  ggplot(data = ., aes(x = " ", y = procent, group = klm, fill = klm)) +
  xlab("Województwo") +
  geom_bar(stat = 'identity', col='black') +
  facet_wrap(~woj, ncol=1) + 
  coord_flip()
