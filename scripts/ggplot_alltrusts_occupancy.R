
library(dplyr)
library(ggplot2)
library(stringr)
library(bbplot)
library(gghighlight)

beds <- read_csv("output_data/beds20190207.csv")

beds %>%
  filter(!provider=="ENGLAND") %>% 
  filter(measure=="Occupancy rate") %>% 
  filter(date=="2019-02-03") %>% 
  ggplot(aes(x=reorder(provider, desc(value)), y=value)) +
  geom_bar(stat="identity", position="identity", fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  coord_flip() +
  bbc_style()
