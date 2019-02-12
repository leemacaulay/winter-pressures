library(dplyr)
library(ggplot2)
library(ggalt)
library(bbplot)

dumbell_df <- beds %>%
  filter(!provider=="ENGLAND") %>% 
  filter(measure=="Occupancy rate") %>% 
  group_by(provider) %>% 
  mutate(max=max(value), min=min(value)) %>% 
  mutate(gap=max-min) %>% 
  select(provider, min, max, gap) %>%
  unique() %>%
  arrange(gap)

ggplot(dumbell_df) +
  geom_dumbbell(aes(y = reorder(provider, desc(gap)), x = min, xend = max), 
                colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  bbc_style() +
  theme(axis.text.y=element_blank()) +
  

