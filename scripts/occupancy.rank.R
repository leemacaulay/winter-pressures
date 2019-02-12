library(dplyr)
library(readr)
library(stringr)
library(DT)
library(formattable)

beds <- read_csv("output_data/beds20190207.csv")

cumbria <- c("North Cumbria University Hospitals NHS Trust", "University Hospitals of Morecambe Bay NHS Foundation Trust")

latest <- beds %>%
  filter(!provider == "ENGLAND") %>% 
  filter(measure == "Occupancy rate") %>% 
  filter(date == max(date)) %>% 
  mutate(occupancy.rate = round(value*100, 2), rank = dense_rank(desc(occupancy.rate))) %>% 
  select(rank, provider, occupancy.rate) %>% 
  arrange(desc(occupancy.rate))

datatable(latest) 

latest %>% 
  filter(provider %in% cumbria)

average.occupancy <- beds %>%
  filter(!provider == "ENGLAND") %>% 
  filter(measure == "Occupancy rate") %>%
  group_by(provider) %>% 
  summarise(avg.occupancy.rate=round(mean(value)*100,2)) %>% 
  mutate(rank = dense_rank(desc(avg.occupancy.rate))) %>%
  select(Rank=rank, 
         "NHS Provider"=provider, 
         "Occupancy Rate"=avg.occupancy.rate) %>% 
  arrange(Rank) %>%
  head(10)

average.occupancy.cumbria <- beds %>%
  filter(!provider == "ENGLAND") %>% 
  filter(measure == "Occupancy rate") %>%
  group_by(provider) %>% 
  summarise(avg.occupancy.rate=round(mean(value)*100,2)) %>% 
  mutate(rank = dense_rank(desc(avg.occupancy.rate))) %>%
  select(Rank=rank, 
         "NHS Provider"=provider, 
         "Occupancy Rate"=avg.occupancy.rate) %>% 
  arrange(Rank) %>% 
  filter(`NHS Provider` %in% cumbria)

average.occupancy.final <- rbind(average.occupancy, average.occupancy.cumbria)

customRed = "#ff7f7f"
customRed0 = "#ffcbcb"
customGreen = "#7fff7f"
customYellow = "#fffa8b"

formattable(average.occupancy.final, align =c("c", "l", "c"),
            list(
              Rank = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Occupancy Rate` = color_tile(customYellow, customRed)
              )
            )
