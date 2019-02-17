library(dplyr)
library(readr)
library(stringr)
library(DT)
library(formattable)

beds <- read_csv("output_data/beds20190214.csv")

cumbria <- c("North Cumbria University Hospitals NHS Trust", "University Hospitals of Morecambe Bay NHS Foundation Trust")
dates <- c(as.Date("2019-02-03"), as.Date("2019-02-10"))

latest <- beds %>%
  filter(!provider == "ENGLAND") %>% 
  filter(measure == "Occupancy rate") %>% 
  filter(date == max(date)) %>% 
  mutate(occupancy.rate = round(value*100, 2), rank = dense_rank(desc(occupancy.rate))) %>% 
  select(rank, provider, occupancy.rate) %>% 
  arrange(desc(occupancy.rate))

### https://stackoverflow.com/questions/39920870/merging-rows-with-shared-information

f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) first(x) else NA
}

latest <- beds %>%
  filter(!provider == "ENGLAND") %>% 
  filter(measure == "Occupancy rate") %>% 
  mutate(occupancy.rate = round(value*100, 2)) %>%
  filter(date %in% dates) %>% 
  spread(date, occupancy.rate) %>% 
  select(provider, 
         last.week=as.character(as.Date(dates[1])),
         this.week=as.character(as.Date(dates[2]))
         ) %>% 
  group_by(provider) %>% 
  summarise_all(funs(f)) 

latest <- latest %>% 
  mutate(rank = dense_rank(desc(this.week)),
         previous.rank = dense_rank(desc(last.week)),
         change = this.week - last.week) %>% 
  select(rank, previous.rank, provider, this.week, last.week, change) %>%
  arrange(desc(this.week))

avg.occupancy <- latest %>% 
  select(Rank=rank,
         "Previous Rank"=previous.rank,
         "NHS Provider"=provider, 
         "Occupancy Rate"=this.week,
         "Change"=change) %>% 
  head(10)

avg.occupancy.cumbria <- latest %>% 
  select(Rank=rank, 
         "Previous Rank"=previous.rank,
         "NHS Provider"=provider, 
         "Occupancy Rate"=this.week,
         "Change"=change) %>% 
  filter(`NHS Provider` %in% cumbria)
  

datatable(avg.occupancy) 

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

avg.occupancy.final <- rbind(avg.occupancy, avg.occupancy.cumbria)

customRed = "#ff7f7f"
customRed0 = "#ffcbcb"
customGreen = "#7fff7f"
customYellow = "#fffa8b"

formattable(avg.occupancy.final, align =c("c", "c", "l", "c", "c"),
            list(
              Rank = formatter("span", style = ~ style(color = "grey",font.weight = "bold"), 
                               x ~ icontext(
                                 case_when(
                                 x < avg.occupancy.final$`Previous Rank` ~ "arrow-up",
                                 x == avg.occupancy.final$`Previous Rank` ~ "minus",
                                 x > avg.occupancy.final$`Previous Rank` ~ "arrow-down"
                                 ), x)),
              `Previous Rank` = formatter("span", style = ~ style(color = "grey",font.style = "italic")),
              `Occupancy Rate` = color_tile(customYellow, customRed),
              Change = formatter("span", style = x ~ style(color = ifelse(x >= 0, "red", "green")))))
