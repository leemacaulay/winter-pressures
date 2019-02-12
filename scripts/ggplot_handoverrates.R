
library(dplyr)
library(readr)
library(stringr)
library(DT)
library(ggplot2)
library(bbplot)

ambulances <- read_csv("output_data/ambulances20190207.csv")

cumbria <- c("North Cumbria University Hospitals NHS Trust", "University Hospitals of Morecambe Bay NHS Foundation Trust")

average <- ambulances %>%
  filter(!provider == "ENGLAND") %>% 
  filter(measure %in% c("Delay >60 mins", "Arriving by ambulance", "Delay 30-60 mins")) %>%
  group_by(provider, measure) %>% 
  summarise(average=mean(value))

sum <- ambulances %>%
  filter(!provider == "ENGLAND") %>% 
  filter(measure %in% c("Delay >60 mins", "Arriving by ambulance", "Delay 30-60 mins")) %>%
  group_by(provider, measure) %>% 
  summarise(sum=sum(value))

sum.delay.rate <- sum %>% 
  spread(measure, sum) %>%
  rename(
    arriving=`Arriving by ambulance`,
    thirty.to.sixty=`Delay 30-60 mins`,
    over.sixty=`Delay >60 mins`
  ) %>% 
  mutate(short.name=str_remove(provider, " NHS Foundation Trust| NHS Trust")) %>% 
  mutate(percent.delayed=round((thirty.to.sixty+over.sixty)/arriving*100,4)) %>% 
  arrange(desc(percent.delayed)) %>% 
  head(50) 

delay.rate <- average %>% 
  spread(measure, average) %>%
  rename(
    arriving=`Arriving by ambulance`,
    thirty.to.sixty=`Delay 30-60 mins`,
    over.sixty=`Delay >60 mins`
  ) %>% 
  mutate(short.name=str_remove(provider, " NHS Foundation Trust| NHS Trust")) %>% 
  mutate(percent.delayed=round((thirty.to.sixty+over.sixty)/arriving*100,4)) %>% 
  arrange(desc(percent.delayed)) %>% 
  head(50)

plot <- ggplot(delay.rate, aes(x=reorder(short.name, percent.delayed), y=percent.delayed)) +
  geom_col(width=0.7,
           position="identity", 
           fill=ifelse(delay.rate$provider %in% cumbria, "#1380A1", "#dddddd")) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() +
  coord_flip() +
  theme(panel.grid.major.x=element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank())
  
final <- plot + theme(axis.text.y = element_text(size = 8, vjust=0.5, hjust=1, margin=margin(0,-20,0,0))) +
  labs(title="Delays getting treatment",
       subtitle = "Average rate of handover delays (>30 mins) since Dec 2018")

finalise_plot(plot_name = final,
              source_name = "NHS England",
              save_filepath = "rmd/handover_delay.png")
             
  