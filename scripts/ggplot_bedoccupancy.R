
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(bbplot)

beds <- read_csv("output_data/beds20190214.csv")

bar <- beds %>%
  filter(str_detect(provider, "North Cumbria") | str_detect(provider, "Morecambe Bay")) %>%
  filter(measure=="Occupancy rate")
  
ggplot(bar, aes(x = date, y = value, fill = provider)) +
  geom_bar(stat="identity", 
           position="dodge") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  facet_grid(provider~.)
           

line <- beds %>% 
  filter(str_detect(provider, "North Cumbria") | str_detect(provider, "Morecambe Bay")) %>% 
  filter(measure=="Occupancy rate") %>% 
  ggplot(aes(x=date, y=value, color=provider)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_vline(xintercept = as.numeric(as.Date("2019-02-03")), size = 1, colour="#333333", linetype = "dashed") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1")) +
  bbc_style() +
  labs(title="Feeling the pressure",
       subtitle = "Bed occupancy rates in Cumbria") +
  guides(colour=FALSE) +
  scale_y_continuous(limits=c(0,1),
                     breaks = seq(0, 1, by = 0.25),
                     labels = c("0%", "25", "50", "75", "100"))

line

labelled <- line +
  xlim(c(as.Date("2018-12-01", "%Y-%m-%d"), as.Date("2019-02-17", "%Y-%m-%d"))) +
  geom_label(aes(x = as.Date("2019-02-10", "%Y-%m-%d"), y = 0.97, label = "96.9%"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#FAAB18", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6) +
  geom_label(aes(x = as.Date("2019-02-10", "%Y-%m-%d"), y = 0.92, label = "94%"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#1380A1", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6) +
  geom_label(aes(x = as.Date("2019-01-25", "%Y-%m-%d"), y = 0.2, label = "North Cumbria"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#FAAB18", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6) +
  geom_label(aes(x = as.Date("2019-01-25", "%Y-%m-%d"), y = 0.15, label = "Morecambe Bay"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#1380A1", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6)

finalise_plot(plot_name = labelled,
              source = "Source: NHS England",
              save_filepath = "rmd/occupied_cumbria_20190210.png",
              width_pixels = 750,
              height_pixels = 550)

