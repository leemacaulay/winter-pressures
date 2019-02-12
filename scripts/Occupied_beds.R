
library(openxlsx)
library(tidyr)
library(dplyr)

## Let's get the most recent Winter Daily SitRep timeseries data for 2018/2019. We need to use a workaround because readxl can't grab URLs directly yet (readxl#278)
## https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/winter-daily-sitrep-2018-19-data/

tmp <- tempfile(fileext = ".xlsx")
httr::GET(url = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/02/Winter-data-timeseries-20190207.xlsx",
          httr::write_disk(tmp))

## Adapted from this - https://howisonlab.github.io/datawrangling/Handling_multi_indexes.html#a-tidyverse-solution

bed_headers <- read_excel(tmp, sheet = "G&A beds", skip = 13, col_names = FALSE, na="..", n_max = 2)

# change excel dates into readable dates
bed_headers[1,] <- as.character(convertToDate(bed_headers[1,]))
# fill only works down or up, so have to transpose headers
long_bed_headers <- data.frame(t(bed_headers))
# remove unneeded rows
long_bed_headers <- long_bed_headers[5:319,]
# Add column names down the columns
long_bed_headers <- fill(long_bed_headers,1:2)
# back to vertical
bed_headers <- data.frame(t(long_bed_headers))

# summarize using str_c from stringr which collapses strings.
# summarize_all makes it work on all columns.
column_labels <- bed_headers %>% summarize_all(str_c, collapse = "--")

# pull them into one character vector, add remaining columns
headers = c("region","X1","code","provider", unname(unlist(column_labels[1,])))

# tidy up the finished data frame
beds_long <- beds %>%
  gather(variable, value, -provider, -code, -region, -X1) %>% # gather all measures, omit rest of columns
  separate(variable, c("date", "measure"), sep="--",remove=T) %>% # separate them out
  select(-X1) %>% 
  drop_na(value) %>% 
  select(provider, region, code, date, measure, value)

write_csv(beds_long, "output_data/beds20190207.csv")

beds <- read_csv("output_data/beds20190207.csv")
  
