library(dataRetrieval)
library(sharpshootR)
library(lubridate)
library(dplyr)
library(readr)
library(here)
library(rvest)
source("functions.R")
source("parameters.R")

# Adult ------------------------------------

## Escapement --------------------------------------

### In river
url_escapement <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=All&locType=location&location=Sacramento+and+San+Joaquin+River+Systems%3AAll%3AAll"
escapement <- read_csv(url_escapement) %>%
  mutate(Year2 = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  filter(Year2 <= report_year)%>%
  select(-Year)
write_csv(escapement, "data_raw/escapement.csv")

url_downstreamRBDD <-  "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=In-River&locType=location&location=Sacramento+River+System%3ASacramento+River%3AMainstem+-+Downstream+of+RBDD"
esc_dstRBDD <- read_csv(url_downstreamRBDD) %>%
  mutate(Year2 = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  filter(Year2 <= report_year)%>%
  select(-Year) %>%
  mutate(Reach = "DST")

url_upstreamRBDD <-  "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=In-River&locType=location&location=Sacramento+River+System%3ASacramento+River%3AMainstem+-+Upstream+of+RBDD"
esc_ustRBDD <- read_csv(url_upstreamRBDD) %>%
  mutate(Year2 = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  filter(Year2 <= report_year) %>%
  select(-Year)%>%
  mutate(Reach = "UST")

url_cck <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=In-River&locType=location&location=Sacramento+River+System%3ASacramento+River%3AClear+Creek"
esc_cck <- read_csv(url_cck) %>%
  mutate(Year2 = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  filter(Year2 <= report_year) %>%
  select(-Year)%>%
  mutate(Reach = "CCK")

url_battle <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=In-River&locType=location&location=Sacramento+River+System%3ASacramento+River%3ABattle+Creek+-+Upstream+of+CNFH"
esc_battle <- read_csv(url_battle) %>%
  mutate(Year2 = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  filter(Year2 <= report_year)%>%
  select(-Year)%>%
  mutate(Reach = "BAT")

esc_byreach <- bind_rows(esc_dstRBDD, esc_ustRBDD, esc_cck, esc_battle) %>%
  rename(Escapement = Annual,
         Year = Year2) %>%
  mutate(Reach_text = case_when(Reach == "UST" ~ "Upstream RBDD",
                           Reach == "DST" ~ "Downstream RBDD",
                           Reach == "CCK" ~ "Clear Creek",
                           Reach == "BAT" ~ "Battle Creek"))
write_csv(esc_byreach, "data_raw/escapement_by_reach.csv")

### Hatchery

url_cnfh <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=Hatchery&locType=location&location=Sacramento+River+System%3ASacramento+River%3AHatchery+Transfers+to+Battle+Creek+-+CNFH"
cnfh <- read_csv(url_cnfh) %>%
  mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  filter(Year <= report_year)%>%
  mutate(Reach = "CNFH",
         Reach_text = "Coleman Hatchery")

url_lstn <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/grandtab_graph.php?sc=1&outputFormat=csv&species=Chinook%3AWinter&type=Hatchery&locType=location&location=Sacramento+River+System%3ASacramento+River%3AHatchery+Transfers+to+Livingston+Stone+NFH"
lstn <- read_csv(url_lstn) %>%
  mutate(Year = as.numeric(substr(Year, start = 1, stop = 4))) %>%
  filter(Year <= report_year)%>%
  mutate(Reach = "LSTON",
         Reach_text = "Livingston Stone Hatchery")

hatchery <- bind_rows(cnfh, lstn) %>%
  rename(Escapement = Annual)

write_csv(hatchery, "data_raw/hatchery_transfer.csv")

## Carcass Survey ------------------------------

url_carcass <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/carcass_detail.php?sc=1&outputFormat=csv&year=all&run=winter&clip=all&sex=all"
carcass <- read_csv(url_carcass) %>%
  mutate(surveydate = ymd(surveydate),
         year = year(surveydate)) %>%
  filter(year <= report_year)

write_csv(carcass, "data_raw/carcass_data.csv")

## Fecundity -----------------------------------
url_jpe <- "https://www.cbr.washington.edu/sacramento/data/jpe_data.html"
fecundity <- url_jpe %>%
  read_html() %>%
  html_nodes(xpath = '/html/body/div[2]/div[6]/table') %>%
  html_table() %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  filter(row_number() <= n()-1,
         !brood_year %in% c(2003, 2004)) %>%
  mutate(brood_year = as.numeric(brood_year),
         value = stringr::str_replace(value, ",", ""),
         fecundity = as.numeric(value))


write_csv(fecundity, "data_raw/fecundity_data.csv")

## Redd data --------------------------------

url_redd_dewatering <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/redd_dewatering.php?sc=1&outputFormat=csv&year=all&run=W&section=all"
redd_d <- read_csv(url_redd_dewatering) %>%
  filter(year <= report_year)

write_csv(redd_d, "data_raw/redd_dewatering_data.csv")


url_aerial_redd <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/redd_aerial.php?sc=1&outputFormat=csv&year=all&run=Winter&reach=all"
redd <- read_csv(url_redd) %>%
  filter(year <= report_year)

write_csv(redd, "data_raw/redd_data.csv")

# Egg to Fry --------------------------------


# Juvenile ---------------------------

## Upper Sac

## Mid-low ----------------------------

## Delta  ---------------------------


