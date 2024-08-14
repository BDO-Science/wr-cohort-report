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

url_jpe <- "https://www.cbr.washington.edu/sacramento/data/jpe/jpedata_all.txt"
jpe_data <- read.delim(url_jpe, header = TRUE, sep = "|", dec = ".")
unique(jpe_data$component)
write_csv(jpe_data, "data_raw/jpe_data.csv")


url_dewatering <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/redd_dewatering.php?sc=1&outputFormat=csv&year=all&run=W&section=all"
dewater_data <- read_csv(url_dewatering)
current_year_dewatering <- dewater_data %>%
  filter(year == report_year)

write_csv(current_year_dewatering, "data_raw/current_year_dewatering.csv")



# Juvenile ---------------------------


## Upper Sac -----------------------
url_stranding <- "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_stranding.php?sc=1&outputFormat=csv&year=all&section=all"
stranding_data <- read_csv(url_stranding)
write_csv(stranding_data, "data_raw/stranding_data.csv")


## Mid-low ----------------------------

### Hatchery Releases
hatcheryreach_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/calfishtrack.php?sc=1&outputFormat=csv&surv=reach&population=Winter_H_",report_year+1)
hatchreach_data <- read_csv(hatcheryreach_url) %>%
  filter(!is.na(rkm_start)) %>%
  select(rkm_start, rkm_end, reach_start, reach_end,
         reach_survival_est = reach_survival_est_per10km,
         SE_reach = SE,
         LCL_reach = LCL,
         UCL_reach = UCL,
         rel_date = mean_rel_date)

hatcherycum_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/calfishtrack.php?sc=1&outputFormat=csv&surv=cumulative&population=Winter_H_",report_year+1)
hatchcum_data <- read_csv(hatcherycum_url) %>%
  filter(!is.na(rkm)) %>%
  select(rkm_start = rkm,
         receiver,
         cumulative_survival_est,
         rkm_start = rkm,
         SE_cum = SE,
         LCL_cum = LCL,
         UCL_cum = UCL,
         count,
         rel_date
         )

hatchery_data <- left_join(hatchcum_data, hatchreach_data, by= c("rkm_start", "rel_date"))

write_csv(hatchery_data, "data_raw/hatchery_release_data.csv")

### Catch

catch_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/sampling_graph.php?sc=1&outputFormat=csv&year=", report_year,
"&species=CHN%3AWinter&loc=all%3Aall%3Aall&cumData=1&typeData=raw&addData=1")
juvenile_data <- read_csv(catch_url)

write_csv(juvenile_data, "data_raw/cohort_juv_data.csv")

### RBDD Fork Length

raw_rbdd_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.1365.10&entityid=58540ac4ed34ce05f3309510f4be91e5"
rbdd <- read_csv(raw_rbdd_url) %>%
  filter(run == "winter run")

write_csv(rbdd,"data_raw/raw_wr_rbdd.csv")

fl_url <- paste0("https://www.cbr.washington.edu/sacramento/data/php/rpt/redbluff_graph.php?sc=1&outputFormat=csv&byear=", report_year,
"&species=Chinook%3AWinter&addData=length")
fl_data <- read_csv(fl_url) %>%
  janitor::clean_names() %>%
  filter(!is.na(brood_year))

write_csv(fl_data, "data_raw/rbdd_juv_fl.csv")

## Delta  ---------------------------

### Food availability ---------
library(zooper)
library(sf)
library(deltamapr)

# Indicate when the dataset starts.
startyear = report_year-10
endyear = report_year+1

#### Mesozoop -------------

# Mesozooplankton query from zooper
meso <- Zoopsynther(Data_type = "Community",                        # Taxa or Community
                       Sources = c("EMP","FMWT","STN","20mm"),     # EMP, FMWT, STN, DOP, 20 mm
                       Size_class = "Meso",
                       Date_range = c(paste0(startyear, "-01-01"), paste0(endyear, "-12-31")))     # last 10 years

# Filter to orders of interest and time frame for juveniles in the Delta
juv_meso <- meso %>%
  filter(Order %in% c("Cyclopoida", "Cladocera", "Calanoida") ) %>%
  mutate(Month = lubridate::month(Date)) %>%
  filter(Month %in% c(12, 1, 2, 3,4,5,6)) %>%
  mutate(Month_fac = month(Month, label = TRUE, abbr = TRUE),
         Month_fac = factor(Month_fac,
                            levels = c("Dec", "Jan", "Feb", "Mar", "Apr","May", "Jun")),
         Year2 = if_else(Month == 12, Year+1, Year))  %>%
  mutate(Year_type = case_when(Year2 == endyear ~ as.character(report_year),
                               Year2 <= report_year & Year2 >=startyear  ~ "Ten-year Average"))

# Add regional designation from deltamapr
juv_meso_region <- juv_meso %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(R_EDSM_Regions_1718P1)) %>%
  st_join(R_EDSM_Regions_1718P1)

# Separate current year from averages
current_meso <- juv_meso_region %>%
  filter(Year2 == endyear) %>%
  group_by(Year2, Order, Region, Year_type, Month, Month_fac) %>%
  summarize(sumCPUE = sum(CPUE)) %>%
  ungroup() %>%
  group_by(Order, Region, Year_type, Month, Month_fac) %>%
  summarize(meanCPUE = mean(sumCPUE)) %>%
  ungroup()

# Calculate annual sum, then mean of annual sums for past 10 years
mean_meso <- juv_meso_region %>%
  filter(Year_type == "Ten-year Average") %>%
  group_by(Year2, Order, Region, Year_type, Month, Month_fac) %>%
  summarize(sumCPUE = sum(CPUE)) %>%
  ungroup() %>%
  group_by(Order, Region, Year_type, Month, Month_fac) %>%
  summarize(meanCPUE = mean(sumCPUE))

# Bring average back together with current
meso_all <- bind_rows(current_meso, mean_meso)

# Write file
write_csv(meso_all, "data_raw/zooper_meso.csv")

#### Macrozoop ----------------
# Macrozooplankton query

macro <- Zoopsynther(Data_type = "Community",                        # Taxa or Community
                             Sources = c("EMP","FMWT","STN","20mm"),     # EMP, FMWT, STN, DOP, 20 mm
                             Size_class = "Macro",
                             Date_range = c(paste0(startyear, "-01-01"), paste0(endyear, "-12-31")))     # last 10 years

# Filter to orders of interest and time frame for juveniles in the Delta
juv_macro <- macro %>%
  mutate(Month = lubridate::month(Date)) %>%
  filter(Month %in% c(12,1,2,3,4,5,6))%>%
  mutate(Month_fac = month(Month, label = TRUE, abbr = TRUE),
         Month_fac = factor(Month_fac,
                            levels = c("Dec", "Jan", "Feb", "Mar", "Apr","May", "Jun")),
         Year2 = if_else(Month == 12, Year+1, Year))  %>%
  mutate(Year_type = case_when(Year2 == endyear ~ as.character(report_year),
                               Year2 <= report_year & Year2 >=startyear  ~ "Ten-year Average"))

# Add regional designation from deltamapr
juv_macro_region <- juv_macro %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(R_EDSM_Regions_1718P1)) %>%
  st_join(R_EDSM_Regions_1718P1)

# Separate current year from averages
current_macro <- juv_macro_region %>%
  filter(Year2 == endyear) %>%
  group_by(Year2, Order, Region, Year_type, Month, Month_fac) %>%
  summarize(sumCPUE = sum(CPUE)) %>%
  ungroup() %>%
  group_by(Order, Region, Year_type, Month, Month_fac) %>%
  summarize(meanCPUE = mean(sumCPUE)) %>%
  ungroup()

# Calculate annual sum, then mean of annual sums for past 10 years
mean_macro <- juv_macro_region %>%
  filter(Year_type == "Ten-year Average") %>%
  group_by(Year2, Order, Region, Year_type, Month, Month_fac) %>%
  summarize(sumCPUE = sum(CPUE)) %>%
  ungroup() %>%
  group_by(Order, Region, Year_type, Month, Month_fac) %>%
  summarize(meanCPUE = mean(sumCPUE))

# Combine current year and averaged data
macro_all <- bind_rows(current_macro, mean_macro)

# Write file
write_csv(macro_all, "data_raw/zooper_macro.csv")


### Loss -----------------------------

# Older juveniles, all races query
salvage_data <- read.csv("https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=1%3Aall&dnaOnly=no&age=older")

salvage_wr <- salvage_data %>%
  filter(LAD.Race == "Winter") %>%
  mutate(Date = date(Sample.Time),
         Year = year(Date),
         Month = month(Date)) %>%
  select(Date, Year, Month, Facility, Adipose.Clip, Length, nfish, Expanded.Salvage, Loss, DNA.Sample.ID, X14.day.OMRI)
write_csv(salvage_wr, "data_raw/salvage_allyears.csv")

# summarize daily cumulative loss
daily_salvage <- salvage_wr %>%
  mutate(Month = month(Date)) %>%
  filter(Year <= report_year+1 & Year >= report_year,
         Date >= as.Date(paste0(report_year, "-10-01")),
         Date <= as.Date(paste0(report_year+1, "-06-30"))) %>%
  select(Date, Facility, nfish, Expanded.Salvage, Loss) %>%
  complete(Date = seq.Date(as.Date(paste0(report_year, "-10-01")),
                           as.Date(paste0(report_year+1, "-06-30")), by = "day"),
           Facility,
           fill = list(nfish = 0, Expanded.Salvage = 0, Loss = 0)) %>%
  group_by(Facility) %>%
  mutate(cumLoss = cumsum(Loss)) %>%
  ungroup()

write_csv(daily_salvage,"data_raw/delta_loss.csv")

### Abundance --------------------------


