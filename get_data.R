library(dataRetrieval)
library(sharpshootR)
library(lubridate)
library(dplyr)
source("functions.R")

start = as.Date("2017-01-01")
end = today()-60

# Keswick Flow (USGS) -----------------------------------
flow_kwk <- f_get_NWIS_flow(siteNumbers=11370500, parameterCd = c('00060'))


# Keswick and Clear Creek DO (CDEC) -----------------------------
stations <- c("KWK", "CCR")

do <- lapply(stations,
              function(x){CDECquery(id = x,sensor = 61, interval = "H",start = start,end = end)})

do_df <- clean_cdec(cdec_df = do) %>%
  rename(DO = parameter) %>%
  filter(DO<30 & DO > 0)

saveRDS(do_df, paste0("data_raw/do_cdec_", year(start), "-", year(end), ".rds", compress = "xz"))

# Keswick and Clear Creek Turbidity (CDEC) -----------------------------
turb <- lapply(stations,
               function(x){CDECquery(id = x, sensor = 27, interval = "H", start = start, end = end)})

turb_df <- clean_cdec(cdec_df = turb) %>%
  rename(Turbidity = parameter) %>%
  filter(Turbidity>0)

saveRDS(turb_df, paste0("data_raw/turbidity_cdec_", year(start), "-", year(end), ".rds", compress = "xz"))

# Water Temperature at CCR and BSF -------------------------------------------------------------------------
stations_temp <- c("CCR", "BSF")
start_temp = as.Date("2011-01-01")

temp <- lapply(stations_temp,
               function(x){CDECquery(id = x, sensor = 25, interval = "H", start = start_temp, end = end)})

temp_df <- clean_cdec(cdec_df = temp) %>%
  rename(Temperature = parameter) %>%
  filter(Temperature>20 & Temperature<150)

saveRDS(temp_df, paste0("data_raw/temp_cdec_", year(start_temp), "-", year(end), ".rds", compress = "xz"))
