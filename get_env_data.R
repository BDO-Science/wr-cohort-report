library(dataRetrieval)
library(sharpshootR)
library(lubridate)
library(dplyr)
library(readr)
source("functions.R")
source("parameters.R")

# Shasta Dam ------------------------------------
storage_sha <- CDECquery(id = "SHA",sensor = 15, interval = "D", start = start,end = end)
storage_sha_df <- clean_cdec(df = storage_sha, param_name = "storage")
storage_sha_daily_years <- f_daily_10year(df = storage_sha_df, colName = storage)
storage_sha_plot_data <- f_convert_mean_long(df = storage_sha_daily_years, param_name = "Storage")

saveRDS(storage_sha_df, paste0("data_raw/storage_sha_df_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(storage_sha_daily_years, paste0("data_raw/storage_sha_daily_years_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(storage_sha_plot_data, paste0("data_raw/storage_sha_plot_data_", year(start), "-", year(end), ".rds"))

# Flow (USGS, CDEC) -----------------------------------

## Adult  --------------

## Only needs to be done once, then these are saved.
# flow_kwk <- f_get_NWIS_flow(siteNumbers=11370500, parameterCd = c('00060'), startDate = start, endDate = end)
# flow_bnd <- f_get_NWIS_flow(siteNumbers=11377100, parameterCd = c('00060'), startDate = start, endDate = end)

event_flow_kwk <- readRDS("data_raw/USGS_NWIS_11370500_2011_2021_flow.rds") %>%
  mutate(station = "KWK")
event_flow_bnd <- readRDS("data_raw/USGS_NWIS_11377100_2011_2021_flow.rds") %>%
  mutate(station = "BND")
flow_kwk_bnd_df <- rbind(event_flow_kwk, event_flow_bnd) %>%
  mutate(month = month(date, label = TRUE, abbr=FALSE))

flow_kwk_bnd_monthly <- f_monthly(df = flow_kwk_bnd_df, colName = flow)
flow_kwk_bnd_daily_years <- f_daily_10year(flow_kwk_bnd_df, flow)
flow_kwk_bnd_plot_data <- f_convert_mean_long(df = flow_kwk_bnd_daily_years, param_name = "flow")

saveRDS(flow_kwk_bnd_df, paste0("data_raw/flow_kwk_bnd_df_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(flow_kwk_bnd_daily_years, paste0("data_raw/flow_kwk_bnd_daily_years_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(flow_kwk_bnd_monthly, paste0("data_raw/flow_kwk_bnd_monthly_", year(start), "-", year(end), ".rds"))
saveRDS(flow_kwk_bnd_plot_data, paste0("data_raw/flow_kwk_bnd_plot_data_", year(start), "-", year(end), ".rds"))

## Juvenile ---------------------------

### Mid-low ----------------------------
event_flow_hmc <- CDECquery(id = "HMC", sensor = 20, interval = "H",start = start,end = end2)
flow_hmc_df <- clean_cdec(df = event_flow_hmc, param_name = "flow") %>%
  rename(month_num = month)

## Only needs to be done once, then these are saved.
# flow_wlk <- f_get_NWIS_flow(siteNumbers=11390500, parameterCd = c('00060'), startDate = start, endDate = end2)
# flow_von <- f_get_NWIS_flow(siteNumbers=11425500, parameterCd = c('00060'), startDate = start, endDate = end2)

event_flow_wlk <- readRDS(paste0("data_raw/USGS_NWIS_11390500_", year(start), "_", year(end2), "_flow.rds")) %>%
  mutate(station = "WLK")
event_flow_von <- readRDS(paste0("data_raw/USGS_NWIS_11425500_", year(start), "_", year(end2), "_flow.rds")) %>%
  mutate(station = "VON")

flow_wlk_von_df <- rbind(event_flow_wlk, event_flow_von) %>%
  mutate(month_num = month(date)) %>%
  select(datetime, date, date2, month_num,wy, year, station, flow)

flow_hmc_wlk_von_df <- rbind(flow_hmc_df, flow_wlk_von_df) %>%
  mutate(month =month(date, label = TRUE, abbr  =FALSE))

flow_hmc_wlk_von_monthly <- f_monthly_extrayear(df = flow_hmc_wlk_von_df, colName = flow)
flow_hmc_wlk_von_daily_years <- f_daily_10year(flow_hmc_wlk_von_df, flow)
flow_hmc_wlk_von_plot_data <- f_convert_mean_long_extrayear(df = flow_hmc_wlk_von_daily_years, param_name = "flow")

saveRDS(flow_hmc_wlk_von_df, paste0("data_raw/flow_hmc_wlk_von_df_", year(start), "-", year(end2), ".rds", compress = "xz"))
saveRDS(flow_hmc_wlk_von_daily_years, paste0("data_raw/flow_hmc_wlk_von_daily_years_", year(start), "-", year(end2), ".rds", compress = "xz"))
saveRDS(flow_hmc_wlk_von_monthly, paste0("data_raw/flow_hmc_wlk_von_monthly_", year(start), "-", year(end2), ".rds"))
saveRDS(flow_hmc_wlk_von_plot_data, paste0("data_raw/flow_hmc_wlk_von_plot_data_", year(start), "-", year(end2), ".rds"))

### Delta  ---------------------------
#### FPT ------
flow_fpt <- f_get_NWIS_flow(siteNumbers=11447650, parameterCd = c('00060'), startDate = start, endDate = end2)
flow_fpt_df <- readRDS(paste0("data_raw/USGS_NWIS_11447650_", year(start), "_", year(end2), "_flow.rds")) %>%
  mutate(station = "FPT",
         month = month(date))

flow_fpt_monthly <- f_monthly_extrayear(df = flow_fpt_df, colName = flow)
flow_fpt_daily_years <- f_daily_10year(flow_fpt_df, flow)
flow_fpt_plot_data <- f_convert_mean_long_extrayear(df = flow_fpt_daily_years, param_name = "flow")

saveRDS(flow_fpt_df, paste0("data_raw/flow_fpt_df_", year(start), "-", year(end2), ".rds", compress = "xz"))
saveRDS(flow_fpt_daily_years, paste0("data_raw/flow_fpt_daily_years_", year(start), "-", year(end2), ".rds", compress = "xz"))
saveRDS(flow_fpt_monthly, paste0("data_raw/flow_fpt_monthly_", year(start), "-", year(end2), ".rds"))
saveRDS(flow_fpt_plot_data, paste0("data_raw/flow_fpt_plot_data_", year(start), "-", year(end2), ".rds"))

#### Outflow --------
outflow <- CDECquery(id = "DTO", sensor = 23,interval = "D", start, end2)
outflow_df <- clean_cdec(outflow, param_name = "Outflow")
outflow_daily <- f_daily_10year(outflow_df, Outflow)
outflow_monthly <- outflow_df %>%
  mutate(month = month(date, label = TRUE, abbr = FALSE),
         month_num = month(date),
         year = year(date)) %>%
  filter(year %in% c(report_year, report_year+1)) %>%
  filter((year == report_year & month_num %in% c(9, 10, 11, 12)) | (year == report_year+1 & month_num <7)) %>%
  ungroup() %>%
  select(-date2, -date) %>%
  distinct() %>%
  group_by(year, month, month_num, station) %>%
  summarize(mean = round(mean(Outflow, na.rm = TRUE),1),
            min = round(min(Outflow, na.rm = TRUE),1),
            max = round(max(Outflow, na.rm = TRUE),1)) %>%
  ungroup() %>%
  arrange(station, year, month)
outflow_long <- f_convert_mean_long_extrayear(outflow_daily, "Outflow")

endDate = max(outflow_df$date)
saveRDS(outflow_df, paste0("data_raw/outflow_df_", year(start), "-", year(end2), ".rds", compress = "xz"))
saveRDS(outflow_daily, paste0("data_raw/outflow_daily_years_", year(start), "-", year(endDate), ".rds", compress = "xz"))
saveRDS(outflow_monthly, paste0("data_raw/outflow_monthly_", year(start), "-", year(endDate), ".rds"))
saveRDS(outflow_long, paste0("data_raw/outflow_plot_data_", year(start), "-", year(endDate), ".rds"))


#### OMR -------------------
omr <- CDECquery(id = "OMR",sensor = 20, interval = "H",start = start,end = end2)
  omr_df <- clean_cdec(df = omr, param_name = "omr")
  omr_daily_years <- f_daily_10year(df = omr_df, colName = omr)
  omr_monthly <- f_monthly_extrayear(df = omr_df, colName = omr)
  omr_plot_data <- f_convert_mean_long_extrayear(df = omr_daily_years, param_name = "omr")

  endDate = max(omr_df$date)
  statext = "OMR"
  saveRDS(omr_df, paste0("data_raw/omr_cdec_", statext,"_", year(start), "-", year(endDate), ".rds", compress = "xz"))
  saveRDS(omr_daily_years, paste0("data_raw/omr_", statext, "_daily_years_", year(start), "-", year(endDate), ".rds", compress = "xz"))
  saveRDS(omr_monthly, paste0("data_raw/omr_", statext, "_monthly_", year(start), "-", year(endDate), ".rds"))
  saveRDS(omr_plot_data, paste0("data_raw/omr_", statext, "_plot_data_", year(start), "-", year(endDate), ".rds"))

# DO (CDEC) -----------------------------

## Adult ----------------------------------
stations_do <- c("KWK", "CCR")
sta_text_adult <- "KWK_CCR"

do_KWK_CCR <- lapply(stations_do,
              function(x){CDECquery(id = x,sensor = 61, interval = "H",start = start,end = end)})

f_do_data(df = do_KWK_CCR, statext = sta_text_adult, sta = "adult")

## Juvenile -----------------

### Mid-Lower ------------------------
sta_do_ml <- c("RDB","SRH")
sta_text_do_ml <- "RDB_SRH"

do_raw_ml <- lapply(sta_do_ml,
                     function(x){CDECquery(id = x, sensor = 61, interval = "H",start = start,end = end2)})
f_do_data(df = do_raw_ml, sta_text_do_ml, "ml")

### Delta -------------------------
library(CDECRetrieve)
blp <- cdec_query("BLP", sensor_num = 61, dur_code = "E", start = start, end = end2)
do_raw_blp <- convert_event_data(df = blp)
sxs <- cdec_query("SXS", sensor_num = 61, dur_code = "E", start = sxs_start, end = end2)
do_raw_sxs <- convert_event_data(df = sxs)
do_raw_delta_e <- rbind(do_raw_blp, do_raw_sxs)
# do_raw_delta_e <- CDECquery(id = "BLP", sensor = 61, interval = "e",  start = start, end = end2)

sta_do_delta_h <- c("SRH", "MAL")
do_raw_delta_h <- lapply(sta_do_delta_h,
                 function(x){CDECquery(id = x, sensor = 61, interval = "H",start = start,end = end2)})

do_raw_delta_h2 <- bind_rows(do_raw_delta_h)
do_raw_delta <- rbind(do_raw_delta_e, do_raw_delta_h2) %>% select(-month)

sta_text_do_delta <- "SRH_SXS_BLP_MAL"
f_do_data(do_raw_delta, sta_text_do_delta, "ml")

# Turbidity (CDEC) -----------------------------

## Egg to Fry --------------------------------

sta_turb_ef <- c("KWK", "CCR")
sta_text_turb_ef <- "KWK_CCR"
turb_raw_ef <- lapply(sta_turb_ef,
               function(x){CDECquery(id = x, sensor = 27, interval = "H", start = start, end = end)})

f_turb_data(turb_raw_ef, sta_text_turb_ef, "ef")

## Juvenile --------------------------------------------

### Mid-low -------------------------------
sta_turb_ml <- c("RDB", "FPT")
sta_text_turb_ml <- "RDB_FPT"
turb_raw_FPT <- CDECquery(id = "FPT", sensor = 221, interval = "H", start = start, end = end2) %>%
  bind_rows()
turb_raw_RDB <- CDECquery(id = "RDB", sensor = 61, interval = "H", start = start, end = end2) %>%
  bind_rows()
turb_raw_ml <- rbind(turb_raw_FPT, turb_raw_RDB)

f_turb_data_test(turb_raw_ml, sta_text_turb_ml, "ml")
# Water Temperature in F-------------------------------------------------------------------------

## Adult -----------------------------------------------
stations_wtemp_adult <- c("CCR", "BSF")
sta_text_wtemp_adult <- "CCR_BSF"
wtemp_raw_adult <- lapply(stations_wtemp_adult,
               function(x){CDECquery(id = x, sensor = 25, interval = "H", start = start, end = end)})
f_wtemp_data(wtemp_raw_adult, sta_text_wtemp_adult, "adult")

## Juvenile ---------------------------

### Mid-low ----------------------------------
sta_text_wtemp_ml <- "WLK"

library(CDECRetrieve)
wlk <- cdec_query("WLK", sensor_num = 25, dur_code = "E", start = wlk_start, end = end2)
wtemp_raw_ml <- convert_event_data(df = wlk)

f_wtemp_data(wtemp_raw_ml, sta_text_wtemp_ml, "ml")


### Delta ------------------------------------
stations_wtemp_delta_F <- c("FPT", "GSS", "MAL")
stations_wtemp_delta_C <- c("SUS", "SWE")

wtemp_raw_delta_C <- lapply(stations_wtemp_delta,
                              function(x){cdec_query(station = x, sensor_num = 146, dur_code = "E", start = start, end = end2)})
wtemp_raw_delta_C2 <- wtemp_raw_delta_C %>% bind_rows() %>%
  mutate(parameter_value = parameter_value * 9/5 + 32)
wtemp_raw_delta_F <- lapply(stations_wtemp_delta,
                          function(x){cdec_query(station = x, sensor_num = 25, dur_code = "E", start = start, end = end2)}) %>%
bind_rows()

wtemp_raw_delta <- rbind(wtemp_raw_delta_C2, wtemp_raw_delta_F) %>%
  filter(!is.na(parameter_value),
         !is.na(location_id)) %>%
  convert_event_data()

sta_text_wtemp_delta <- "FPT_SUS_SWE_GSS_MAL"

f_wtemp_data(wtemp_raw_delta, sta_text_wtemp_delta, "delta")

# Probably don't need this but if we need to also include a wet year, would do it this way
# temp_years <- temp_daily %>%
#   mutate(fyear = factor(year(date))) %>%
#   dplyr::select(station, date2, Temperature = mean_temp, fyear)
#
# temp_avg <- temp_daily %>%
#   rename(Temperature = mean_10year) %>%
#   mutate(fyear = "mean") %>%
#   select(station, date2, Temperature, fyear)
#
# temp_all <- rbind(temp_years, temp_avg) %>%
#   filter(fyear %in% c("mean", report_year, 2017)) %>%
#   rename(doy = date2) %>%
#   mutate(year_type = case_when(fyear == "mean" ~ paste0(report_year-10, "-", report_year, " mean"),
#                                TRUE~ fyear))


##
