library(dataRetrieval)
library(sharpshootR)
library(lubridate)
library(dplyr)
library(readr)
source("functions.R")
source("parameters.R")

# Shasta Dam ------------------------------------
storage_sha <- CDECquery(id = "SHA",sensor = 15, interval = "D",start = start,end = end)
storage_sha_df <- clean_cdec(df = storage_sha, param_name = "storage")
storage_sha_daily_years <- f_daily_10year(df = storage_sha_df, colName = storage)
storage_sha_plot_data <- f_convert_mean_long(df = storage_sha_daily_years, param_name = "Storage")

saveRDS(storage_sha_df, paste0("data_raw/storage_sha_df_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(storage_sha_daily_years, paste0("data_raw/storage_sha_daily_years_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(storage_sha_plot_data, paste0("data_raw/storage_sha_plot_data_", year(start), "-", year(end), ".rds"))

# Flow (USGS) -----------------------------------

## Adult  --------------

## Only needs to be done once, then these are saved.
# flow_kwk <- f_get_NWIS_flow(siteNumbers=11370500, parameterCd = c('00060'), startDate = start, endDate = end)
# flow_bnd <- f_get_NWIS_flow(siteNumbers=11377100, parameterCd = c('00060'), startDate = start, endDate = end)

event_flow_kwk <- readRDS("data_raw/USGS_NWIS_11370500_2011_2021_flow.rds") %>%
  mutate(station = "KWK")
event_flow_bnd <- readRDS("data_raw/USGS_NWIS_11377100_2011_2021_flow.rds") %>%
  mutate(station = "BND")
flow_kwk_bnd_df <- rbind(event_flow_kwk, event_flow_bnd)

flow_kwk_bnd_monthly <- f_monthly(df = flow_kwk_bnd_df, colName = flow)
flow_kwk_bnd_daily_years <- f_daily_10year(flow_kwk_bnd_df, flow)
flow_kwk_bnd_plot_data <- f_convert_mean_long(df = flow_kwk_bnd_daily_years, param_name = "flow")

saveRDS(flow_kwk_bnd_df, paste0("data_raw/flow_kwk_bnd_df_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(flow_kwk_bnd_daily_years, paste0("data_raw/flow_kwk_bnd_daily_years_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(flow_kwk_bnd_monthly, paste0("data_raw/flow_kwk_bnd_monthly_", year(start), "-", year(end), ".rds"))
saveRDS(flow_kwk_bnd_plot_data, paste0("data_raw/flow_kwk_bnd_plot_data_", year(start), "-", year(end), ".rds"))


# DO (CDEC) -----------------------------

## Adult ----------------------------------
stations_do <- c("KWK", "CCR")

do_KWK_CCR <- lapply(stations_do,
              function(x){CDECquery(id = x,sensor = 61, interval = "H",start = start,end = end)})

do_KWK_CCR_df <- clean_cdec(df = do_KWK_CCR, param_name = "DO") %>%
  filter(DO<30 & DO > 0)
do_KWK_CCR_monthly <- f_monthly(df = do_KWK_CCR_df, colName = DO)
do_KWK_CCR_daily_years <- f_daily_10year(df = do_KWK_CCR_df, colName = DO)
do_KWK_CCR_plot_data <- f_convert_mean_long(df = do_KWK_CCR_daily_years, param_name = "DO")

saveRDS(do_KWK_CCR_df, paste0("data_raw/do_cdec_KWK_CCR_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(do_KWK_CCR_daily_years, paste0("data_raw/do_KWK_CCR_daily_years_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(do_KWK_CCR_monthly, paste0("data_raw/do_KWK_CCR_monthly_", year(start), "-", year(end), ".rds"))
saveRDS(do_KWK_CCR_plot_data, paste0("data_raw/do_KWK_CCR_plot_data_", year(start), "-", year(end), ".rds"))

# Turbidity (CDEC) -----------------------------

## Egg to Fry --------------------------------

stations_turb <- c("KWK", "CCR")
turb_KWK_CCR <- lapply(stations_turb,
               function(x){CDECquery(id = x, sensor = 27, interval = "H", start = start, end = end)})

turb_KWK_CCR_df <- clean_cdec(df = turb_KWK_CCR, param_name = "Turbidity") %>%
  filter(Turbidity>0)
turb_KWK_CCR_monthly <- f_monthly(df = turb_KWK_CCR_df, colName = Turbidity)
turb_KWK_CCR_daily_years <- f_daily_10year(df=turb_KWK_CCR_df, colName=Turbidity)
turb_KWK_CCR_plot_data <- f_convert_mean_long(df = turb_KWK_CCR_daily_years, param_name = "Turbidity")


saveRDS(turb_KWK_CCR_df, paste0("data_raw/turb_KWK_CCR_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(turb_KWK_CCR_daily_years, paste0("data_raw/turb_KWK_CCR_daily_years_", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(turb_KWK_CCR_plot_data, paste0("data_raw/turb_KWK_CCR_plot_data_", year(start), "-", year(end), ".rds"))
saveRDS(turb_KWK_CCR_monthly, paste0("data_raw/turb_KWK_CCR_monthly_", year(start), "-", year(end), ".rds"))

# Water Temperature in F-------------------------------------------------------------------------

## Adult -----------------------------------------------
stations_temp <- c("CCR", "BSF")
temp_CCR_BSF <- lapply(stations_temp,
               function(x){CDECquery(id = x, sensor = 25, interval = "H", start = start, end = end)})
temp_CCR_BSF_df <- clean_cdec(df = temp_CCR_BSF, param_name = "WaterTemp") %>%
  filter(WaterTemp>20 & WaterTemp<100)
temp_CCR_BSF_monthly <- f_monthly(df = temp_CCR_BSF_df, colName = WaterTemp)
temp_CCR_BSF_daily_years <- f_daily_10year(df=temp_CCR_BSF_df, colName=WaterTemp)
temp_CCR_BSF_plot_data <- f_convert_mean_long(df = temp_CCR_BSF_daily_years, param_name = "WaterTemp")

saveRDS(temp_CCR_BSF_df, paste0("data_raw/temp_cdec_CCR_BSF", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(temp_CCR_BSF_daily_years, paste0("data_raw/temp_CCR_BSF_daily_years", year(start), "-", year(end), ".rds", compress = "xz"))
saveRDS(temp_CCR_BSF_plot_data, paste0("data_raw/temp_CCR_BSF_plot_data", year(start), "-", year(end), ".rds"))
saveRDS(temp_CCR_BSF_monthly, paste0("data_raw/temp_CCR_BSF_monthly", year(start), "-", year(end), ".rds"))


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
