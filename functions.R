library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(cvpiaHabitat)
library(cvpiaFlow)
library(sharpshootR)

library(rvest)
#Function adjusted from Trinh Nguyen's code to pull salvage datasets from SacPAS
pull_salvage <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]

  df <- lapply(startingForm$fields$year$options, function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "26:all") # This part tells it what species

    submittedFormURL <- suppressMessages(submit_form(session = startingSession,
                                                     form = filledForm, POST = salvageURL)$url)

    csvLink <- submittedFormURL

    if (length(csvLink) == 0) {
      return(NULL)
    } else {
      csvDownload <- csvLink
    }

    df <- csvDownload %>%
      read_csv()  }) %>%
    bind_rows()
  df
}

# Get NWIS Flow -------------------------------------------------------------------
# Default is Vernalis
f_get_NWIS_flow <- function(siteNumbers=11303500, parameterCd = c('00060'), startDate = start, endDate = end, tz = "Etc/GMT+8"){

  # get data
  print("Downloading data...")
  data <- dataRetrieval::readNWISuv(siteNumbers, parameterCd, startDate, endDate, tz)

  # fix names

  data2 <- dataRetrieval::renameNWISColumns(data)

  print("Data downloaded!")

  # clean names
  data2 <- janitor::clean_names(data2)%>%
    mutate(date = date(date_time),
           date2 = ymd(paste0("1980-", month(date), "-", day(date))),
           year = year(date),
           fyear = factor(year))

  # write out
  saveRDS(data2, paste0("data_raw/USGS_NWIS_", siteNumbers, "_flow.rds"))

  # print message!
  print("Data saved in data_raw")
}

# Clean CDEC -------------------------------------------------------------------

clean_cdec <- function(cdec_df)

 data <-cdec_df %>%
  bind_rows() %>%
  mutate(datetime = as.POSIXct(datetime, tz = "America/Los_Angeles", format = "%Y-%m-%d %H:%M:%S"),
         date = lubridate::date(datetime),
         year = lubridate::year(date),
         month = as.numeric(month),
         date2 = ymd(paste0("1980-", month, "-", lubridate::day(date)))) %>%
  select(datetime,date, date2, month, wy = water_year, year, station = station_id, parameter=value) %>%
  filter(!is.na(station),
         !is.na(date))









# CVPIA ---------------------------------------------------------


#  https://flowwest.github.io/cvpiaData/articles/create-hab-inputs.html

# returns flow for each month of a watershed during simulation window
get_flow <- function(watershed, years=c(1980, 1999)) {

  # get the flow values at the dates
  dplyr::pull(dplyr::filter(dplyr::select(cvpiaFlow::flows_cfs, date, watershed),
                            lubridate::year(date) >= years[1],
                            lubridate::year(date) <= years[2]), 2)
}

# transforms to array data structure for SIT model input, [watersheds, months, years]
create_SIT_array <- function(input) {

  output <- array(NA, dim = c(nrow(input), 12, ncol(input) / 12))
  index <-  1
  for (i in seq(1, ncol(input), 12)) {
    output[ , , index] <- as.matrix(input[ , i:(i + 11)])
    index <- index + 1
  }
  return(output)

}


get_spawn_hab_all <- function(species) {

  watersheds <- cvpiaHabitat::modeling_exist %>%
    dplyr::filter(!is.na(FR_spawn), Watershed != 'Upper Sacramento River', Watershed != 'Upper Mid Sac Region') %>%
    dplyr::pull(Watershed)

  most <- purrr::map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, years=c(1979, 1999))
    habitat <- cvpiaHabitat::set_spawning_habitat(watershed,
                                                  species = species,
                                                  flow = flows)
    tibble(
      year = rep(1979:1999, each = 12),
      month = rep(1:12, 21),
      watershed = watershed,
      hab_sq_m = habitat)
  })

  # deal with sacramento special cases
  # upper sac
  up_sac_flows <- get_flow('Upper Sacramento River', years=c(1979, 1999))
  months <- rep(1:12, 21)
  up_sac_hab <- purrr::map2_dbl(months, up_sac_flows, function(month, flow) {
    cvpiaHabitat::set_spawning_habitat('Upper Sacramento River',
                                       species = species,
                                       flow = flow, month = month)
  })

  up_sac <- tibble(
    year = rep(1979:1999, each = 12),
    month = rep(1:12, 21),
    watershed = 'Upper Sacramento River',
    hab_sq_m = up_sac_hab)

  hab <-   dplyr::bind_rows(most, up_sac) %>%
    tidyr::spread(watershed, hab_sq_m) %>%
    dplyr::bind_cols(tibble(`Sutter Bypass` = rep(NA, 252),
                            `Yolo Bypass` = rep(NA, 252),
                            `Upper-mid Sacramento River` = rep(NA, 252),
                            `Lower-mid Sacramento River` = rep(NA, 252),
                            `Lower Sacramento River` = rep(NA, 252),
                            `San Joaquin River` = rep(NA, 252))) %>%
    tidyr::gather(watershed, habitat, -year, -month) %>%
    dplyr::mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    dplyr::select(date, watershed, habitat) %>%
    tidyr::spread(date, habitat) %>%
    dplyr::left_join(cvpiaData::watershed_ordering) %>%
    dplyr::arrange(order) %>%
    dplyr::select(-watershed, -order) %>%
    create_SIT_array()

  return(hab)
}

get_floodplain_hab_all <- function(watersheds, species) {

  watersheds_fp <- cvpiaData::watershed_ordering %>%
    dplyr::filter(!(watershed  %in% c('Sutter Bypass','Yolo Bypass',
                                      'Lower-mid Sacramento River', 'Upper Sacramento River',
                                      'Upper-mid Sacramento River', 'Lower Sacramento River'))) %>%
    dplyr::pull(watershed)

  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed)
    habitat <- cvpiaHabitat::acres_to_square_meters(
      cvpiaHabitat::set_floodplain_habitat(watershed, species, flows))

    tibble(
      year = rep(1980:1999, each = 12),
      month = rep(1:12, 20),
      watershed = watershed,
      hab_sq_m = habitat)
  })

  # deal with sac, already in square meters
  # upper sac
  up_sac_flow <- get_flow('Upper Sacramento River')
  up_mid_sac_flow <- get_flow('Upper-mid Sacramento River')
  low_sac_flow <- get_flow('Lower Sacramento River')

  up_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Upper Sacramento River', species, up_sac_flow)
  up_mid_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Upper-mid Sacramento River', species, up_mid_sac_flow)
  low_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Lower Sacramento River', species, low_sac_flow)

  # lower-mid sacramento
  low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1")
  low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2")
  low_mid_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Lower-mid Sacramento River', species,
                                                         low_mid_sac_flows1, flow2 = low_mid_sac_flows2)
  sac <- tibble(
    year = rep(rep(1980:1999, each = 12), times = 4),
    month = rep(1:12, 80),
    watershed = rep(c('Upper Sacramento River', 'Upper-mid Sacramento River',
                      'Lower-mid Sacramento River', 'Lower Sacramento River'), each = 240),
    hab_sq_m = c(up_sac_fp, up_mid_sac_fp, low_mid_sac_fp, low_sac_fp))

  hab <- bind_rows(most, sac) %>%
    spread(watershed, hab_sq_m) %>%
    bind_cols(tibble(`Sutter Bypass` = rep(NA, 240),
                     `Yolo Bypass` = rep(NA, 240))) %>%
    gather(watershed, habitat, -year, -month) %>%
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>%
    select(date, watershed, habitat) %>%
    spread(date, habitat) %>%
    left_join(cvpiaData::watershed_ordering) %>%
    arrange(order) %>%
    select(-watershed, -order) %>%
    create_SIT_array()

  return(hab)

}
