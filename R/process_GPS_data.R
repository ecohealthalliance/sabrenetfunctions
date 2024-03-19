#' Process GPS Data -- Clean raw ingested GPS tracks from all bats
#'
#' @param GPS_data Ingested GPS data
#' @param outlier_pos Obvious outlier GPS data (previously hand-built data frame of min and max for long and lat (two columns, two rows))
#' @param outlier_date Drop data points before some date?
#' @param ping_time_test Duty cycle (in min) that we want to test. NA to ignore this argument
#' @param file_type Whether GPS was ingested via kml or txt
#' @param epd Errant Point Directory. Location where a file exists that lists additional points to be stripped from the GPS data
#'
#' @return list of data frame of cleaned GPS data and ping time
#' @export
process_GPS_data <- function (GPS_data, outlier_pos, outlier_date, ping_time_test, file_type, epd) {

## grab the errant points file
if (!is.null(epd)) {
  errant_points <- utils::read.csv(paste(epd, "errant_points.csv", sep = "/"))
}

## Two different file types can be loaded, processing a bit different (kml and txt)
if (file_type == "kml") {

GPS_data %<>%
  dplyr::mutate(date = apply(matrix(Name), 1, FUN = function(x) strsplit(x, "_")[[1]][2])) %>%
  dplyr::select(-Name) %>%
  dplyr::mutate(
    Date = apply(matrix(date), 1, FUN = function(x) strsplit(x, " ")[[1]][1])
  , time = apply(matrix(date), 1, FUN = function(x) strsplit(x, " ")[[1]][2])
  ) %>% dplyr::select(-date) %>%
  dplyr::rename(date = Date) %>%
  dplyr::mutate(
    h = apply(matrix(time), 1, FUN = function(x) {strsplit(x, "[:]")[[1]][1]} %>% as.numeric())
  , m = apply(matrix(time), 1, FUN = function(x) {strsplit(x, "[:]")[[1]][2]} %>% as.numeric())
  ) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(
    lag_m = dplyr::lag(m, 1)
  , dif_m = m - lag_m
  )

} else if (file_type == "txt") {

GPS_data %<>%
    ## convert : times to hour minute
    dplyr::mutate(
      h = apply(matrix(time), 1, FUN = function(x) {strsplit(x, "[:]")[[1]][1]} %>% as.numeric())
    , m = apply(matrix(time), 1, FUN = function(x) {strsplit(x, "[:]")[[1]][2]} %>% as.numeric())
    ) %>%
    dplyr::group_by(ID) %>%
    ## calculate time gap between consecutive points in minutes
    dplyr::mutate(
      lag_m = dplyr::lag(m, 1)
    , dif_m = m - lag_m
    )

} else {
  stop("File type not supported")
}

## Time between points overall reasonably calculated with mode
ping_time <- GPS_data %>% dplyr::pull(dif_m) %>% calc_mode(.)

GPS_data %<>%
  ## fractional hour
  dplyr::mutate(
    m     = m / 60
  , index = seq(dplyr::n())
  ) %>%
  dplyr::group_by(index) %>%
  ## hour + fractional hour
  dplyr::mutate(
    n_time = h + m
  , date   = as.Date(date)) %>%
  dplyr::ungroup(index) %>%
  ## drop columns
  dplyr::select(-c(index, h, m, lag_m, dif_m)) %>%
  ## remove testing period
  dplyr::filter(date >= outlier_date)

## identify points where gps location failed
GPS_data_unfiltered <- GPS_data %>% dplyr::mutate(zero_point = ifelse(long == 0, 1, 0))

GPS_data_info <- GPS_data %>%
  dplyr::group_by(ID, date, `Laboratory ID`, `GPS Attachment Date`) %>%
  ## proportion of points that were a success
  dplyr::summarize(
    n_entry = dplyr::n()
  , n_succ  = length(which(long != 0))
  ) %>%
  dplyr::mutate(
    prop_succ = n_succ / n_entry
  )

GPS_bat_info <- GPS_data %>%
  dplyr::group_by(ID) %>%
  ## max battery at start and battery at last point
  dplyr::mutate(bat = as.numeric(bat)) %>%
  dplyr::mutate(bat_start = max(bat)) %>%
  dplyr::filter(point_num == max(point_num)) %>%
  ## calculate battery used
  dplyr::mutate(
    bat_used = bat_start - bat
  , bat_left = bat - 3.4
  ) %>%
  dplyr::mutate(
    tot_bat  = bat_used + bat_left
  , prop_bat = bat_left / tot_bat
  ) %>%
  ## some go slightly negative (past turnoff point of battery because of lag)
  dplyr::mutate(prop_bat = ifelse(prop_bat < 0, 0, prop_bat))

## Clean up errant points for removal from the GPS data
 ## These are points of testing that slipped through the date filter, or clearly failed/wild
  ## GPS locations while the units were attached to the bat. These are stored in a simple csv and parsed here
errant_points.expanded <- errant_points[1, ][-1, ]
for (i in 1:nrow(errant_points)) {
  if (grep("[:]", errant_points[i, ]$point_num) %>% length() == 0) {
    errant_points.expanded <- rbind(errant_points.expanded, errant_points[i, ])
  } else {
    ttt   <- errant_points$point_num[i] %>% strsplit(":") %>% unlist()
    ttt.s <- seq(ttt[1], ttt[2])
    errant_points.expanded <- rbind(
      errant_points.expanded
    , data.frame(ID = rep(errant_points$ID[i], length(ttt.s)), point_num = ttt.s)
    )
  }
}
errant_points.expanded %<>% dplyr::mutate(drop_point = 1, point_num = as.numeric(point_num))

## Drop any extreme errant data points (which can happen with these units)
GPS_data %<>%
  dplyr::filter(
    long > outlier_pos$long[1]
  , long < outlier_pos$long[2]
  , lat  > outlier_pos$lat[1]
  , lat  < outlier_pos$lat[2]
  )

## Remove the custom cleaned "errant points" (see comment above)
GPS_data %<>% dplyr::left_join(., errant_points.expanded, by = c("ID", "point_num")) %>% dplyr::filter(is.na(drop_point))

GPS_data %<>%
  dplyr::group_by(ID) %>%
  ## steps to calculate gaps between consecutive success points and identifying transitions between "nights"
   ## which actually occurs betweeen 4am and 7:30pm on the same calendar day
  dplyr::mutate(
    lag_time     = dplyr::lag(n_time, 1)
  , lag_point    = dplyr::lag(point_num, 1)
  , time_diff    = n_time - lag_time
  , time_diff    = ifelse(is.na(time_diff), 0, time_diff)
  , point_diff   = point_num - lag_point
  , point_diff   = ifelse(is.na(point_diff), 0, point_diff)
  , over_day     = ifelse(time_diff > 5, 1, 0)
  )

## Quick check to make sure consecutive days are not labeled as the same "flight clust" (each nightly period)
 ## which can happen with certain stretches of failed data points
GPS_data %<>%
  dplyr::mutate(
    over_day     = ifelse(over_day == 0 & point_diff > 50, over_day + 1, over_day)
  , flight_clust = cumsum(over_day) + 1
  ) %>%
  dplyr::group_by(ID, flight_clust) %>%
  dplyr::mutate(point_order = seq(dplyr::n()))

## Stripping some points for testing, not relevant for main analysis
if (!is.na(ping_time_test)) {

  if (ping_time_test < ping_time) {
    stop(paste("GPS duty cycle has to be less than or equal to what was used in the
example data, which was", ping_time, sep = " = "))
  }

  prop_points_to_cut <- ping_time / ping_time_test

  GPS_data %<>% dplyr::mutate(drop_points = ifelse(((point_order * prop_points_to_cut) %% 1) != 0, 1, 0)) %>%
    dplyr::filter(drop_points == 0)

  ping_time <- ping_time_test

}

  return(
    list(
      gps_data            = GPS_data
    , gps_data_unfiltered = GPS_data_unfiltered
    , gps_data_info       = GPS_data_info
    , gps_bat_info        = GPS_bat_info
    , ping_time           = ping_time
    )
  )

}


#' Process GPS Data for Plotting -- Further cleaning and creating of additional fields for plotting
#'
#' @param GPS_data Data frame of cleaned GPS data
#' @param bin_dist Width in c(long, lat) to join nearby datapoints to count as time spent in the same spot
#' @param min_flight_dist Distance traveled between adjacent data points at which 'flight' is assigned
#' @param time_window Number of points on either side of a focal point not identified as 'flight' to characterize that point as 'feeding'
#' @param ping_time Number of minutes that elapse between GPS records
#'
#' @return data frame of stacked GPS tracks
#' @export
process_GPS_data_for_plotting <- function (GPS_data, bin_dist, min_flight_dist, time_window, ping_time) {

  ## Calculate distance between consecutive points mostly
  GPS_data %<>%
    dplyr::group_by(ID, flight_clust) %>%
    dplyr::mutate(
      long_lag = dplyr::lag(long, 1)
    , lat_lag  = dplyr::lag(lat, 1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(index = seq(dplyr::n())) %>%
    dplyr::group_by(index) %>%
    dplyr::mutate(dist = geodist::geodist(c(lon = long, lat = lat), c(long = long_lag, lat = lat_lag), quiet = TRUE)[, 1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index)

  ## binning close by data points
  GPS_data %<>%
    dplyr::mutate(
      date_p = as.character(date)
    , binned_long = plyr::round_any(long, bin_dist[1])
    , binned_lat  = plyr::round_any(lat, bin_dist[2])
    )

  ## Labeling stretches of points within a given "window" that are within a threshold distance apart as a bat feeding
   ## These are important parameters for consideration
  GPS_data %<>%
    dplyr::group_by(ID, flight_clust) %>%
    dplyr::mutate(
      flight      = ifelse(dist > min_flight_dist, 1, 0)
    , prop_flight = zoo::rollapply(flight, width = (time_window * 2 + 1), fill = NA, FUN = mean, align = "center")
    , time_flight = prop_flight * ping_time * (time_window * 2 + 1)
    , feeding     = ifelse(prop_flight == 0, 1, 0)
    )

 return(GPS_data)

}
