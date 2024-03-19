#' Download GPS files from Airtable Base
#'
#' @param base String. Base ID
#' @param table_name String. Table nanme
#' @param field List. Fields to be included in extract
#' @param dir_name Location to temporarily store sequence file for processing
#'
#' @return data frame of stacked GPS tracks
#' @export
get_GPS_data_airtable  <- function (
    base       = "appi9YHtonEpMFuAz"
  , table_name = "GPS Data"
  , field      = "GPS Data Processed"
  , dir_name   = "data/temp_raw_gps_data"
  ) {

## First pull the whole table as this is needed for a summary of GPS effectiveness
gps_data <- airtabler::fetch_all(base = base, table_name = table_name) %>%
  dplyr::mutate(index = seq(dplyr::n())) %>%
  split_tibble("index")

## Next pull each of the GPS files
gps_data <- airtabler::fetch_all(base = base, table_name = table_name) %>%
  dplyr::mutate(index = seq(dplyr::n())) %>%
  split_tibble("index")

x      <- airtabler::fetch_all(base, table_name)
xfield <- purrr::pluck(x, field)

xlist <- purrr::map(xfield, function(x) {
  if (is.null(x$url)) {
    ID <- x$id
    warning(sprintf("Record ID %s is null", ID))
    return(NULL)
  }

  apply(x$url %>% matrix(), 1, FUN = function(y) airtabler::read_excel_url(y)) %>% do.call("rbind", .)

})

gps_combined <- purrr::pmap(list(xlist, gps_data), .f = function(x, y) {

  if (is.null(x)) {
    return(x)
  } else {

  x %<>%
    dplyr::mutate(
      `GPS ID`              = y$`GPS ID`
    , `GPS Attachment Date` = y$`GPS Attachment Date`
    , `Laboratory ID`       = y$`Laboratory ID`) %>%
    dplyr::relocate(c(`Laboratory ID`, `GPS ID`, `GPS Attachment Date`))

  x %<>%
    dplyr::mutate(TTF = as.numeric(TTF)) %>%
    dplyr::filter(!is.na(TTF)) %>%
    dplyr::select(
       `GPS ID`, `Laboratory ID`, `GPS Attachment Date`
      , ID, Date, Time, Long, Latt, Altitude
      , Satt, Maxsnr, V1) %>%
    dplyr::rename(
        point_num = ID, ID = `GPS ID`
      , long = Long, lat = Latt, elev = Altitude
      , date = Date, time = Time
      , sat  = Satt, sig = Maxsnr, bat = V1)

  if (nrow(x > 0)) {
    x %<>%
      dplyr::mutate(bat = apply(bat %>% matrix(), 1, FUN = function(y) strsplit(y, "V")[[1]][1])) %>%
      dplyr::mutate(index = seq(dplyr::n())) %>%
      dplyr::group_by(index) %>%
      dplyr::mutate(
        long = dms_to_deg(long)
      , lat  = dms_to_deg(lat)
      ) %>%
      dplyr::ungroup(index) %>%
      dplyr::select(-index)
  }

  return(x)

  }

}) %>% do.call("rbind", .)

## In a few cases the very last data point was retrieved by both base stations for some reason
gps_combined %<>% dplyr::distinct()

## Order by point_num for visualization in advance of further cleaning
gps_combined %<>% dplyr::group_by(ID) %>% dplyr::arrange(point_num)

return(gps_combined)

}
