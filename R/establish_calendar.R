#' Set up a calendar for Matlapitsi Rousettus for use in figures.
#'
#' @return Data frame with date identifiers for figure creation to highlight specific timing of Rousettus life history at Matlapitsi
#' @export
establish_calendar <- function() {

Date <- as.POSIXlt(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 days"))

Calendar <- data.frame(
  Date
, Day            = Date$mday
, NameDay        = weekdays(Date)
, Month          = Date$mon + 1
, NameMonth      = month.name[Date$mon + 1]
, NameMonthShort = month.abb[Date$mon + 1]
, Week           = strftime(Date, "%V")
, Year           = 1900 + Date$year
, Quarter        = quarters(Date)
, Season         = c(
  rep("Summer", seq(as.Date("2020-01-01"), as.Date("2020-02-14"), by="1 days") %>% length())
, rep("Fall"  , seq(as.Date("2020-02-15"), as.Date("2020-05-14"), by="1 days") %>% length())
, rep("Winter", seq(as.Date("2020-05-15"), as.Date("2020-08-14"), by="1 days") %>% length())
, rep("Spring", seq(as.Date("2020-08-15"), as.Date("2020-11-14"), by="1 days") %>% length())
, rep("Summer", seq(as.Date("2020-11-15"), as.Date("2020-12-31"), by="1 days") %>% length())
)
) %>% dplyr::mutate(DOY = seq(dplyr::n()))

bat_lh <- data.frame(
  Day       = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 days")
) %>% dplyr::mutate(
  Breeding  = ifelse(Day >= "2020-05-15" & Day <= "2020-08-15", 1, 0)
, Gestation = ifelse(Day >= "2020-06-01" & Day <= "2020-11-30", 1, 0)
, Births    = ifelse(
  (Day >= "2020-10-01" & Day <= "2020-12-31") |
  (Day >= "2020-01-01" & Day <= "2020-02-01"), 1, 0)
, Weaning   = ifelse(Day >= "2020-02-01" & Day <= "2020-04-15", 1, 0)
, SA_Abund  = ifelse(Day >= "2020-04-01" & Day <= "2020-10-01", 1, 0)
) %>% dplyr::rename(Date = Day)

Calendar %<>% dplyr::left_join(., bat_lh, by = "Date")

}
