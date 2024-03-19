#' Summarize Virus Data
#'
#' @param bat_data Data frame. Sample data
#' @param site_data Data frame. Site info
#' @param specimen_data Data frame. Specimen data
#' @param sequence_data Data frame. Sequences (all the already cleaned ones in the Airtable table "Sequences")
#' @param base_ids list of two Airtable base IDs
#'
#' @return List of two data frames (specimen data and sample data)
#' @export
summarize_virus_data <- function(bat_data, site_data, specimen_data, sequence_data, base_ids) {

  `%notin%` <- Negate(`%in%`)

  ## unlist, respecting NULL entries for left_joining further down
  specimen_data %<>%
    dplyr::mutate(
      `Sample Type`   = lapply(`Sample Type`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Laboratory ID` = lapply(`Laboratory ID`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    ## NOTE: Need to get at the root of this problem, I do not know why just Corona Year Tested is a list and other
     ## "year tested" columns are not
    , `Corona Year tested` = lapply(`Corona Year tested`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
  )

  ## Due to somewhat different database structures regarding how visit information is spread between Site Data and Event Data,
   ## have to, at least for now, split up and do something a bit different to parse each and stick back together
    ## !! Would be really nice to change the database structure to match, but it seems that this either difficult or low priority
  site_data.s  <- site_data %>% dplyr::filter(base_id == base_ids[[1]])
  site_data.b  <- site_data %>% dplyr::filter(base_id == base_ids[[2]])
  event_data.s <- event_data %>% dplyr::filter(base_id == base_ids[[1]])
  event_data.b <- event_data %>% dplyr::filter(base_id == base_ids[[2]])

  bat_data.s <- bat_data %>%
    dplyr::filter(base_id == base_ids[[1]]) %>%
    ## drop the lists for species (while retaining nulls) and site data
    dplyr::mutate(
      `Common name`                     = lapply(`Common name`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Scientific name`                 = lapply(`Scientific name`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Animal type`                     = lapply(`Animal type`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Species in Environment`          = lapply(`Species in Environment`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Common name (Environmental)`     = lapply(`Common name (Environmental)`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Scientific name (Environmental)` = lapply(`Scientific name (Environmental)`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Animal type (Environmental)`     = lapply(`Animal type (Environmental)`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , site_data                         = lapply(`Site Data`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()) %>%
    dplyr::left_join(., site_data.s %>% dplyr::select(-createdTime) %>% dplyr::rename(site_data = id), by = c("site_data", "base_id"))  %>%
    dplyr::mutate(
      Site_Name = apply(Site_ID %>% matrix(), 1, FUN = function(x) strsplit(x, "_")[[1]][1])
    , Year     = sapply(Date, FUN = function(x) strsplit(x, "-")[[1]][1]) %>% as.numeric()
    , Month    = sapply(Date, FUN = function(x) strsplit(x, "-")[[1]][2]) %>% as.numeric()
    , Day      = sapply(Date, FUN = function(x) strsplit(x, "-")[[1]][3]) %>% as.numeric()
    , Date     = as.Date(Date)
    , DOY      = lubridate::yday(Date)
    ) %>%
    dplyr::select(
    ## Critical species-level stuff
      id, Laboratory_ID, Year, Month, DOY, Day, Date, Site_Name, `Specimen type`
    , `Animal type`,  `Common name`, `Scientific name`
    , `Animal type (Environmental)`,  `Common name (Environmental)`, `Scientific name (Environmental)`
    , `Species in Environment`, Recapture, `Unique Individual`
    ## Individual-level characteristics
    , Sex, Age, `Reproductive status`, `Nipple condition`, `Forearm length (mm)`, `Final mass (g)`
    ## Keep Laboratory ID around to identify environmental samples, but also need Laboratory ID to match with
    ## Specimen Data for joining
    ) %>%
    dplyr::rename(`Laboratory ID` = id) %>%
    dplyr::mutate(
      ## May have caught all of these by now, but doesnt hurt to double check
      Age = plyr::mapvalues(Age, from = "Adult ", to = "Adult")
    , Sex = plyr::mapvalues(Sex, from = c("Male ", "Female "), to = c("Male", "Female"))
    )

  event_data.b %<>%
    dplyr::mutate(`Site Data` = lapply(`Site Data`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()) %>%
    dplyr::left_join(site_data.b %>% dplyr::select(-createdTime) %>% dplyr::rename(`Site Data` = id)) %>%
    dplyr::select(-createdTime, -`Site Data`) %>%
    dplyr::rename(site_data = id)

  bat_data.b <- bat_data %>%
    dplyr::filter(base_id == base_ids[[2]]) %>%
    ## drop the lists for species (while retaining nulls) and site data
    dplyr::mutate(
      `Common name`                     = lapply(`Common name`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Scientific name`                 = lapply(`Scientific name`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Animal type`                     = lapply(`Animal type`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Species in Environment`          = lapply(`Species in Environment`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Common name (Environmental)`     = lapply(`Common name (Environmental)`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Scientific name (Environmental)` = lapply(`Scientific name (Environmental)`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , `Animal type (Environmental)`     = lapply(`Animal type (Environmental)`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , site_data                         = lapply(`Site Data`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()) %>%
    dplyr::left_join(., event_data.b, by = c("site_data", "base_id")) %>%
    dplyr::mutate(
      Year  = sapply(Date, FUN = function(x) strsplit(x, "-")[[1]][1]) %>% as.numeric()
    , Month = sapply(Date, FUN = function(x) strsplit(x, "-")[[1]][2]) %>% as.numeric()
    , Day   = sapply(Date, FUN = function(x) strsplit(x, "-")[[1]][3]) %>% as.numeric()
    , Date  = as.Date(Date)
    , DOY   = lubridate::yday(Date)
    ) %>%
    dplyr::rename(Site_Name = Site_ID) %>%
    dplyr::select(
    ## Critical species-level stuff
      id, Laboratory_ID, Year, Month, DOY, Day, Date, Site_Name, `Specimen type`
    , `Animal type`,  `Common name`, `Scientific name`
    , `Animal type (Environmental)`,  `Common name (Environmental)`, `Scientific name (Environmental)`
    , `Species in Environment`, Recapture, `Unique Individual`
    ## Individual-level characteristics
    , Sex, Age, `Reproductive status`, `Nipple condition`, `Forearm length (mm)`, `Final mass (g)`
    ## Keep Laboratory ID around to identify environmental samples, but also need Laboratory ID to match with
     ## Specimen Data for joining
    ) %>%
    dplyr::rename(`Laboratory ID` = id) %>%
    dplyr::mutate(
      Age = plyr::mapvalues(Age, from = "Adult ", to = "Adult")
    , Sex = plyr::mapvalues(Sex, from = c("Male ", "Female "), to = c("Male", "Female"))
    )

  bat_data <- rbind(bat_data.s, bat_data.b)

  ## Combine individual and environmental samples in long form
  bat_data %<>% dplyr::mutate(
      `Scientific name` = ifelse(is.na(`Scientific name`) & !is.na(`Scientific name (Environmental)`)
                                 , `Scientific name (Environmental)`
                                 , `Scientific name`)
    , `Common name`     = ifelse(is.na(`Common name`) & !is.na(`Common name (Environmental)`)
                                 , `Common name (Environmental)`
                                 , `Common name`)
    , `Animal type`     = ifelse(is.na(`Animal type`) & !is.na(`Animal type (Environmental)`)
                                 , `Animal type (Environmental)`
                                 , `Animal type`)
    , `Specimen type`   = ifelse(is.na(`Specimen type`) & !is.na(`Animal type`)
                                 , `Animal type`
                                 , `Specimen type`)
    )

  ## Identify environmental samples
  bat_data %<>%
    dplyr::mutate(Recapture = ifelse(is.na(Recapture), FALSE, Recapture)) %>%
    dplyr::mutate(index = seq(dplyr::n())) %>% dplyr::group_by(index) %>%
    dplyr::mutate(env = ifelse(grep("E", Laboratory_ID) %>% length() > 0, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index)

  ## pull just some columns for visuals
  specimen_data %<>%
    dplyr::select(
     `Specimen ID`, `Sample Type`, `Laboratory ID`
   , tidyselect::all_of(tidyselect::ends_with("Outcome"))
   , tidyselect::all_of(tidyselect::ends_with("Year tested"))
   ) %>%
   dplyr::left_join(., bat_data)

  ## ! To resolve when the issue with multiple wells is resolved
  specimen_data <- specimen_data[!duplicated(specimen_data), ]

  ## Convert to long form for better plotting, reserving both Outcome and Year Tested
  outcomes <- specimen_data %>%
    tidyr::pivot_longer(
      .
    , c(tidyselect::all_of(tidyselect::ends_with("Outcome")))
    , names_to = "Virus", values_to = "Result") %>%
    dplyr::select(-tidyselect::all_of(tidyselect::ends_with("Year tested"))) %>%
    dplyr::mutate(
      Virus = apply(Virus %>% matrix(), 1, FUN = function(x) strsplit(x, " Outcome")[[1]][1])
    , Virus = plyr::mapvalues(Virus, from = "Filo Real-time", to = "Filo")
    ) %>%
    dplyr::filter(!is.na(Result)) %>%
    dplyr:: mutate(Result_n = ifelse(((Result %notin% c("Negative", "Inconclusive", "Excluded from testing")) & !is.na(Result)), 1, 0)) %>%
    dplyr::filter(Result != "Excluded from testing")

  yrs_tested <- specimen_data %>%
    tidyr::pivot_longer(
      .
    , c(tidyselect::all_of(tidyselect::ends_with("Year tested")))
    , names_to = "Virus", values_to = "Year Tested") %>%
    dplyr::select(-tidyselect::all_of(tidyselect::ends_with("Outcome"))) %>%
    dplyr::mutate(Virus = apply(Virus %>% matrix(), 1, FUN = function(x) strsplit(x, " Year tested")[[1]][1])) %>%
    dplyr::filter(!is.na(`Year Tested`))

  specimen_data <- dplyr::left_join(outcomes, yrs_tested)

  ## A bit more cleanup
  specimen_data %<>% dplyr::mutate(
    ## If any exist, if not, ignored
    Virus = plyr::mapvalues(
      Virus
    , from = c("Filo Real-time", "PAR", "Influ")
    , to = c("Filo", "Paramyxo", "Flu")
    )
  ) %>% dplyr::rename(Species = `Scientific name`) %>%
    dplyr::mutate(index = seq(dplyr::n())) %>% dplyr::group_by(index) %>%
    dplyr::mutate(env = ifelse(grep("Environmental", `Sample Type`) %>% length() > 0, 1, 0)) %>%
    dplyr::ungroup() %>% dplyr::select(-index)

  ## Add sequence data (cleaned from tables "Sequences" from both bases)
  specimen_data %<>%
    dplyr::left_join(
     .
    , sequence_data %>%
       dplyr::select(Virus, Specimen_ID, Sequence) %>%
       dplyr::mutate(Specimen_ID = lapply(Specimen_ID, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()) %>%
       dplyr::rename(`Specimen ID` = Specimen_ID)
    )

  return(
    list(
      specimen_data = specimen_data
    , sample_data   = bat_data
    )
  )

}


#' Process Summarized Specimen Data for Plotting
#'
#' @param specimen_data Data frame. Already summarized specimen data
#' @param grouping_vec Character vector. Variables to group over to summarize
#' @param wanted_site Desired site to isolate for further summary
#' @param sample_type individual or environmental
#'
#' @return Data frame (further summarized specimen data)
#' @export
process_summarized_specimen_data_for_plotting <- function(specimen_data, grouping_vec, wanted_site, sample_type) {

  `%notin%` <- Negate(`%in%`)

specimen_data %<>% dplyr::filter(Site_Name == wanted_site)

if (sample_type == "individual") {
 specimen_data %<>% dplyr::filter(env == 0)
} else if (sample_type == "environmental") {
 specimen_data %<>% dplyr::filter(env == 1)
} else {
 stop("sample_type choice not supported")
}

## Again, maybe these have already all been caught, but doesnt hurt to double check
specimen_data %<>% dplyr::mutate(
  Sex = plyr::mapvalues(Sex
                        , from = c("Male ", "male", "Female ", NA)
                        , to = c("Male", "Male", "Female", "Not indicated"))
, Age = plyr::mapvalues(Age
                        , from = c("Sub-adult", "Juvenile", "Adult ", NA)
                        , to = c("Sub-Adult", "Sub-Adult", "Adult", "Not indicated"))
, `Reproductive status` = plyr::mapvalues(`Reproductive status`
                                          , from = c("Lactating ", "scrotal")
                                          , to = c("Lactating", "Scrotal"))
)

specimen_data.s <- specimen_data %>%
  dplyr::filter(
    Virus %notin% c("Flu", "Paramyxo", "Rabula", "Filo", "Lyssa")
  , `Animal type` != "Snake"
  ) %>%
  dplyr::group_by(`Laboratory ID`, Virus) %>%
  dplyr::mutate(Result_n = ifelse(any(Result_n == 1), 1, 0)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by_at(c(grouping_vec, "Laboratory ID")) %>%
  ## fixes if there is a pos/neg among sample types for a given animal
  dplyr::filter(Result_n == max(Result_n)) %>%
  ## for pos/neg we care about at the animal level not sample level
   ## NOTE: this needs a bit of cleanup because of changes to "Result" that can happen between
    ## tests. e.g., from Unclassified or Positive to AlphaCoV for example
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by_at(grouping_vec) %>%
  dplyr::summarize(
    n_samples  = dplyr::n()
  , n_positive = sum(Result_n)
  , prop_pos   = n_positive/n_samples
  ) %>%
  dplyr::ungroup() %>%
  ## many missing sample types
  dplyr::filter() %>%
  dplyr::arrange(dplyr::desc(prop_pos))

specimen_data.s.gg <- specimen_data.s %>%
  dplyr::mutate(
    Year.s  = Year - (min(Year) - 1)
  , Month.s = (Year.s - 1) * 12 + Month
  , Year    = as.factor(Year)
  )

## NOTE: needs some cleanup as this is quite breakable
secondary_grouping_vec <- grouping_vec[grouping_vec %notin% c("Year", "Month")]

specimen_data.s.gg.s <- specimen_data.s.gg %>%
  dplyr::group_by_at(c(secondary_grouping_vec, "Year.s")) %>%
  dplyr::summarize(
    tot_samps = sum(n_samples)
  , tot_pos   = sum(n_positive)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    prop_pos = tot_pos / tot_samps
  , Months.s = (Year.s - 1) * 12 + 6
  )

return(
  list(
    specimen_data.gg   = specimen_data.s.gg
  , specimen_data.gg.s = specimen_data.s.gg.s
  )
)

}


#' Process Summarized Sampling Data for Plotting
#'
#' @param sample_data Data frame. Already summarized sample data
#' @param wanted_site Desired site to isolate for further summary
#' @param name_for_sampling_plot Common name for plot of sampling over time
#'
#' @return Data frame (further summarized sampling data)
#' @export
process_summarized_sampling_data_for_plotting <- function(sample_data, wanted_site, name_for_sampling_plot) {

sample_data %<>%
  dplyr::filter(
    Site_Name     == wanted_site
  , `Common name` == name_for_sampling_plot
  , env           == 0
  ) %>% dplyr::mutate(
  Sex = plyr::mapvalues(Sex
                        , from = c("Male ", "male", "Female ", NA)
                        , to = c("Male", "Male", "Female", "Not indicated"))
, Age = plyr::mapvalues(Age
                        , from = c("Sub-adult", "Juvenile", "Adult ", NA)
                        , to = c("Sub-Adult", "Sub-Adult", "Adult", "Not indicated"))
, `Reproductive status` = plyr::mapvalues(`Reproductive status`
                                          , from = c("Lactating ", "scrotal")
                                          , to = c("Lactating", "Scrotal"))
)

sample_data.demo <- sample_data %>%
  ## Don't want duplicate specimens here, just want to look at the individual (Sample)
  dplyr::group_by(`Laboratory ID`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    is_adult     = ifelse(Age == "Adult", 1, 0)
  , is_juvenile  = ifelse(Age == "Sub-Adult", 1, 0)
  , is_pup       = ifelse(Age == "Pup", 1, 0)
  , is_male      = ifelse(Sex == "Male", 1, 0)
  , is_female    = ifelse(Sex == "Female", 1, 0)
  , is_repro     = ifelse(`Reproductive status` %in% c("Pregnant", "Scrotal", "Lactating", "Post-lactating"), 1, 0)
  , is_repro_f   = ifelse(`Reproductive status` %in%
                     #    c("Pregnant", "Lactating", "Post-lactating")
                     #    c("Pregnant", "Lactating")
                          c("Pregnant")
                          , 1, 0)
  , is_repro_m   = ifelse(`Reproductive status` %in% c("Scrotal"), 1, 0)
  , isnt_repro   = ifelse(`Reproductive status` %in% c("Non-scrotal", "Not pregnant"), 1, 0)
  , isnt_repro_f = ifelse(`Reproductive status` %in% c("Not pregnant") & Sex == "Female", 1, 0)
  , isnt_repro_m = ifelse(`Reproductive status` %in% c("Non-scrotal") & Sex == "Male", 1, 0)
  ) %>%
  dplyr::group_by(Year, Month, Day) %>%
  dplyr::summarize(
    n_adult    = sum(is_adult)
  , n_sub      = sum(is_juvenile)
  , n_pup      = sum(is_pup)
  , n_male     = sum(is_male)
  , n_fem      = sum(is_female)
  , n_repro    = sum(is_repro)
  , n_repro_f  = sum(is_repro_f)
  , n_repro_m  = sum(is_repro_m)
  , n_nrepro   = sum(isnt_repro)
  , n_nrepro_f = sum(isnt_repro_f)
  , n_nrepro_m = sum(isnt_repro_m)
  , n_samps    = dplyr::n()
  ) %>%
  dplyr::mutate(
    age_recorded     = (n_adult + n_sub + n_pup) / n_samps
  , sex_recorded     = (n_male + n_fem) / n_samps
  , repro_recorded   = (n_repro + n_nrepro) / n_samps
  , repro_recorded_f = (n_repro_f + n_nrepro_f) / n_fem
  )

Calendar <- establish_calendar()

sample_data.demo %<>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    .
  , Calendar %>% dplyr::select(Month, Day, DOY, Breeding, Gestation, Births, Weaning, SA_Abund)
  ) %>%
  dplyr::mutate(overall_time = (Year - min(Year)) * 365 + DOY, .after = Day)

return(list(
  sample_data.gg = sample_data.demo
))

}

