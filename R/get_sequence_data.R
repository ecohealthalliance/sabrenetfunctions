#' Get Sequence Data -- Extract sequence data from Specimen Results and strip the sequence from the uploaded .qb or .fas files
#'
#' @param specimen_data Specimen data
#' @param cleaned_sequences Sequences from Sequences table in Airtable (already stripped from file attachments in Specimen Data)
#' @param base base id
#' @param dir_name Location to temporarily store sequence file for processing
#' @param field string to look for for sequence attachments
#'
#' @return Data frame of sequences for all entries of sequence data that have files uploaded
#' @export
get_sequence_data <- function(
  specimen_data     = specimen_data
, cleaned_sequences
, base              = base_id
, dir_name          = "data/sequence_data"
, field             = "Consensus Sequence"
  ) {

  ## Get already stripped sequence data from table "Sequences" and do some cleaning
  cleaned_sequences %<>%
    dplyr::rename(spec_id = Specimen_ID) %>%
    dplyr::mutate(
     `Specimen ID` = lapply(`Specimen ID`, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    , spec_id      = lapply(spec_id, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()
    ) %>%
    dplyr::select(-c(id, base_id))

  ## Get the entries in specimen data which have raw sequence data
  sequences.raw  <- specimen_data %>%
    dplyr::select(dplyr::contains(field) & tidyselect::where(is.list), id, base_id) %>%
    tidyr::pivot_longer(-c(id, base_id)) %>%
    dplyr::mutate(value = lapply(value, FUN = function(x) ifelse(is.null(x), NA, x)) %>% unlist()) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(index = seq(dplyr::n()))

  ## Check which of Specimen IDs with raw sequence files already have had their sequences cleaned
  sequences.raw %<>%
    dplyr::left_join(., cleaned_sequences %>% dplyr::rename(id = `Specimen ID`), by = "id") %>%
    dplyr::filter(is.na(Sequence)) %>%
    dplyr::select(-c(Sequence, Name, Virus))

  ## Retrieve each raw sequence file that has not yet been cleaned; clean and combine
  if (nrow(sequences.raw) > 0) {

  sequences.raw %<>%
    dplyr::mutate(index = seq(dplyr::n())) %>%
    split_tibble("index")

  sequences <- lapply(sequences.raw, FUN = function(x) get_each_sequence(x, dir_name)) %>% do.call("rbind", .)

  ## bit of cleaning to match Airtable structure for inserting
  sequences %<>%
    dplyr::select(base_id, id, name, clean_seq) %>%
    dplyr::rename(Sequence = clean_seq, Name = name) %>%
    dplyr::mutate(Virus = apply(Name %>% matrix(), 1, FUN = function(x) strsplit(x, " Consensus")[[1]][1])) %>%
    dplyr::left_join(., specimen_data %>% dplyr::select(`id`, `Specimen ID`), by = "id") %>%
    dplyr::relocate(., c(`Specimen ID`, Virus), .after = id)

  ## Add new stripped sequences to those already cleaned ones directly downloaded from "Sequences"
  cleaned_sequences %<>%
    dplyr::select(-c(`Specimen ID`, createdTime)) %>%
    dplyr::rename(`Specimen ID` = spec_id) %>%
    rbind(., sequences %>% dplyr::select(Name, `Specimen ID`, Virus, Sequence))

  return(
    list(
      new_sequences     = sequences
    , cleaned_sequences = cleaned_sequences
    )
  )

  } else {

  cleaned_sequences %<>% dplyr::select(-`Specimen ID`) %>% dplyr::rename(`Specimen ID` = spec_id)

  return(
    list(
      new_sequences     = NULL
    , cleaned_sequences = cleaned_sequences
    )
  )

  }

}


#' Guts of the internal lapply within get_sequence_data. This function not to be run separately, run the wrapper get_sequence_data
#'
#' @param x a sequence file
#' @param dir_name name of local directory where sequence files will temporarily get stored for parsing
#'
#' @return a data frame with a stripped sequence (character string) and identifier information for this sample
#' @export
get_each_sequence <- function(x, dir_name) {

    ## Get each Specimen Data entry with an unstripped sequence
    y       <- airtabler::air_get(
      base       = x$base_id
    , table_name = "Specimen Data"
    , record_id  = x$id
    )

   ## build the file name for downloading
   xfield   <- purrr::pluck(y$fields, x$name)
   file_ext <- (xfield$filename %>% strsplit(., "[.]"))[[1]][2]

   filename <- paste(x$id, x$name, sep = "--") %>%
     paste(., file_ext, sep = ".")
   dest     <- sprintf("%s/%s", dir_name, filename)

   ## Try to download each. Store false if the download fails, these will throw an error
   downloaded <- try({utils::download.file(url = xfield$url, destfile = dest, quiet = T)}, silent = TRUE) %>% {
     if(class(.) == "try-error") FALSE else TRUE
     }

   x %<>% dplyr::mutate(downloaded = downloaded, filename = filename, ext = file_ext)

   ## Strip out the sequence depending on file type
   if (x$downloaded == TRUE) {
      if (x$ext == "fas") {
        temp_csv <- suppressWarnings(utils::read.csv(dest))
        if (nrow(temp_csv) == 1) {
         names(temp_csv) <- NULL
         x %<>% dplyr::mutate(clean_seq = temp_csv %>% unlist())
        } else {
         temp_csv <- temp_csv[-1, 1] %>% paste(., collapse = "")
         x %<>% dplyr::mutate(clean_seq = temp_csv %>% unlist())
        }
      } else if (x$ext == "gb") {
        ## the .gb extension needs a custom function
        x %<>% dplyr::mutate(clean_seq = suppressWarnings(read.gb(dest)))
      } else {
        print("File type not supported, check files manually")
        print(y$fields$`Specimen ID`)
        x %<>% dplyr::mutate(clean_seq = NA)
      }
    } else {
      stop("Problem downloading file, check files manually")
    }

   ## Remove the locally saved temporary file
   print(y$fields$`Specimen ID`)
   print(x$clean_seq)
   file.remove(dest)

   x

  }


#' Function to extract sequence from .gb files. Called from within get_each_sequence
#'
#' @param z a .gb file
#'
#' @return a stripped sequence (character string)
#' @export
read.gb <- function(z) {
  x <- utils::read.csv(z)
  get_bp <- x[1, ] %>% strsplit(" ") %>% unlist()
  get_bp <- get_bp[grep("bp", get_bp) - 1]
  x <- x[4:(nrow(x) - 1), ]
  if (any(grepl("//", x))) {
    x <- x[-grep("//", x)]
  }
  x <- apply(x %>% matrix(), 1, FUN = function(y) {strsplit(y, "1 ")[[1]][2]})
  x <- apply(x %>% matrix(), 1, FUN = function(y) {strsplit(y, " ")[[1]] %>% paste(collapse = "")}) %>% paste(collapse = "")

  if ((strsplit(x, "") %>% unlist() %>% length()) != get_bp){
    print("Base pairs not parsed correctly")
    return(NA)
  } else {
    return(x)
  }

}

