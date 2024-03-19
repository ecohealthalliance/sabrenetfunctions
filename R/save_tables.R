#' Save ethics reporting table and other DTRA reporting tables locally
#'
#' @param ethics_table Data frame. Ethics table
#' @param DTRA_tables List of data frames (DTRA tables).
#'
#' @return Data frame. Summary of sampling by Year, Month, Site
#' @export
save_tables <- function(ethics_table, DTRA_tables) {

  ethics_file_path <- "outputs/ethics_reporting_table.csv"

  utils::write.csv(ethics_table
            , ethics_file_path
            , row.names = F
            , na = "")

  dtra_file_paths <- c()

  for (i in seq_along(DTRA_tables)) {

    file_name <- paste("outputs/", names(DTRA_tables)[[i]], sep = "") %>% paste(., ".csv", sep = "")

    dtra_file_paths <- append(x = dtra_file_paths,values = file_name)

    utils::write.csv(DTRA_tables[[i]]
              , file_name
              , row.names = F
              , na = "")

  }

  out_paths <-c(dtra_file_paths,ethics_file_path)

  return(out_paths)
}
