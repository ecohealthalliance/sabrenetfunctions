#' Build some file paths for saving outputs
#'
#' @param tar_obj Saved tar object built from a tar_make call
#' @param pattern file extension
#' @param ... additional arguments
#'
#' @return Local file paths for saving objects
#' @export
generate_file_paths_linked <- function(tar_obj, pattern, ...) {

  list_tars    <- list(tar_obj, ...)
  flat_reports <- unlist(list_tars, recursive = TRUE)
  files        <- purrr::keep(flat_reports, function(x) {grepl(pattern, x)})

  if (length(files) == 0) {
    stop("No files found")
  } else {
    message(files)
  }

  return(files)
}
