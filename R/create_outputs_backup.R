#' Move Figures and tables to google drive
#'
#' @param tar_obj Target. A target object to be backed up the output of tar_file
#' @param ... additional targets to be backed up
#' @param google_drive_folder String. Where should files go in Google drive
#'
#' @return Documentation of upload
#' @export
create_outputs_backup <- function(tar_obj, ..., google_drive_folder = "Figures and Tables") {

  ## local file path
  file_path <- c(tar_obj,...)

  ## file name (last part of path)
  file_name <- basename(file_path)

  ## upload to google drive
  googledrive::drive_auth(path = "auth/sabrenet-d8d2c083cff4.json")

  ## overwrites "current" file
  current_upload <- purrr::map2(file_path, file_name, function(x, y) {
    print(c(x, y))
    googledrive::drive_upload(media = here::here(x), path = google_drive_folder, name = y, overwrite = TRUE)
  })

  print("making archive")

  ## archive path
  archive_path       <- sprintf("%s/archive", google_drive_folder)
  archive_path_today <- sprintf("%s/%s/", archive_path, Sys.Date())

  ## should be a tryCatch where it passes only on specific errors
  try(googledrive::drive_mkdir(name = as.character(Sys.Date()), path = archive_path, overwrite = FALSE))

  ## does not overwrite, will create multiple files with the same name if run multiple times
  archive_upload <- purrr::map2(file_path, file_name, function(x, y) {
    googledrive::drive_upload(media = here::here(x), path = archive_path_today, name = y)
  })

  return(list(current_upload, archive_upload))

}
