#' pull data down from airtable
#'
#' @param base Airtable base
#' @param table_name Airtable table name
#' @param view View for the Airtable table name desired
#' @param fields which fields to download
#' @param ... anything else
#'
#' @return A data frame of data from Airtable
#' @export
get_sabrenet_data <- function(base = list("appi9YHtonEpMFuAz"), table_name = "Sample Data", view = "For Export", fields, ...) {

  `%notin%` <- Negate(`%in%`)

   dat <- lapply(base, FUN = function(x) {
      td <- airtabler::fetch_all(base = x, table_name = table_name, view = view, fields = fields, ...)
      td %<>% dplyr::mutate(base_id = rep(x, dplyr::n()), .before = tidyselect::everything())
      td
    }
  )  %>% do.call("rbind.fill", .)

   needed_cols <- unlist(fields)[which(unlist(fields) %notin% names(dat))]

   for (i in seq_along(needed_cols)) {
     dat %<>% dplyr::mutate(!!needed_cols[i] := "")
   }

   return(dat)

}
