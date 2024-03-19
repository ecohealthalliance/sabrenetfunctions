#' Get mode of data
#'
#' @param v data vector
#'
#' @return numeric (mode of input vector)
#' @export
calc_mode    <- function(v) {
  uniqv <- unique(v); uniqv[which.max(tabulate(match(v, uniqv)))]
}


#' Split up data frame into list with entries defined by 'col'
#'
#' @param tibble Tibble for splitting into list
#' @param col single or multiple columns over which the tibble will be split up into list entries
#'
#' @return List of tibbles split according to the argument col
#' @export
split_tibble <- function(tibble, col = 'col') {
  temp_list <- tibble %>% split(., .[, col])
  ## allow for multiple grouping columns, but drop those entries with no records
  temp_list[(lapply(temp_list, nrow) %>% unlist() > 0)]
}


#' Get base ID from Sys.getenv() to work in Actions workflow
#'
#' @return Airtable base IDs from .yml / github actions
#' @export
get_base_id <- function() {
  base_id <- Sys.getenv("BASE_ID")
  if (grep("[,]", base_id) %>% length() > 0) {
    base_id <- (base_id %>% strsplit(., ", "))[[1]] %>% as.list()
  }
}


#' Convert dms to degree
#'
#' @param x (d)egree (m)inute (s)econd vector of locations
#'
#' @return degree locations
#' @export
dms_to_deg <- function (x) {
  dms <- strsplit(x, "[:]")[[1]] %>% as.numeric()
  if (dms[1] < 0) {
    dms[1] - dms[2] / 60 - dms[3] / 3600
  } else {
    dms[1] + dms[2] / 60 + dms[3] / 3600
  }
}


#' Post-fitting correction for prevalence of pooled samples
#'
#' @param num_pos_pools Number of positive pools
#' @param num_pools Number of pools
#' @param pool_size Number of samples per pool
#'
#' @return Adjusted positivity controlling for the pooling
#' @export
correct_pooled_prevalence <- function(num_pos_pools, num_pools, pool_size) {
  x <- num_pos_pools
  n <- num_pools
  m <- pool_size
  a <- 0.5 * (m - 1) / m
  p_hat <- 1 - ((n - x + a) / (n + a))^(1 / m)
  p_hat
}

