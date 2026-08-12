#' Calculate the amount of time between two sampling periods.
#'
#' @description The time between two user indicated sampling periods is
#' calculated and assigned to the first sampling period in each pair. For use
#' in interval growth estimation.
#'
#'@param df data.frame containing columns for sample site (site), sample period
# (cum) and date. Column names must be name site, cum, and date
#'
#'@returns data.frame with columns for site, sample period, date, and sample
# interval length. Assigns interval length to first sampling period in each pair
#'
#'@export

sample_interval <- function(df){
  df %>%
    # dplyr::distinct(dplyr::all_of(sample.groups,"date"))
    # distinct(cum,site,group_id,date) %>%
    dplyr::group_by(site) %>%

    # Estimate sampling intervals
    dplyr::mutate(interval = dplyr::case_when(

      # do not estimate sampling intervals when the next period was not sampled
      cum+1 != lead(cum,order_by = cum) ~ NA,
      T~lead(date,order_by = cum)-date
    ),
    interval = as.numeric(round(interval))
    ) %>%
    dplyr::ungroup()
}
