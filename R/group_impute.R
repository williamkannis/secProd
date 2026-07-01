#' Randomly impute missing values within groupings
#'
#' @description
#' Impute missing values by taking random sample within user defined groupings.
#' Data is only imputed if group has a sufficncent sample size defined by the
#' user.
#'
#' @param data data.frame containing missing data.
#' @param column name of column with missing data.
#' @param by name of columns used to define group.
#' @param limit minimum number of non-NA values within a group required to
#' impute data
#' @param impute.tag numeric or character value to designate rows where data
#' were imputed.
#'
#' @returns input data.frame with missing values imputed. Additional column
#' 'impute' with user specified tag to designate rows where data were imputed.
#'
#' @export


group_impute <- function(data, column, by, limit, impute.tag = "imputed"){

  # Are columns present in data?
  tidyselect::eval_select(rlang::enquo(column), data)
  tidyselect::eval_select(rlang::enquo(by), data)

  # is limit numeric?
  stopifnot('Please provide "limit" as numeric value'=is.numeric(limit))

  # are missing values present?
  miss_data <- data %>% pull({{ column }})
  if(!any(is.na(miss_data))) {
    return(data)
    warning(paste0(
      "No missing values present in indicated column. Returning identical ",
      "data.frame"
    )
  )
  }

  # impute data based data availability
  data %>%
    dplyr::mutate(
      par_impute = .impute_sampler({{ column }},limit),
      impute = dplyr::case_when(
        is.na({{ column }}) & !is.na(par_impute) ~ impute.tag,
        T ~ NA
      ),
      {{ column }} := par_impute,
      .by = {{ by }}
    ) %>%
    dplyr::select(-par_impute)
}

# Helper function to sample groups with sufficient sample sizes, and only replace
# NA values
.impute_sampler <- function(x,limit) {

  # only sample from group if sufficient sample of non-na's exist
  n_real <- sum(!is.na(x))
  if(n_real<=limit) return(x)

  # only NA values should be repaced
  x[is.na(x)] <- sample(
    x[!is.na(x)],
    sum(is.na(x)),
    replace = T
  )
  x
}
