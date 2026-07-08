#' Estimate secondary production
#'
#' @description UPDATE
#' Estimates production, standing biomass, and PtoB ratios using instantaneous
#' growth rate method. Production is estimated for all sites and sampling
#' intervals contained in a data frame using size/age class specific growth
#' rates and biomass summarized at the sample interval.
#'
#' @param growth data.frame containing sampling event and size-specific site
#' instantaneous growth rates. Should contain columns for sample interval (cum),
#' length, age, interval length (interval).
#' @param biomass data.frame containing size scrutruce data with columns for
#' site, sample interval (cum),length, weight (dry_wt), and area.
#' @param class.size numeric value indicating the denominator for size class
#' estimation For example, 30 indicates size classes are 30 day cohorts
#
#'
#' @details
#' Additional details...
#'
#'
#' @returns UPDATE data.frame summarized at the site/sample interval level containing
#' original grouping variables and estimates of  biomass, standing biomass, and
#' production.
#'
#' @examples
#'
#'
#' @export

production <- function(
    method,
    sample,
    biomass,
    growth = NULL,
    class.type,
    class.size,
    bio.boot = F,
    growth.boot = F,
    iter = NULL,
    return.raw = F,
    parallel = F,
    mc.cores = NULL
){

  boot <- any(bio.boot,growth.boot)

  # Production method errors
  if(!method %in% c("igr")){
    stop('"method" must be one of the following: "igr"')
  }
  if(method %in% c("igr") & is.null(growth)){
    stop(
      paste0("Please provide data.frame or list 'growth' when estimating ",
             "production using method = 'igr'"))
  }

  # Bootstrapping errors
  if(!boot & !is.null(iter)){
    iter = NULL
    warning(paste0(
      "Bootstrapping iterations 'iter' provided but no bootstrapping selected.",
      " Did you mean to select 'bio.boot' or 'growth.boot' as TRUE? Ignoring ",
      "'iter' input.")
    )
  }

  if(boot & is.null(iter)){
    stop("Please provide numeric value 'iter' for bootstrapping iterations")
  }

  if(!method %in% c("igr") & growth.boot){
    stop(
      paste0('growth.boot can only be TRUE when estimating production with ',
             'method = "igr"'))
  }

  if(growth.boot & length(dim(growth)) != 3){
    stop(paste0(
      "If bootstrapping growth rates, please provide 'growth' as a ",
      "3-dimensional array.")
    )
  }

  # if(growth.boot & !inherits(growth, "list")){
  #   stop(paste0(
  #     "If bootstrapping growth rates, please provide growth as a list of ",
  #     "data.frames")
  #     )
  # }


  if(!growth.boot & !is.data.frame(growth)){
    stop(paste0(
      "When not bootstrapping growth rates, please provide 'growth' as a ",
      "data.frame.")
    )
  }

  # Assess sample data
  if(is.null(sample$area)) {
    sample$area = 1
    warning(paste0(
      "No vaule for 'area' provided in 'sample', all estimates will assume an ",
      "area of 1."))
  }

  # Parallel processing set up
  if (parallel) {
    lapply_fun <- function(...) parallel::mclapply(...,mc.cores=mc.cores)
  } else {
    lapply_fun <- lapply
  }


  # Production estimation inputs
  prod_fun <- get(paste0(".",method))
  if(!boot) iter = 1

  # production estimation
  prod_list <- lapply_fun(seq_len(iter), function(i){
    arg <- list(
      sample = sample,
      biomass = biomass,
      growth = growth,
      class.type = class.type,
      class.size = class.size
    )

    # Bootstrap data as indicated
    if(bio.boot) arg$biomass <-  dplyr::slice_sample(
      biomass,
      prop=1,
      replace = T,
      by=c(site,cum)
    )
    if(growth.boot) arg$growth <- .post_draw(growth_array,1)[[1]]

    # Return production based on specified methodology
    do.call(prod_fun,arg)
  })

  # summarize bootstrapping results where applicable
  if(boot) {
    prod <- .boot_summary(prod_list,group.var = c("site","cum"))
  } else {
    prod <- prod_list[[1]]
  }

  # non-bootstrapped sample density and biomass
  biomass_sum <- .biomass_summary(
    biomass = biomass,
    sample = sample,
    group.var = c("site","cum"))

  # Prepare output
  out_summary <- biomass_sum %>%
    left_join(
      prod,
      by=join_by(site,cum)
    )

  # with no biomass bootstrapping, the biomass output from production helpers
  # is redundnat to biomass outpur from summary function
  if(!bio.boot){
    out_summary <- out_summary %>%
      dplyr::select(-starts_with("biomass_"))
  }

  # Export raw bootstrapping data if specified
  if(return.raw) {
    out <- list(out_summary,prod_list)
    names(out) <-  c("production_summary","raw_production")
    return(out)
  }
  out_summary
}
