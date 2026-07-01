#' Instanteonous growth rate method production estimates
#'
#' @description
#' Estimates production, standing biomass, and PtoB ratios using instantaneous
#' growth rate method. Production is estimated for all sites and sampling
#' intervals contained in a data frame using size/age class specific growth
#' rates and biomass summarized at the sample interval.
#' @param growth data.frame containing sampling event and size-specific site
#' instantaneous growth rates. Should contain columns for sample interval (cum),
#' length, age, interval length (interval).
#' @param biomass data.frame containing size scrutruce data with columns for
#' site, sample interval (cum),length, weight (dry_wt), and area.
#' @param size.class numeric value indicating the denominator for size class
#' estimation For example, 30 indicates size classes are 30 day cohorts
#'
#' @description
#' A short description...
#'
#' @returns data.frame summarized at the site/sample interval level containing
#' original grouping variables and estimates of  biomass, standing biomass, and
#' production.
#'
#' @export


production_igr <- function(growth,biomass,size.class) {

  options(dplyr.summarise.inform=F)

  # Link class-specific growth rates to biomass data
  df <- size_class_growth(growth,biomass,10)

  # Estimated production per size class
  prod <-size_class_igr(df)

  # Sum all sizes classes into period-level production estimates
  prod %>%
    group_by(site,cum) %>%
    summarize(
      biomass= sum(biomass),
      stand_biomass= sum(stand_bio),
      production = sum(production),
      ptob = production/stand_biomass
    )
}


# Helper function that summarizes biomass and growth data by specified age
# class. Uses growth rates derived from growth curves. The average growth rate
# per age class is estimated  using all possibles sizes for that class rather
# than within class size  structure of data. This removes issues of inconsistent
# growth calculations between intervals with data at initial sample and those
# with data only at final sample. See archived version of function for more
# info.

size_class_growth <-function(growth,biomass,size.class) {

  ## Summarize biomass into age classes  ##
  biomass_sc <- growth %>%

    # Create size or age classes
    mutate(age_class = floor(age_pred/size.class)) %>%

    # join in fish biomass data. Size classes with no fish observed at that sample
    # will have a NA weight
    # left_join(biomass,by=join_by(site, cum, length)) %>%
    left_join(biomass,by=join_by(group_id, interval, length)) %>%

    # Group into size/age classes based on growth rate data and calculate
    # the total number of fish in that class as well as average weight and growth
    # rates. Unobserved  class have NA weights and will be ignored in this step.
    # group_by(site,cum,age_class) %>%
    group_by(site,cum,age_class,group_id) %>%
    summarize(
      interval = mean(interval),
      mean_mass=mean(dry_wt,na.rm=T),
      n=length(dry_wt[!is.na(dry_wt)]),
      area = mean(area,na.rm=T)
    ) %>%

    # For unobserved size classes, change weights to zero
    mutate(mean_mass = case_when(
      is.nan(mean_mass) ~ 0,
      T~ mean_mass
    ),
    area =case_when(
      is.nan(area) ~ 1,
      T~ area
    )) %>%
    ungroup()

  ## Age class-specific growth rates  ##
  growth %>%

    # Create size or age classes
    mutate(age_class = floor(age_pred/size.class)) %>%

    # Age class specific growth rate.
    # group_by(site,cum,age_class) %>%
    group_by(group_id,interval,age_class) %>%
    summarize(mean_growth=mean(interval_growth_pred)) %>%

    # merge in biomass data
    # left_join(biomass_sc, by = join_by(site, cum, age_class))
    left_join(biomass_sc, by = join_by(interval, group_id, age_class))
}

# You may want to modify this so you only have to make one prediciton
# per age group, and then input. less flexible but more computer friendly,
# think about this


# Helper function to Estimate age-class-pecific standing biomass and production
# between sampling intervals

# INPUT:
# ! df = data frame summarized at age class containing columns for site, sample
#        interval (cum), sample interval length (interval), age_class, number of
#        individual per age class, average weight and growth rate of class

# OUTPUT: data frame contain original columns plus production and standing
# biomass estimates

# REQUIRES: dplyr (all)

size_class_igr <- function(df) {
  df %>%
    group_by(site,age_class) %>%
    mutate(
      biomass = (n*mean_mass)/area,
      lead_biomass = case_when(
        cum+1 != lead(cum,order_by = cum) ~NA,  # if subsequent sample event is missing, assign lead to zero
        T~ lead(biomass,order_by = cum)  # otherwise use value from subsequent sample
      ),

      # Estimate standing biomass and production between two sampling events
      stand_bio = (biomass+lead_biomass)/2,
      production = stand_bio*mean_growth
    ) %>%
    ungroup()
}


# ------------------------------------------------------------------------------
#' Bootstrapped instantaneous growth rate production estimates
#'
#' @description
#' A short description...
#'
#' @param
#'
#' @returns description
#'
#' @export

# DESCRIPTION: Estimates production, standing biomass, and PtoB ratios using
# instantaneous growth rate method. Production is estimated for all sites and
# sampling intervals contained in a data frame using size/age class specific
# growth rates and biomass summarized at the sample interval. Result include
# uncertainty in the form of confidence intervals derived from bootstrapping the
# biomass data and the option to sample random posteior draws from growth-curve
# derived growth rates (provide array). Summarized results are returned with
# mean and CIs for estimated quantities, along with non-bootstrapped biomass
# and density measures.

# ! growth = data frame or array created using  growthStack or prediction_samplr.
#             Should contain site, sample interval (cum), length, age, interval
#             length (interval) and interval growth rate. If array is provided, each
#             slice should represent one posterior draw of a growth rate estimate.
#             When array is provided, random draws form growth rates are added
#             to bootstrap. When data frame is provided, only biomass is bootstrapped.
# ! biomass = data frame containing columns for site, sample interval (cum),
#             length, and weight (dry_wt).
# ! size.class = numeric value indicating the denominator for size class
#              estimation For example, 30 indicates size classes are 30 day
#              cohorts
# ! n.sim = numeric, number of iterations for bootstrap
# ! return.raw = T or F, return bootstrap array along with summarized estimates
# ! parallel = T or F, use parralel processing?
# ! mc.cores = number of cores for parallel processing

# OUTPUT: If return.raw = F, a dataframe containg non-bootstrap biomass and
# density estimates along with mean and 95CI esitmates of biomass, standing
# biomass, and PtoB ratios. If return = T, a list containg the summarized data
# frame and a 3d array with raw bootstrapping results with columns for site
# and sampling interval, and the biomass, standing biomass, production, and
# PtoB ratios of that bootstrap iteration

# REQUIRES: dplyr (all), parallel (recommenced)

production_igr_boot <- function(
    growth,
    biomass,
    size.class,
    n.sim,
    return.raw=F,
    parallel=F,
    mc.cores=NULL
){

  # Parallel processing?
  if (parallel == T) {
    lapply_fun <- function(...) parallel::mclapply(...,mc.cores=mc.cores)
    map_fun <- function(...) parallel::mcMap(...,mc.cores=mc.cores)
  } else {
    lapply_fun <- lapply
    map_fun <- Map
  }

  ## Bootstrap biomass ##
  # create n.sim bootstraps of mass data
  biomass_list <- lapply_fun(seq_len(n.sim),
                             function(x) slice_sample(
                               biomass,
                               prop=1,
                               replace = T,
                               by=c(site,cum)))

  ## Bootstrap growth rate ##
  # Are there more than one sample of predicted growth rates?
  if(is.na(dim(growth)[3])){

    ## When growth rates do not have more than one sample, only bootstrap mass
    out_list <- lapply_fun(
      biomass,
      production_igr,
      growth=growth,
      size.class=size.class
    )

  } else {
    # Draw n.sim samples from growth posterior
    growth_list <-.post_draw(growth,n.sim)

    # estimate secondary production with each growth and mass resample
    out_list <- map_fun(
      function(x,y) production_igr(
        x,
        y,
        size.class=size.class
      ),
      growth_list,
      biomass_list)
  }
  out_array <- abind::abind(out_list,along = 3)

  ## Summarize bootstrapping results ##
  out_summary <- .biomass_summary(biomass) %>% # non-bootstrapped sample density and biomass
    right_join(
      .boot_summary(out_array,group.var = c("site","cum")),  # Calculate mean and 95 CI interval of estimates
      by=join_by(site,cum)
    ) %>%

    # biomass_summary will give NAs to sites with no fish, change these to zero
    mutate(
      sample_den = case_when(
        biomass_mean == 0 ~ 0,
        T ~ sample_den
      ),
      sample_biomass = case_when(
        biomass_mean == 0 ~ 0,
        T ~ sample_biomass
      )
    )

  # Export raw bootstrapping data if specified
  if(return.raw == T) {
    out <- list(out_summary,out_array)
    return(out)
  }
  out_summary
}

