#-------------------------------------------------------------------------------
# General helper functions
#-------------------------------------------------------------------------------


# Summarize data in arrays created in bootstrapping or posterior distribution
# sampling. Returns data.frame containing group specific mean or median values
# with 95% confidence or credible intervals

.boot_summary <- function(array,group.var,sum.fun="mean",ci=c(0.0275,0.975)){

  # Summary function
  fun <- get(sum.fun)

  # summary function label
  fun_label <- paste0("_",sum.fun)

  # Calculate summary statistic and 95 CI interval of estimates
  summary <- as.data.frame(apply(array,c(1,2), fun))
  uprs <- as.data.frame(apply(array,c(1,2), quantile, probs=ci[2], na.rm=T))
  lwrs <- as.data.frame(apply(array,c(1,2), quantile, probs=ci[1], na.rm=T))

  # Merge into one summary table
  summary %>%
    dplyr::left_join(
      lwrs,
      by=group.var,
      suffix = c("","_lwr")
    )  %>%
    dplyr::left_join(
      uprs,
      by=group.var,
      suffix = c(fun_label,"_upr")
    )
}


# Summarize non-bootstraped sample density and biomass and data.

.biomass_summary <- function(biomass){

  # Calculate density and biomass of fish at each sample
  biomass %>%
    group_by(site,cum) %>%
    summarise(n = n(),
              mean_mass = mean(dry_wt,na.rm=T),
              area = mean(area, na.rm=T)) %>%
    mutate(sample_den = n/area,
           sample_biomass = mean_mass*sample_den) %>%
    select(-mean_mass,-n,-area) %>%
    ungroup()
}
