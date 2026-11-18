#-------------------------------------------------------------------------------
# Method-specific production helper functions
#-------------------------------------------------------------------------------

# Instantaneous growth rate method
.igr <- function(growth,biomass,sample,class.type,class.size) {

  # Create age classes
  if(!class.type %in% colnames(biomass)){
    stop(paste0('"biomass" does not contain column for ',class.type))
  }
  if(!class.type %in% colnames(growth)){
    stop(paste0('"growth" does not contain column for ',class.type))
  }
  size_input <- unique(c(biomass[[class.type]],growth[[class.type]]))

  # Summarize biomass and growth at size or age class
  class_df <- .class_fun(size_input,class.type,class.size)
  biomass_sc <- .class_bio_sum(sample,class_df,biomass)
  growth_sc <- .class_growth_sum(growth,class_df)

  # Assess data overlap
  if(biomass_sc %>%
     dplyr::filter(  # these have warnings earlier in function
       !is.na(group_id),
       !is.na(interval)
     ) %>%
      dplyr::anti_join(
        growth_sc,
        by = dplyr::join_by("class", "group_id", "interval")
      ) %>%
      nrow() > 0) {
    warning(
      'Some size classes in biomass are missing growth rates. NA production ',
      'will be returned'
      )
  }

  # combine biomass and growth data
  input <- biomass_sc %>%
    dplyr::left_join(
      growth_sc,
      by = dplyr::join_by("class", "group_id", "interval")
    )

  # Estimate class specific igr production
  prod_sc <- .class_igr(input)

  # Summarize
  .prod_sum(prod_sc)

}


#-------------------------------------------------------------------------------
# Size frequency method


