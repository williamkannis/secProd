#-------------------------------------------------------------------------------
# Method-specific production helper functions
#-------------------------------------------------------------------------------

# Instantaneous growth rate method
.igr <- function(growth,biomass,sample,class.type,class.size) {

  # # Assess input data
  # if(c("") %in% names(biomass)){
  #   stop()
  # }
  # if(c("") %in% names(growth)){
  #   stop()
  # }
  # if(c("") %in% names(sample)){
  #   stop()
  # }

  #ASSESS DATA OVERLAP

  # Summarize biomass and growth at size or age class
  class_df <- .class_fun(growth,class.type,class.size)
  biomass_sc <- .class_bio_sum(sample,class_df,biomass)
  growth_sc <- .class_growth_sum(growth,class_df)

  # combine biomass and groth data
  input <- biomass_sc %>%
    dplyr::left_join(
      growth_sc,
      by = join_by("class", "group_id", "interval")
    )

  # Estimate class specific igr production
  prod_sc <- .class_igr(input)

  # Summarize
  .prod_sum(prod_sc)

}


#-------------------------------------------------------------------------------
# Size frequency method


