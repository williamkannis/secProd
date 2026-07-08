#-------------------------------------------------------------------------------
# General production helper functions
#-------------------------------------------------------------------------------

# Separate data into size or age classes for class specific production estimates

.class_fun <- function(input,class.type,class.size) {

  # Error messages
  if(!class.type %in% c("age","size")){
    stop('class.type must be "age" or "size"')
  }
  if(any(!c("group_id","length") %in% names(input))) {
    stop('input must contian columns "group_id" and "length"')
  }

  #  class
  if(class.type == "age"){
    class_df <- input %>%
      dplyr::mutate(class = floor(age_pred/class.size)) %>%
      dplyr::distinct(group_id,length,class)
  }
  if(class.type == "size"){
    class_df <- input %>%
      dplyr::mutate(class = floor(length/class.size)) %>%
      dplyr::distinct(group_id,length,class)
  }

  # CHeck that al groups have the same groups
  group <- unique(class_df$group_id)
  class_test <-lapply(group, function(x){
    class_df %>%
      dplyr::filter(group_id == x) %>%
      dplyr::distinct(class) %>%
      dplyr::pull(class)
  })
  if(length(unique(class_test)) != 1){
    stop("Growth groups do not have the same size/age classes")
  }

  return(class_df)
}

#-------------------------------------------------------------------------------
# Summarize biomass at age class, using sample data to record instance with no
# individuals. These will be presented with NaN wt and can be changed to zero
.class_bio_sum <- function(sample,class_df,biomass) {
  sample %>%
    dplyr::left_join(
      class_df,
      by = "group_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      biomass,
      by = c("cum", "site", "length")
    ) %>%
    dplyr::group_by(site,cum,class,group_id) %>%
    dplyr::summarize(
      interval = mean(interval),
      mean_mass=mean(wt,na.rm=T),
      n=sum(!is.na(wt)),
      area = mean(area,na.rm=T),
      .groups = "drop"
    ) %>%
    dplyr::mutate(mean_mass = case_when(
      is.nan(mean_mass) ~ 0,
      T~ mean_mass
    ))

  # Not every growth grouping has all age classes, this can lead to NA
  # production downstream. Add in missing age classes to demostrate no fish of
  # these ages were found

  ## THIS WORKS BUT CREATES ADDITIONAL ISSUES. THERE ARE NO GROWTH RATES FOR
  ## THESE AGE CLASSES LEAING TO NA PRODUCTION WHEN TWO SAMPLE OF BIOMASS ARE
  ## AVAILABE. MAY NEED TO JUST USE SIZE CLASS INSTEAD OF AGE.

  ## IF YOU WANT TO USE AGE, WILL NEED TO USE POPULATION LEVEL PARAMETERS ##


  # class_id <- unique(class_df$class)
  # miss_class <- sample %>%
  #   dplyr::distinct(cum,site,interval,area,group_id) %>%
  #   tidyr::crossing(class=class_id) %>%
  #   dplyr::mutate(
  #     mean_mass = 0,
  #     n = 0
  #   ) %>%
  #   dplyr::anti_join(biomass_sc,by = c("site","cum","class"))
  # biomass_sc %>% dplyr::bind_rows(miss_class)
}


#-------------------------------------------------------------------------------
# Summarize growth rates per class

.class_growth_sum <- function(growth,class_df) {
  growth %>%
    dplyr::right_join(
      class_df,
      by = c("group_id", "length")
    ) %>%
    dplyr::group_by(group_id,interval,class) %>%
    dplyr::summarize(
      mean_growth=mean(interval_growth_pred),
      .groups = "drop"
    )
}


#-------------------------------------------------------------------------------
# Class specific production via instantaneous growth rate method

.class_igr <- function(input) {
  input %>%
    dplyr::group_by(site,class) %>%
    dplyr::mutate(

      # Estimate individual and interval biomass for production estimate
      # Do not assign lead biomass if consecutive samples are not taken
      biomass = (n*mean_mass)/area,
      lead_biomass = case_when(
        cum+1 != lead(cum,order_by = cum) ~NA,
        T~ lead(biomass,order_by = cum)
      ),
      interval_biomass = (biomass+lead_biomass)/2,
      production = interval_biomass*mean_growth
    ) %>%
    dplyr::ungroup()
}


#-------------------------------------------------------------------------------
# Summarize class-specific production and friends to the a specified grouping

.prod_sum <- function(input){

  # Method specific columns to summarize
  summary_cols <- c(
    "biomass",
    "interval_biomass",
    # "mean_growth",
    "production"
  )
  select_cols <- summary_cols[summary_cols %in% colnames(input)]

  #if(select_cols)

  # Summarize production at grouping level, and estimat PtoB
  input %>%
    dplyr::select(dplyr::all_of(c("site","cum",select_cols))) %>%
    dplyr::group_by(site,cum) %>%
    dplyr::summarize(
      dplyr::across(
        everything(),
        # \(x) sum(x, na.rm = TRUE)  ## NARM TURNS NAS INTO ZEROS< THIS ISNT IDEAL IN ALL CASES
        # \(x) if_else(all(is.na(x)), NA_real_, sum(x)),
        sum
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(PtoB = production/interval_biomass) %>%
    dplyr::ungroup()
}

