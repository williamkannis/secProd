#-------------------------------------------------------------------------------
# General production helper functions
#-------------------------------------------------------------------------------

.check_input_data <- function(df, type) {

  if(!type %in% c("sample","biomass", "growth", "class_df")) {
    stop('type must be "sample","biomass","growth", or "class_df')
  }

  if(type == "sample") {

    if(any(!c("site" ,"cum", "interval","area", "group_id") %in% names(df))) {
      stop('"sample" must contain columns "site" ,"cum", "interval","area", ',
      'and "group_id"')
    }

    if(nrow(df %>%
            dplyr::distinct(site ,cum)) != nrow(df)) {
      stop('"sample" contains duplicated entries')
    }
    # EVENTAULY SPLIT THIS TO SAY IF AREA OR INTERVAL IS DUPLICATED

    if(all(is.na(df$area))) {
      stop('"area" has all NA values in "sample"')
    }

    if(all(is.na(df$interval))) {
      stop('"interval" has all NA values in "sample"')
    }

    if(any(is.na(df$area))) {
      warning(
        'NA values present for "area" in "sample". This will result in NAs ',
        'for density, biomass, and production')
    }

    if(any(is.na(df$interval))) {
      warning(
        'NA values present for some "intervals" in "sample". This will result ',
        'in NA for production')
    }

    # missing group_id
    if(any(is.na(df$group_id))) {
      warning(
        'NA values present for "group_id" in "sample". These values will not ',
        'be assigned a growth rate and will result in NA production estimate.')
    }

    if(any(is.na(df %>% dplyr::select(cum,site)))) {
      warning(
        'NA values present for "cum" and "site" in "sample". ',
        'These groupings will be ignored'
      )
    }

  }

  if(type == "biomass") {

    if(any(!c("cum", "site", "length", "wt") %in% names(df))) {
      stop('"biomass" must contain columns "cum", "site", "length", and "wt"')
    }

    if(any(all(is.na(df$length)),all(is.na(df$wt)))) {
      stop('"length" and/or "wt" have all NA values in "biomass"')
    }

    if(any(!is.numeric(df$length),!is.numeric(df$wt))) {
      stop('"length" and "wt" must be provided as numeric in "biomass"')
    }

    if(any(is.na(df %>% dplyr::select(cum,site,length)))) {
      warning(
        'NA values present for "cum", "site", and "length" in "biomass". ',
        'These groupings will be ignored'
      )
    }

    if(any(!is.na(df$length) & is.na(df$wt))) {
      warning(
        'NA values present for "wt" in "biomass", these lines will be treated ',
        'as no individual present'
        )

      # stop(
      #   'Length present but wt missing for at least one entry. Please ensure',
      #   ' all individuals have a weight'
      # )
    }

  }

  if(type == "growth") {


    if(any(!c("group_id" ,"length", "interval","interval_growth_pred") %in% names(df))) {
      stop('"growth" must contain columns "group_id" ,"length", "interval", ',
      'and "interval_growth_pred"')
    }

    if(any(!is.numeric(df$length),!is.numeric(df$interval_growth_pred))) {
      stop('"length" and "interval_growth_pred" must be provided as numeric ',
      'in "growth"')
    }

    if(any(!is.na(df$interval_growth_pred) & df$interval_growth_pred < 0)) {
      stop('"interval_growth_pred" must be postive')
    }

    if(any(is.na(df$group_id))) {
      warning(
        'NA values present for "group_id" in "growth", these growth rates will be ignored'
      )
    }

    if(any(is.na(df$interval))) {
      warning(
        'NA values present for "interval" in "growth", these growth rates will be ignored'
      )
    }

    if(any(is.na(df$length))) {
      warning(
        'NA values present for "length" in "growth", these growth rates will be ignored'
      )
    }

    if(any(is.na(df$interval_growth_pred))) {
      warning(
        'NA values present for "interval_growth_pred" in "growth"'
      )
    }


  }

  if(type == "class_df") {

    # UPDATE IN FUTURE TO INCLUDE AGE

    if(any(!c("length", "class") %in% names(df))) {
      stop('"class_df" must contain columns "length" and "class"')
    }

    if(any(is.na(df))) {
      stop("'class_df contains NA values for atleast one column")
    }

  }

}

# Separate data into size or age classes for class specific production estimates

.class_fun <- function(input,class.type,class.size) {

  # Error messages
  if(!class.type %in% c("age","length")){
    stop('class.type must be "age" or "length"')
  }
  if(!(is.numeric(class.size) && class.size > 0 && class.size %% 1 == 0)) {
    stop("class.size must be postive interger")
  }

  if(all(is.na(input))) {
    stop(
      'No values present for "input"'
    )
  }

  if(any(is.na(input))) {
    warning(
      'NA values present for "input", resulting size classes will be ignored'
    )
  }

  class_df <- data.frame(
   input = input,
   class = floor(input/class.size)
   ) %>%
    dplyr::filter(!is.na(input)) %>%
    dplyr::distinct(input,class)

  colnames(class_df) <- c(class.type, "class")

  class_df

}


#-------------------------------------------------------------------------------
# Summarize biomass at age class, using sample data to record instance with no
# individuals. These will be presented with NaN wt and can be changed to zero
.class_bio_sum <- function(sample,class_df,biomass) {

  # Check input data
  .check_input_data(sample, "sample")
  .check_input_data(class_df, "class_df")
  .check_input_data(biomass, "biomass")

  # Check for data overlap
  if(any(!unique(biomass$length[!is.na(biomass$length)]) %in% unique(class_df$length))) {
    stop('"class_id" does not contain all lengths in "biomass')
  }
  if(
    biomass %>%
    dplyr::filter(
      !is.na(cum),
      !is.na(site),
      !is.na(length)
    ) %>%
    dplyr::anti_join(
      sample,
      by = dplyr::join_by(site,cum)
    ) %>%
    nrow() != 0
  ) {


    warning('Some site and cum values from "biomass" are missing ',
            'from "sample". This will prevent biomass data to be assigned an ',
            'age class or growth rate, and will be removed')
  }

  # if site/cum from sample not in biomass, that means no individual present
  # no warning needed

  # Assign each sample each length grouping
  # (this is essential to preserving absences)
  sample_class <- sample %>%
  dplyr::filter(
    !is.na(cum),
    !is.na(site),
  ) %>%
  dplyr::cross_join(
    class_df
  )

  # Add biomass data to sample data (full join could create NA size classes)
  sample_class %>%
    dplyr::left_join(
      biomass,
      by = c("cum", "site", "length")
    ) %>%

  # Calculate class means
    dplyr::group_by(site,cum,class,group_id,interval,area) %>%
    dplyr::summarize(
      mean_mass=mean(wt,na.rm=T), # UPDATE IN THE FUTURE TO SEPERATE JOIN NAS (ABSENCE) FROM MISSING WEIGHTS
      n=sum(!is.na(wt)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(mean_mass = dplyr::case_when(
      is.nan(mean_mass) ~ 0,
      T~ mean_mass
    ))

}


#-------------------------------------------------------------------------------
# Summarize growth rates per class

.class_growth_sum <- function(growth,class_df) {

  .check_input_data(growth, "growth")
  .check_input_data(class_df, "class_df")

  # Remove growth rares with missing groupings
  growth_for <- growth %>%
    dplyr::filter(
      !is.na(length),
      !is.na(group_id),
      !is.na(interval)  # RETHINK THIS. WORKS FOR NOW
    )

  # Check that data.frame matches
    if(growth_for %>%
      dplyr::anti_join(
        class_df,
        by = "length"
      ) %>%
      nrow() !=0) {
      stop(
        'Some values for "length" from "growth" are missing ',
        'from "class_df"')
    }

  # Attach growth rate to size classes
  df <- growth_for %>%
    dplyr::left_join(
      class_df,
      by = c("length")
    ) %>%

    # Estimate mean growth per size class
    dplyr::group_by(group_id,interval,class) %>%
    dplyr::summarize(
      mean_growth=mean(interval_growth_pred),
      .groups = "drop"
    )

  if(any(is.na(df$mean_growth))){
    warning("Some size classes are missing growth rates. NA production will be returned")
  }
  df
}


#-------------------------------------------------------------------------------
# Class specific production via instantaneous growth rate method

.class_igr <- function(input) {

  # CHECK INPUT DATA

  input %>%
    dplyr::group_by(site,class) %>%
    dplyr::mutate(

      # Estimate individual and interval biomass for production estimate
      # Do not assign lead biomass if consecutive samples are not taken
      biomass = (n*mean_mass)/area,
      lead_biomass = dplyr::case_when(
        cum+1 != dplyr::lead(cum,order_by = cum) ~NA,
        T~ dplyr::lead(biomass,order_by = cum)
      ),
      interval_biomass = (biomass+lead_biomass)/2,
      production = interval_biomass*mean_growth
    ) %>%
    dplyr::ungroup()
}


#-------------------------------------------------------------------------------
# Summarize class-specific production and friends to the a specified grouping

.prod_sum <- function(input){

  # CHECK INPUT DATA

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
        dplyr::everything(),
        # \(x) sum(x, na.rm = TRUE)  ## NARM TURNS NAS INTO ZEROS< THIS ISNT IDEAL IN ALL CASES
        # \(x) if_else(all(is.na(x)), NA_real_, sum(x)),
        sum
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(PtoB = production/interval_biomass) %>%
    dplyr::ungroup()
}

# WHAT TO DO IF NA IS PRESENT IN ONE CLASS BUT NOT OTHERS (MISSING GROWTH DATA)
# MISSING LENGTH DATA IS TRESTED AS ABSENCE OF INDIVIUDALS
# MAYBE HAVE ARGUMENT TO REMOVE NA PRODUCTION SIZE CLASS
# RIGHT NOW MISSING PRODUCTION SIZE CLASS RESULTS IN NA PRODUCTION FOR WHOLE SAMPLE
# THIS IS MOST CONSERVATIVE APPROACH

# old code

# .class_fun <- function(input,class.type,class.size) {
#
#   # Error messages
#   if(!class.type %in% c("age","size")){
#     stop('class.type must be "age" or "size"')
#   }
#   if(!(is.numeric(class.size) && class.size > 0 && class.size %% 1 == 0)) {
#     stop("class.size must be postive interger")
#   }
#
#   if(any(!c("length") %in% names(input))) {
#     stop('input must contain columns "length"')
#   }
#
#   if(all(is.na(input$length))) {
#     stop(
#       'No values present for "length"'
#     )
#   }
#
#   if(any(is.na(input$length))) {
#     warning(
#       'NA values present for "length", resulting size classes will be ignored'
#     )
#   }
#
#   if(class.type == "age" && !"age_pred" %in% names(input)) {
#     stop('input must contain column "age_pred"')
#   }
#
#   if(class.type == "age" && all(is.na(input$age_pred))) {
#     stop(
#       'No values present for "age_pred"'
#     )
#   }
#
#   if(class.type == "age" && any(is.na(input$age_pred))) {
#     warning(
#       'NA values present for "age_pred", resulting age classes will be ignored'
#       )
#   }
#
#   #  class type
#   if(class.type == "size") input$class_input <- input$length
#   if(class.type == "age") input$class_input <- input$age_pred
#
#   # Create class groupings
#   class_df <- input %>%
#     dplyr::mutate(class = floor(class_input/class.size)) %>%
#     dplyr::filter(
#       !is.na(length),
#       !is.na(class_input),
#       !is.na(class)
#     ) %>%
#     dplyr::distinct(
#       # group_id,
#       length,
#       class
#       )
#
#
#   # # Check that all groups have the same groups
#   # group <- unique(class_df$group_id)
#   # class_test <-lapply(group, function(x){
#   #   class_df %>%
#   #     dplyr::filter(group_id == x) %>%
#   #     dplyr::distinct(class) %>%
#   #     dplyr::pull(class)
#   # })
#   # if(length(unique(class_test)) != 1){
#   #   stop("Growth groups do not have the same size/age classes")
#   # }
#
#   return(class_df)
#
#   ## UPDATE FUNCTION IN FUTURE TO ACCEPT AGE CLASS
# }
