#-------------------------------------------------------------------------------
# General helper functions
#-------------------------------------------------------------------------------

# Draw random samples from posterior distribution of growth curve parameters
# from a 3d array (groupings present) or matrix (no groupings). Returns a list
# of growth parameter data.frames (col = parameter, row = groups). Each
# data.frame is a random draw from posterior distribution

.post_draw <- function (model.out,n.sim){

  # For outputs without group specific parameters (matrix),
  # each row is a posterior draw
  if(is.na(dim(model.out)[3])){

    # How many iterations?
    n.iter = nrow(model.out)

    # Random draws
    s <- sample(1:n.iter,n.sim,replace = T)

    # Create a list of random draws of growth parameters
    mod_list <- lapply(s,function(x) as.data.frame(model.out[x,]))
  } else {

    # For outputs with group specific parameters (arrays),
    # each slice is a posterior draw

    # How many iterations?
    n.iter = dim(model.out)[3]

    # Random draws
    s <- sample(1:n.iter,n.sim,replace = T)

    # Create a list of random draws of growth parameters
    mod_list <- lapply(s, function(x) {
      as.data.frame(matrix(
        model.out[ , , x],
        nrow = dim(model.out)[1],
        dimnames = dimnames(model.out)[1:2]
      ))
    })
  }
  mod_list
}

#-------------------------------------------------------------------------------
# Summarize data in lists created in bootstrapping sampling. Returns data.frame
# containing group specific summary stats including 95% confidence or credible
# intervals

.boot_summary <- function(
    boot_list,
    group.var,
    ci=c(0.0275,0.975)
) {

  # TEST THAT DATA>FRAMES ARE THE SAME ROWS, COLUMNS, AND GROUPING

  # Combine list of boot draws into one data.frame and summarize,
  dplyr::bind_rows(boot_list) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group.var))
      ) %>%
    dplyr::summarize(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = list(
          mean = \(x) mean(x, na.rm = TRUE),
          sd = \(x) sd(x, na.rm = TRUE),
          med = \(x) median(x, na.rm = TRUE),
          lwr = \(x) quantile(x,probs=ci[1], na.rm=T),
          upr = \(x) quantile(x,probs=ci[2], na.rm=T)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}


# ------------------------------------------------------------------------------
# Summarize non-bootstraped sample density and biomass and data.

.biomass_summary <- function(biomass,sample,group.var){
  df <- biomass %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group.var))) %>%
    dplyr::summarise(
      n = n(),
      mean_mass = mean(wt,na.rm=T),
      .groups = "drop"
    ) %>%
    dplyr::right_join(sample,by=group.var) %>%
    dplyr::mutate(
      sample_den = n/area,
      sample_biomass = mean_mass*sample_den
    ) %>%
    dplyr::select(-n,-mean_mass)

  # assign samples with no fish density and biomass of zero
  df[is.na(df$sample_den),c("sample_den","sample_biomass")] <- 0
  df
}
