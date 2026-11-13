#-------------------------------------------------------------------------------
# General helper functions
#-------------------------------------------------------------------------------

# Draw random samples from posterior distribution of growth curve parameters
# from a 3d array (groupings present) or matrix (no groupings). Returns a list
# of growth parameter data.frames (col = parameter, row = groups). Each
# data.frame is a random draw from posterior distribution

.post_draw <- function (model.out,n.sim){

  if(!is.numeric(n.sim) ||
     length(n.sim) != 1 ||
     is.na(n.sim) ||
     !is.finite(n.sim) ||
     n.sim < 1 ||
     n.sim != floor(n.sim)) {
    stop("'n.sim' must be a positive integer")
  }

  if(!is.matrix(model.out) && length(dim(model.out)) != 3) {
    stop("'model.out' must be a matrix or a 3-dimensional array")
  }

  # For outputs without group specific parameters (matrix),
  # each row is a posterior draw
  if(is.na(dim(model.out)[3])){

    # How many iterations?
    n.iter = nrow(model.out)

    if(n.iter < 1) {
      stop("'model.out' must contain at least one posterior draw")
    }


    # Random draws
    s <- sample(1:n.iter,n.sim,replace = T)

    # Create a list of random draws of growth parameters
    mod_list <- lapply(s,function(x) as.data.frame(model.out)[x,,drop = F])
  } else {

    # For outputs with group specific parameters (arrays),
    # each slice is a posterior draw

    # How many iterations?
    n.iter = dim(model.out)[3]

    if(n.iter < 1) {
      stop("'model.out' must contain at least one posterior draw")
    }


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

  # Validate boot_list
  if(!is.list(boot_list) || length(boot_list) == 0) {
    stop("'boot_list' must be a non-empty list")
  }

  if(!all(vapply(boot_list, is.data.frame, logical(1)))) {
    stop("All elements of 'boot_list' must be data.frames")
  }

  # Validate dimensions
  dims <- lapply(boot_list, dim)

  if(!all(vapply(dims, identical, logical(1), dims[[1]]))) {
    stop("All data.frames in 'boot_list' must have the same dimensions")
  }

  # Validate column names and order
  col_names <- lapply(boot_list, names)

  if(!all(vapply(col_names, identical, logical(1), col_names[[1]]))) {
    stop("All data.frames in 'boot_list' must have identical column names in the same order")
  }

  # Validate grouping variables
  if(!is.character(group.var) ||
     length(group.var) == 0 ||
     anyNA(group.var) ||
     any(!nzchar(group.var))) {
    stop("'group.var' must be a non-empty character vector")
  }

  if(!all(group.var %in% names(boot_list[[1]]))) {
    stop("All 'group.var' variables must be present in the boot_list data.frames")
  }

  # Check that grouping variables have the same combinations and counts
  group_structure <- lapply(boot_list, function(x) {
    x %>%
      dplyr::count(
        dplyr::across(dplyr::all_of(group.var)),
        name = ".n"
      ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(group.var))) %>%
      as.data.frame()
  })

  if(!all(vapply(
    group_structure,
    identical,
    logical(1),
    group_structure[[1]]
  ))) {
    stop("All data.frames in 'boot_list' must contain the same grouping-variable combinations and counts")
  }


  safe_stat <- function(x, fun, ...) {
    if(all(is.na(x))) {
      return(NA_real_)
    }

    fun(x, na.rm = TRUE, ...)
  }

  # Combine list of boot draws into one data.frame and summarize,
  dplyr::bind_rows(boot_list) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group.var))
    ) %>%
    dplyr::summarize(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = list(
          mean = \(x) safe_stat(x, mean),
          sd = \(x) safe_stat(x, sd),
          med = \(x) safe_stat(x, median),
          lwr = \(x) safe_stat(x, quantile, probs = ci[1]),
          upr = \(x) safe_stat(x, quantile, probs = ci[2])
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
      n = dplyr::n(),
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
