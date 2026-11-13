# .post_draw  ==================================================================
test_that(".post_draw requires n.sim to be a positive integer", {

  invalid_n <- list(
    "zero" = 0,
    "negative" = -1,
    "decimal" = 1.5,
    "character" = "5",
    "logical" = TRUE,
    "missing" = NA_real_,
    "NaN" = NaN,
    "infinite" = Inf,
    "multiple values" = c(1, 2),
    "empty" = numeric(0)
  )

  model_out <- matrix(
    1:6,
    nrow = 3,
    ncol = 2
  )

  for(i in seq_along(invalid_n)) {
    expect_error(
      .post_draw(model_out, invalid_n[[i]]),
      "'n.sim' must be a positive integer",
      info = names(invalid_n)[i]
    )
  }
})

test_that(".post_draw requires a matrix or 3-dimensional array", {

  expect_error(
    .post_draw(data.frame(a = 1:3), 1),
    "'model.out' must be a matrix or a 3-dimensional array"
  )

  expect_error(
    .post_draw(1:5, 1),
    "'model.out' must be a matrix or a 3-dimensional array"
  )
})

test_that(".post_draw requires at least one posterior draw", {

  model_out <- matrix(numeric(0), nrow = 0, ncol = 2)

  expect_error(
    .post_draw(model_out, 1),
    "must contain at least one posterior draw"
  )
})

test_that(".post_draw preserves matrix structure with one parameter", {

  model_out <- matrix(
    c(1, 2, 3),
    ncol = 1,
    dimnames = list(NULL, "growth")
  )

  out <- .post_draw(model_out, 3)

  expect_length(out, 3)

  for(i in seq_along(out)) {
    expect_s3_class(out[[i]], "data.frame")
    expect_equal(dim(out[[i]]), c(1, 1))
    expect_equal(names(out[[i]]), "growth")
  }
})

test_that(".post_draw preserves array structure with one group and parameter", {

  model_out <- array(
    c(1, 2, 3),
    dim = c(1, 1, 3),
    dimnames = list("group1", "growth", NULL)
  )

  out <- .post_draw(model_out, 3)

  expect_length(out, 3)

  for(i in seq_along(out)) {
    expect_s3_class(out[[i]], "data.frame")
    expect_equal(dim(out[[i]]), c(1, 1))
    expect_equal(rownames(out[[i]]), "group1")
    expect_equal(names(out[[i]]), "growth")
  }
})


test_that(".post_draw returns the requested number of draws from a matrix", {

  model_out <- matrix(
    c(1, 2, 3, 4, 5, 6),
    nrow = 3,
    dimnames = list(NULL, c("a", "b"))
  )

  set.seed(1)
  out <- .post_draw(model_out, n.sim = 5)

  expect_type(out, "list")
  expect_length(out, 5)

  for(i in seq_along(out)) {
    expect_s3_class(out[[i]], "data.frame")
    expect_equal(names(out[[i]]), c("a", "b"))
    expect_equal(nrow(out[[i]]), 1)
    expect_true(all(unlist(out[[i]]) %in% model_out))
  }
})


test_that(".post_draw returns the requested number of draws from a 3D array", {

  model_out <- array(
    NA_real_,
    dim = c(2, 2, 3),
    dimnames = list(
      c("group1", "group2"),
      c("a", "b"),
      NULL
    )
  )

  model_out[, , 1] <- matrix(c(1, 2, 3, 4), nrow = 2)
  model_out[, , 2] <- matrix(c(5, 6, 7, 8), nrow = 2)
  model_out[, , 3] <- matrix(c(9, 10, 11, 12), nrow = 2)

  set.seed(1)
  out <- .post_draw(model_out, n.sim = 5)

  expect_type(out, "list")
  expect_length(out, 5)

  for(i in seq_along(out)) {
    expect_s3_class(out[[i]], "data.frame")
    expect_equal(dim(out[[i]]), c(2, 2))
    expect_equal(rownames(out[[i]]), c("group1", "group2"))
    expect_equal(names(out[[i]]), c("a", "b"))
    expect_true(all(as.matrix(out[[i]]) %in% model_out))
  }
})

# .boot_summary  ===============================================================

# Input validation  ------------------------------------------------------------

test_that(".boot_summary accepts valid bootstrap data.frames", {

  boot_list <- list(
    data.frame(site = c(1, 1, 2), cum = c(1, 2, 1),
               production = c(10, 20, 30)),
    data.frame(site = c(1, 1, 2), cum = c(1, 2, 1),
               production = c(12, 22, 32)),
    data.frame(site = c(1, 1, 2), cum = c(1, 2, 1),
               production = c(14, 24, 34))
  )

  expect_no_error(
    .boot_summary(boot_list, group.var = c("site", "cum"))
  )
})

test_that(".boot_summary requires all boot_list elements to be data.frames", {

  boot_list <- list(
    data.frame(site = 1, production = 10),
    matrix(c(1, 20), nrow = 1)
  )

  expect_error(
    .boot_summary(boot_list, group.var = "site"),
    "All elements of 'boot_list' must be data.frames"
  )
})

test_that(".boot_summary requires identical data.frame dimensions", {

  boot_list <- list(
    data.frame(site = c(1, 2), production = c(10, 20)),
    data.frame(site = 1, production = 15)
  )

  expect_error(
    .boot_summary(boot_list, group.var = "site"),
    "same dimensions"
  )
})

test_that(".boot_summary requires identical column names and order", {

  boot_list <- list(
    data.frame(site = 1, cum = 1, production = 10),
    data.frame(site = 1, production = 15, cum = 1)
  )

  expect_error(
    .boot_summary(boot_list, group.var = c("site", "cum")),
    "identical column names"
  )
})

test_that(".boot_summary validates group.var", {

  boot_list <- list(
    data.frame(site = 1, production = 10),
    data.frame(site = 1, production = 15)
  )

  expect_error(
    .boot_summary(boot_list, group.var = NULL),
    "'group.var' must be a non-empty character vector"
  )

  expect_error(
    .boot_summary(boot_list, group.var = "missing"),
    "group.var.*must be present"
  )

  expect_error(
    .boot_summary(boot_list, group.var = c("site", NA_character_)),
    "'group.var' must be a non-empty character vector"
  )
})

test_that(".boot_summary requires identical grouping combinations", {

  boot_list <- list(
    data.frame(
      site = c(1, 1, 2),
      cum = c(1, 2, 1),
      production = c(10, 20, 30)
    ),
    data.frame(
      site = c(1, 1, 3),
      cum = c(1, 2, 1),
      production = c(12, 22, 32)
    )
  )

  expect_error(
    .boot_summary(boot_list, group.var = c("site", "cum")),
    "same grouping-variable combinations and counts"
  )
})

test_that(".boot_summary requires identical numbers of rows per group", {

  boot_list <- list(
    data.frame(
      site = c(1, 1, 2),
      cum = c(1, 1, 1),
      production = c(10, 12, 30)
    ),
    data.frame(
      site = c(1, 2, 2),
      cum = c(1, 1, 1),
      production = c(14, 32, 34)
    )
  )

  expect_error(
    .boot_summary(boot_list, group.var = c("site", "cum")),
    "same grouping-variable combinations and counts"
  )
})

test_that(".boot_summary rejects empty boot_list", {

  expect_error(
    .boot_summary(list(), group.var = "site"),
    "'boot_list' must be a non-empty list"
  )
})


# result validation  -----------------------------------------------------------

test_that(".boot_summary calculates summary statistics by group", {

  boot_list <- list(
    data.frame(
      site = c(1, 1), cum = c(1, 2), production = c(1, 4)
    ),
    data.frame(
      site = c(1, 1), cum = c(1, 2), production = c(2, 5)
    ),
    data.frame(
      site = c(1, 1), cum = c(1, 2), production = c(3, 6)
    )
  )

  out <- .boot_summary(boot_list, group.var = c("site", "cum"))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)

  expect_true(all(c(
    "production_mean",
    "production_sd",
    "production_med",
    "production_lwr",
    "production_upr"
  ) %in% names(out)))

  expect_equal(out$production_mean, c(2, 5))
  expect_equal(out$production_med, c(2, 5))
  expect_equal(out$production_sd, c(1, 1))

  expect_equal(
    out$production_lwr,
    c(quantile(1:3, 0.0275), quantile(4:6, 0.0275))
  )

  expect_equal(
    out$production_upr,
    c(quantile(1:3, 0.975), quantile(4:6, 0.975))
  )
})

test_that(".boot_summary uses the supplied confidence interval", {

  boot_list <- lapply(1:10, function(x) {
    data.frame(
      site = 1,
      production = x
    )
  })

  out <- .boot_summary(
    boot_list,
    group.var = "site",
    ci = c(0.10, 0.90)
  )

  expect_equal(
    unname(out$production_lwr),
    unname(quantile(1:10, 0.10))
  )

  expect_equal(
    unname(out$production_upr),
    unname(quantile(1:10, 0.90))
  )
})

test_that(".boot_summary preserves separate groups", {

  boot_list <- list(
    data.frame(
      site = c(1, 1, 2), cum = c(1, 2, 1),
      production = c(1, 10, 20)
    ),
    data.frame(
      site = c(1, 1, 2), cum = c(1, 2, 1),
      production = c(3, 12, 22)
    )
  )

  out <- .boot_summary(boot_list, group.var = c("site", "cum"))

  expect_equal(nrow(out), 3)
  expect_equal(out$production_mean, c(2, 11, 21))
})

test_that(".boot_summary handles groups with all missing values", {

  boot_list <- list(
    data.frame(
      site = c(1, 2),
      production = c(1, NA_real_)
    ),
    data.frame(
      site = c(1, 2),
      production = c(3, NA_real_)
    )
  )

  out <- .boot_summary(boot_list, group.var = "site")

  expect_equal(out$production_mean[1], 2)
  expect_true(is.na(out$production_mean[2]))

  expect_true(is.na(out$production_sd[2]))
  expect_true(is.na(out$production_med[2]))
  expect_true(is.na(out$production_lwr[2]))
  expect_true(is.na(out$production_upr[2]))
})

test_that(".boot_summary ignores missing values when calculating statistics", {

  boot_list <- list(
    data.frame(site = 1, production = 1),
    data.frame(site = 1, production = NA_real_),
    data.frame(site = 1, production = 3)
  )

  out <- .boot_summary(boot_list, group.var = "site")

  expect_equal(out$production_mean, 2)
  expect_equal(out$production_med, 2)
  expect_equal(out$production_sd, sqrt(2))

  expect_equal(
    unname(out$production_lwr),
    unname(quantile(c(1, 3), 0.0275))
  )

  expect_equal(
    unname(out$production_upr),
    unname(quantile(c(1, 3), 0.975))
  )
})

test_that(".boot_summary handles groups with one non-missing value", {

  boot_list <- list(
    data.frame(site = 1, production = 5),
    data.frame(site = 1, production = NA_real_),
    data.frame(site = 1, production = NA_real_)
  )

  out <- .boot_summary(boot_list, group.var = "site")

  expect_equal(unname(out$production_mean), 5)
  expect_true(is.na(out$production_sd))
  expect_equal(unname(out$production_med), 5)
  expect_equal(unname(out$production_lwr), 5)
  expect_equal(unname(out$production_upr), 5)
})

test_that(".boot_summary handles missing values with multiple grouping variables", {

  boot_list <- list(
    data.frame(
      site = c(1, 1, 2),
      cum = c(1, 2, 1),
      production = c(1, NA_real_, 10)
    ),
    data.frame(
      site = c(1, 1, 2),
      cum = c(1, 2, 1),
      production = c(3, NA_real_, 12)
    )
  )

  out <- .boot_summary(
    boot_list,
    group.var = c("site", "cum")
  )

  expect_equal(nrow(out), 3)
  expect_equal(out$production_mean, c(2, NA_real_, 11))
})


# .biomass_summary  ============================================================

test_that(".biomass_summary calculates sample density and biomass", {

  biomass <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    wt = c(2, 4, 3, 5)
  )

  sample <- data.frame(
    site = c(1, 1),
    cum = c(1, 2),
    area = c(2, 4)
  )

  out <- .biomass_summary(
    biomass = biomass,
    sample = sample,
    group.var = c("site", "cum")
  )

  expect_equal(out$sample_den, c(1, 0.5))
  expect_equal(out$sample_biomass, c(3, 2))
})


test_that(".biomass_summary assigns zero density and biomass to samples with no fish", {

  biomass <- data.frame(
    site = 1,
    cum = 1,
    wt = 4
  )

  sample <- data.frame(
    site = c(1, 1),
    cum = c(1, 2),
    area = c(2, 2)
  )

  out <- .biomass_summary(
    biomass = biomass,
    sample = sample,
    group.var = c("site", "cum")
  )

  expect_equal(out$sample_den, c(0.5, 0))
  expect_equal(out$sample_biomass, c(2, 0))
})


test_that(".biomass_summary calculates values independently by site", {

  biomass <- data.frame(
    site = c(1, 1, 2, 2),
    cum = c(1, 1, 1, 1),
    wt = c(2, 4, 10, 20)
  )

  sample <- data.frame(
    site = c(1, 2),
    cum = c(1, 1),
    area = c(2, 4)
  )

  out <- .biomass_summary(
    biomass = biomass,
    sample = sample,
    group.var = c("site", "cum")
  )

  expect_equal(out$sample_den, c(1, 0.5))
  expect_equal(out$sample_biomass, c(3, 7.5))
})
