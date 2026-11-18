# production  ==================================================================

# Test dataset
sample_data <- data.frame(
  site = 1,
  cum = c(1, 2),
  interval = 1,
  area = 10,
  group_id = 1
)

growth_data <- data.frame(
  group_id = 1,
  length = c(5, 10),
  interval = 1,
  interval_growth_pred = c(1, 2)
)

growth_array <- array(
  NA_real_,
  dim = c(nrow(growth_data), ncol(growth_data), 3),
  dimnames = list(
    NULL,
    names(growth_data),
    NULL
  )
)

for(i in 1:3) {
  growth_array[, , i] <- as.matrix(growth_data)
}

# Change predicted growth rates between bootstrap iterations
growth_array[, "interval_growth_pred", 1] <- c(0.9, 1.9)
growth_array[, "interval_growth_pred", 2] <- c(1.0, 2.0)
growth_array[, "interval_growth_pred", 3] <- c(1.1, 2.1)

biomass_data <- data.frame(
  site = c(1, 1, 1, 1),
  cum = c(1, 1, 2, 2),
  length = c(5, 10, 5, 10),
  wt = c(2, 6, 4, 8)
)

# Method and input validation  -------------------------------------------------

test_that("production only accepts a single value for species name",{
  expect_error(
    production(
      method = "igr",
      sp.name <- c(1:3),
      sample = data.frame(),
      biomass = data.frame(),
      growth = data.frame(),
      class.type = "length",
      class.size = 5
    ),
    'Please only provide one value for "sp.name".'
  )
})

test_that("production validates method", {

  expect_error(
    production(
      method = "foo",
      sample = data.frame(),
      biomass = data.frame(),
      class.type = "length",
      class.size = 5
    ),
    '"method" must be one of the following: "igr"'
  )
})

test_that("production requires growth data for IGR", {

  expect_error(
    production(
      method = "igr",
      sample = data.frame(),
      biomass = data.frame(),
      growth = NULL,
      class.type = "length",
      class.size = 5
    ),
    "Please provide data.frame or list 'growth'"
  )
})

test_that("production requires growth to be a data frame without growth bootstrapping", {

  expect_error(
    production(
      method = "igr",
      sample = data.frame(),
      biomass = data.frame(),
      growth = list(),
      class.type = "length",
      class.size = 5
    ),
    "When not bootstrapping growth rates"
  )
})


# Bootstrapping aurgument validation  ------------------------------------------

test_that("production validates bootstrap iterations", {

  expect_error(
    production(
      method = "igr",
      sample = data.frame(),
      biomass = data.frame(),
      growth = data.frame(),
      class.type = "length",
      class.size = 5,
      bio.boot = TRUE
    ),
    "Please provide numeric value 'iter'"
  )

  expect_error(
    production(
      method = "igr",
      sample = data.frame(),
      biomass = data.frame(),
      growth = array(1:8, dim = c(2, 2, 2)),
      class.type = "length",
      class.size = 5,
      growth.boot = TRUE
    ),
    "Please provide numeric value 'iter'"
  )
})

test_that("growth bootstrapping requires a three-dimensional array", {

  expect_error(
    production(
      method = "igr",
      sample = data.frame(),
      biomass = data.frame(),
      growth = data.frame(),
      class.type = "length",
      class.size = 5,
      growth.boot = TRUE,
      iter = 10
    ),
    "3-dimensional array"
  )
})

test_that("iter is ignored when bootstrapping is disabled", {

  expect_warning(
    production(
      method = "igr",
      sample = sample_data,
      biomass = biomass_data,
      growth = growth_data,
      class.type = "length",
      class.size = 5,
      iter = 10
    ),
    "Bootstrapping iterations"
  )
})

test_that("production requires iter to be a positive integer when bootstrapping", {

  invalid_iter <- list(
    "character" = "10",
    "logical" = TRUE,
    "multiple values" = c(1, 2),
    "missing" = NA_real_,
    "NaN" = NaN,
    "infinite" = Inf,
    "zero" = 0,
    "negative" = -1,
    "decimal" = 2.5
  )

  for(i in seq_along(invalid_iter)) {

    expect_error(
      production(
        method = "igr",
        sample = sample_data,
        biomass = biomass_data,
        growth = growth_data,
        class.type = "length",
        class.size = 5,
        bio.boot = TRUE,
        iter = invalid_iter[[i]]
      ),
      "'iter' must be a positive integer",
      info = names(invalid_iter)[i]
    )
  }
})

# Missing area tests  ----------------------------------------------------------

test_that("production assumes area of one when area is missing", {

  sample_no_area <- sample_data %>%
    dplyr::select(-area)

  expect_warning(
    result <- production(
      method = "igr",
      sample = sample_no_area,
      biomass = biomass_data,
      growth = growth_data,
      class.type = "length",
      class.size = 5
    ),
    "No vaule for 'area' provided"
  )

  expect_true(all(result$area == 1))
})

test_that("production does not warn when area is provided", {

  expect_no_warning(
    production(
      method = "igr",
      sample = sample_data,
      biomass = biomass_data,
      growth = growth_data,
      class.type = "length",
      class.size = 5
    )
  )
})


# Non-bootsrap result verification  --------------------------------------------

test_that("production estimates IGR production correctly", {

  result <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5,
    sp.name = "test"
  )

  expect_s3_class(result, "data.frame")

  expect_equal(nrow(result), 2)

  expect_true(all(c(
    "site",
    "cum",
    "production",
    "PtoB",
    "species"
  ) %in% names(result)))

  expect_equal(result$production[1], 1.7)
})

test_that("production summarizes estimates by site and sampling event", {

  result <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5
  )

  expect_equal(
    nrow(result),
    nrow(dplyr::distinct(sample_data, site, cum))
  )

  expect_false("class" %in% names(result))
})

test_that("production keeps estimates separated by site", {

  sample_data <- data.frame(
    site = rep(c(1, 2), each = 2),
    cum = rep(c(1, 2), 2),
    group_id = 1,
    interval = 1,
    area = 10
  )

  biomass_data <- data.frame(
    site = rep(c(1, 2), each = 4),
    cum = rep(c(1, 1, 2, 2), 2),
    length = rep(c(5, 10), 4),
    wt = c(2, 6, 4, 8, 20, 60, 40, 80)
  )

  growth_data <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  out <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5
  )

  expect_equal(sort(unique(out$site)), c(1, 2))

  site_production <- out %>%
    dplyr::filter(cum == 1) %>%
    dplyr::arrange(site)

  expect_equal(site_production$production, c(1.7, 17))
})

# Bootstrap esitamte validation  -----------------------------------------------

test_that("production supports biomass bootstrapping", {

  set.seed(123)

  result <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5,
    bio.boot = TRUE,
    iter = 10,
    sp.name = "test"
  )

  expect_s3_class(result, "data.frame")

  expect_equal(nrow(result), 2)

  expect_true(any(grepl(
    "biomass_",
    names(result)
  )))

  expect_true(any(grepl(
    "production_",
    names(result)
  )))

  expect_all_true(result$species == "test")

})

test_that("production supports growth bootstrapping", {

  set.seed(123)

  result <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_array,
    class.type = "length",
    class.size = 5,
    growth.boot = TRUE,
    iter = 10
  )

  expect_s3_class(result, "data.frame")

  expect_equal(nrow(result), 2)

  expect_true(any(grepl(
    "biomass_",
    names(result)
  )))

  expect_true(any(grepl(
    "production_",
    names(result)
  )))
})

test_that("production supports simultaneous biomass and growth bootstrapping", {

  set.seed(123)

  result <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_array,
    class.type = "length",
    class.size = 5,
    bio.boot = TRUE,
    growth.boot = TRUE,
    iter = 10
  )

  expect_s3_class(result, "data.frame")

  expect_equal(nrow(result), 2)

  expect_true(any(grepl(
    "biomass_",
    names(result)
  )))

  expect_true(any(grepl(
    "production_",
    names(result)
  )))
})


# Return raw result validation  ------------------------------------------------

test_that("production returns raw bootstrap estimates when requested", {

  set.seed(123)

  result <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5,
    bio.boot = TRUE,
    iter = 10,
    return.raw = TRUE
  )

  expect_type(result, "list")

  expect_named(
    result,
    c("production_summary", "raw_production")
  )

  expect_equal(length(result$raw_production), 10)

  expect_s3_class(
    result$production_summary,
    "data.frame"
  )
})

test_that("production returns raw estimates without bootstrapping", {

  result <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5,
    return.raw = TRUE
  )

  expect_type(result, "list")

  expect_named(
    result,
    c("production_summary", "raw_production")
  )

  expect_equal(length(result$raw_production), 1)
})

# Parallel processing  ---------------------------------------------------------

test_that("parallel and sequential production estimates are equivalent", {

  sequential <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5,
    parallel = FALSE
  )

  parallel_result <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5,
    parallel = TRUE,
    mc.cores = 2
  )

  expect_equal(
    sequential,
    parallel_result
  )
})


# Missing data cases  ----------------------------------------------------------
# MAYBE EVENTUALLY MOVE TO .igt
test_that("production handles sampling events with no biomass observations", {

  sample_data <- data.frame(
    site = 1,
    cum = c(1, 2),
    group_id = 1,
    interval = 1,
    area = 10
  )

  biomass_data <- data.frame(
    site = 1,
    cum = 1,
    length = c(5, 10),
    wt = c(2, 6)
  )

  growth_data <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  out <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5
  )

  missing_event <- dplyr::filter(out, cum == 2)

  expect_equal(missing_event$sample_den, 0)
  expect_equal(missing_event$sample_biomass, 0)
})

test_that("production handles missing biomass observations within a sample", {

  sample_data <- data.frame(
    site = 1,
    cum = c(1, 2),
    group_id = 1,
    interval = 1,
    area = 10
  )

  biomass_data <- data.frame(
    site = 1,
    cum = c(1, 1, 2),
    length = c(5, 10, 5),
    wt = c(2, 6, 4)
  )

  growth_data <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  out <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5
  )

  first_event <- dplyr::filter(out, cum == 1)

  # Two observed fish across an area of 10
  expect_equal(first_event$sample_den, 0.2)

  # Mean weight = 4; density = 0.2
  expect_equal(first_event$sample_biomass, 0.8)
})

test_that("production does not convert missing growth estimates to zero", {

  sample_data <- data.frame(
    site = 1,
    cum = c(1, 2),
    group_id = 1,
    interval = 1,
    area = 10
  )

  biomass_data <- data.frame(
    site = rep(1, 4),
    cum = c(1, 1, 2, 2),
    length = c(5, 10, 5, 10),
    wt = c(2, 6, 4, 8)
  )

  growth_data <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, NA_real_)
  )

  suppressWarnings(out <- production(
    method = "igr",
    sample = sample_data,
    biomass = biomass_data,
    growth = growth_data,
    class.type = "length",
    class.size = 5
  ))

  first_event <- dplyr::filter(out, cum == 1)

  expect_true(is.na(first_event$production))

})
