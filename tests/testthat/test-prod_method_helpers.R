# .igr  ========================================================================

# Input validation  ------------------------------------------------------------

test_that("IGR fails if growth or biomass are missing column used in size class",{

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    age = c(5,10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 2),
    interval = 1,
    area = 10,
    group_id = 1
  )

  biomass <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    length = c(5, 10, 5, 10),
    age = c(5,10),
    wt = c(2, 6, 4, 8)
  )

  for (j in c("age","length")){

    growth_wrong <- growth
    colnames(growth_wrong)[colnames(growth_wrong) == j] <- "wrong"

    biomass_wrong <- biomass
    colnames(biomass_wrong)[colnames(biomass_wrong) == j] <- "wrong"

    growth_error <- paste0('"growth" does not contain column for ',j)
    biomass_error <- paste0('"biomass" does not contain column for ',j)

    expect_error(
      .igr(
        growth = growth_wrong,
        biomass = biomass,
        sample = sample,
        class.type = j,
        class.size = 5
      ),
      growth_error
    )

    expect_error(
      .igr(
        growth = growth,
        biomass = biomass_wrong,
        sample = sample,
        class.type = j,
        class.size = 5
      ),
      biomass_error
    )

  }

})

test_that('IGR warns that lengths from biomass are missing in length',{

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 2),
    interval = 1,
    area = 10,
    group_id = 1
  )

  biomass <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    length = c(5, 15, 5, 10),
    wt = c(2, 6, 4, 8)
  )

  expect_warning(
    result <- .igr(
      growth = growth,
      biomass = biomass,
      sample = sample,
      class.type = "length",
      class.size = 5
    ),
    'Some size classes in biomass are missing growth rates. NA production will be returned'
  )

  expect_equal(nrow(result), 2)

  expect_equal(result$biomass, c(0.8, 1.2))
  expect_equal(result$interval_biomass[1], 1)

  expect_true(all(is.na(result$production)))
  expect_true(all(is.na(result$PtoB)))


})


# Result validiation  ----------------------------------------------------------

test_that("IGR assigns species name if given", {

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 2),
    interval = 1,
    area = 10,
    group_id = 1
  )

  biomass <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    length = c(5, 10, 5, 10),
    wt = c(2, 6, 4, 8)
  )

  result <- .igr(
    growth = growth,
    biomass = biomass,
    sample = sample,
    class.type = "length",
    class.size = 5,
    sp.name = "test"
  )
  expect_all_true(result$species == "test")

  result2 <- .igr(
    growth = growth,
    biomass = biomass,
    sample = sample,
    class.type = "length",
    class.size = 5
  )

  expect_all_true(colnames(result2) != "species")

  expect_error(
    .igr(
      growth = growth,
      biomass = biomass,
      sample = sample,
      class.type = "length",
      class.size = 5,
      sp.name = c("test1","test2")
    ),
    'more than one value for sp.name provided'
  )


})

test_that("IGR estimates class-specific production correctly", {

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 2),
    interval = 1,
    area = 10,
    group_id = 1
  )

  biomass <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    length = c(5, 10, 5, 10),
    wt = c(2, 6, 4, 8)
  )

  result <- .igr(
    growth = growth,
    biomass = biomass,
    sample = sample,
    class.type = "length",
    class.size = 5
  )

  expect_equal(nrow(result), 2)

  expect_equal(result$biomass, c(0.8, 1.2))
  expect_equal(result$interval_biomass[1], 1)
  expect_equal(result$production[1], 1.7)
  expect_equal(result$PtoB[1], 1.7)

  expect_true(is.na(result$production[2]))
})

test_that('IGR produces NA production when next cum is missing in sample', {

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 3, 4),
    interval = 1,
    area = 10,
    group_id = 1
  )

  biomass <- data.frame(
    site = 1,
    cum = c(1, 1, 3, 3, 4, 4),
    length = c(5, 10, 5, 10, 5, 10),
    wt = c(2, 6, 4, 8, 4, 6)
  )

  result <- .igr(
    growth = growth,
    biomass = biomass,
    sample = sample,
    class.type = "length",
    class.size = 5
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$biomass, c(0.8, 1.2, 1))
  expect_equal(result$interval_biomass, c(NA, 1.1, NA))
  expect_equal(result$production, c(NA,1.8, NA))
  expect_equal(result$PtoB, c(NA,1.6363636364,NA))

})

test_that('IGR treats missing site/cum in biomass as zero', {

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 2, 3),
    interval = 1,
    area = 10,
    group_id = 1
  )

  biomass <- data.frame(
    site = 1,
    cum = c(1, 1, 3, 3),
    length = c(5, 10, 5, 10),
    wt = c(2, 6, 4, 8)
  )

  result <- .igr(
    growth = growth,
    biomass = biomass,
    sample = sample,
    class.type = "length",
    class.size = 5
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$biomass, c(0.8, 0, 1.2))
  expect_equal(result$interval_biomass, c(.4, .6, NA))
  expect_equal(result$production, c(.7, 1, NA))
  expect_equal(result$PtoB, c(1.75,1.666666667,NA))

})


# NA data handling  ------------------------------------------------------------

test_that('IGR removes NA groupings from estimates',{

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 2),
    interval = 1,
    area = 10,
    group_id = 1
  )

  biomass <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    length = c(5, 10, 5, 10),
    wt = c(2, 6, 4, 8)
  )

  lapply(c("site","cum"), function(x) {

    sample_na <- rbind(sample,c(1,3,1,10,1))
    sample_na[[x]][3] <- NA

    expect_warning(
      result_s <- .igr(
        growth = growth,
        biomass = biomass,
        sample = sample_na,
        class.type = "length",
        class.size = 5
      ),
      'NA values present for "cum" and "site" in "sample". These groupings will be ignored'
    )

    expect_equal(result_s$biomass, c(0.8, 1.2))
    expect_equal(result_s$interval_biomass[1], 1)
    expect_equal(result_s$production[1], 1.7)
    expect_equal(result_s$PtoB[1], 1.7)
    expect_true(is.na(result_s$production[2]))


    biomass_na <- rbind(biomass,c(1,3,10,4))
    biomass_na[[x]][5] <- NA

    expect_warning(
      result_b <- .igr(
        growth = growth,
        biomass = biomass_na,
        sample = sample,
        class.type = "length",
        class.size = 5
      ),
      'NA values present for "cum", "site", and "length" in "biomass". These groupings will be ignored'
    )

    expect_equal(result_b$biomass, c(0.8, 1.2))
    expect_equal(result_b$interval_biomass[1], 1)
    expect_equal(result_b$production[1], 1.7)
    expect_equal(result_b$PtoB[1], 1.7)
    expect_true(is.na(result_b$production[2]))

  })

  biomass_na_l <- rbind(biomass,c(1,3,10,4))
  biomass_na_l$length[5] <- NA

  # expect multiple warnings
  expect_warning(
    expect_warning(
      result <- .igr(
        growth = growth,
        biomass = biomass_na_l,
        sample = sample,
        class.type = "length",
        class.size = 5
      ),
      'NA values present for "input", resulting size classes will be ignored'
    ),
    'NA values present for "cum", "site", and "length" in "biomass". These groupings will be ignored'
  )


  expect_equal(result$biomass, c(0.8, 1.2))
  expect_equal(result$interval_biomass[1], 1)
  expect_equal(result$production[1], 1.7)
  expect_equal(result$PtoB[1], 1.7)
  expect_true(is.na(result$production[2]))

})

test_that('IGR returns NA production for missing interval and group_id', {

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 2),
    interval = 1,
    area = 10,
    group_id = 1
  )

  biomass <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    length = c(5, 10, 5, 10),
    wt = c(2, 6, 4, 8)
  )

  na_warning <- list(
    interval = 'NA values present for some "intervals" in "sample". This will result in NA for production',
    group_id = 'NA values present for "group_id" in "sample". These values will not be assigned a growth rate and will result in NA production estimate.'
  )

  lapply(c("interval", "group_id"), function(x) {

    sample_na <- sample
    sample_na[[x]][1] <- NA

    expect_warning(
      result <- .igr(
        growth = growth,
        biomass = biomass,
        sample = sample_na,
        class.type = "length",
        class.size = 5
      ),
      na_warning[[x]]
    )

    expect_equal(result$biomass, c(0.8, 1.2))
    expect_equal(result$interval_biomass[1], 1)
    expect_true(all(is.na(result$production)))
    expect_true(all(is.na(result$PtoB)))

  })

})

test_that("IGR returns NA production and biomass if area is missing", {

  growth <- data.frame(
    group_id = 1,
    length = c(5, 10),
    interval = 1,
    interval_growth_pred = c(1, 2)
  )

  sample <- data.frame(
    site = 1,
    cum = c(1, 2),
    interval = 1,
    area = c(NA,10),
    group_id = 1
  )

  biomass <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    length = c(5, 10, 5, 10),
    wt = c(2, 6, 4, 8)
  )
  expect_warning(
    result <- .igr(
      growth = growth,
      biomass = biomass,
      sample = sample,
      class.type = "length",
      class.size = 5
    ),
    'NA values present for "area" in "sample". This will result in NAs for density, biomass, and production'
  )


  expect_equal(nrow(result), 2)

  expect_equal(result$biomass, c(NA,1.2))
  expect_true(all(is.na(result$interval_biomass)))
  expect_true(all(is.na(result$production)))
  expect_true(all(is.na(result$PtoB)))

})
