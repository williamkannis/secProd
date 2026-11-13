# .igr  ========================================================================

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
    class.type = "size",
    class.size = 5
  )

  expect_equal(nrow(result), 2)

  expect_equal(result$biomass, c(0.8, 1.2))
  expect_equal(result$interval_biomass[1], 1)
  expect_equal(result$production[1], 1.7)
  expect_equal(result$PtoB[1], 1.7)

  expect_true(is.na(result$production[2]))
})
# main functions tests

# NA
# NA length, cum, site - is removed
# NA area - NA density, biomass, prodciotn
# NA group_id, interval - leads to NA production, but values of biomass

# WHat happens if growth has NA for interval
#
#
