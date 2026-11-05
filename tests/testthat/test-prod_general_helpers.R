
# .class_fun  ==================================================================

# Input validation  ------------------------------------------------------------
test_that("class.type must be age or size", {
  expect_error(
    .class_fun(input, "length", 5),
    'class.type must be "age" or "size"'
  )
})

test_that("input must contain required columns", {
  expect_error(
    .class_fun(data.frame(length = 10), "size", 5),
    'input must contain columns "group_id" and "length"'
  )

  expect_error(
    .class_fun(data.frame(group_id = 1), "size", 5),
    'input must contain columns "group_id" and "length"'
  )

  expect_error(
    .class_fun(data.frame(group_id = 1, length = 4), "age", 5),
    'input must contain column "age_pred"'
  )

})

test_that("class.type must be age or size", {
  expect_error(
    .class_fun(input, "length", 5),
    'class.type must be "age" or "size"'
  )
})

test_that("class.size must be postive integer", {
  expect_error(
    .class_fun(data.frame(group_id = 1, length = 4), "size", .5),
    "class.size must be postive interger"
  )

  expect_error(
    .class_fun(data.frame(group_id = 1, length = 4), "size", -.5),
    "class.size must be postive interger"
  )

  expect_error(
    .class_fun(data.frame(group_id = 1, length = 4), "size", -5),
    "class.size must be postive interger"
  )

  expect_error(
    .class_fun(data.frame(group_id = 1, length = 4), "size", 0),
    "class.size must be postive interger"
  )
})


# Class assignment  ------------------------------------------------------------
test_that("size classes are assigned correctly", {

  input <- data.frame(
    group_id = 1,
    length = c(2, 5, 9, 10, 15, 20)
  )

  result <- .class_fun(input, "size", 5)

  expect_equal(
    result$class,
    c(0, 1, 1, 2, 3, 4)
  )
})

test_that("age classes are assigned correctly", {

  input <- data.frame(
    group_id = 1,
    length = c(10, 15, 20, 25),
    age_pred = c(0, 1, 2, 3)
  )

  result <- .class_fun(input, "age", 2)

  expect_equal(
    result$class,
    c(0, 0, 1, 1)
  )
})

test_that("observations on class boundaries are assigned correctly", {

  input <- data.frame(
    group_id = 1,
    length = c(5, 10, 15)
  )

  result <- .class_fun(input, "size", 5)

  expect_equal(result$class, c(1, 2, 3))
})

# Groups have the same age classes  --------------------------------------------
test_that("identical classes across groups are accepted", {

  input <- data.frame(
    group_id = rep(1:2, each = 3),
    length = rep(c(5, 10, 15), 2)
  )

  expect_no_error(
    .class_fun(input, "size", 5)
  )
})

test_that("groups with different lengths but identical classes are accepted", {

  input <- data.frame(
    group_id = c(1, 1, 1, 2, 2, 2),
    length = c(5, 10, 15, 6, 11, 16)
  )

  result <- .class_fun(input, "size", 5)

  expect_no_error(
    .class_fun(input, "size", 5)
  )

  expect_equal(
    sort(unique(result$class)),
    1:3
  )
})

test_that("different classes across groups produce an error", {

  input <- data.frame(
    group_id = c(1, 1, 1, 2, 2),
    length = c(5, 10, 15, 5, 10)
  )

  expect_error(
    .class_fun(input, "size", 5),
    "Growth groups do not have the same size/age classes"
  )
})


# Duplicate observations  ------------------------------------------------------
test_that("duplicate group length combinations are removed", {

  input <- data.frame(
    group_id = c(1, 1, 1, 1),
    length = c(5, 5, 10, 10)
  )

  result <- .class_fun(input, "size", 5)

  expect_equal(nrow(result), 2)

  expect_equal(
    result$length,
    c(5, 10)
  )
})

test_that("output contains group_id, length, and class", {

  input <- data.frame(
    group_id = 1,
    length = c(5, 10, 15)
  )

  result <- .class_fun(input, "size", 5)

  expect_named(
    result,
    c("group_id", "length", "class")
  )
})


# .class_bio_sum  ==============================================================

test_that("class biomass summary calculates correct values", {

  sample <- data.frame(
    site = 1,
    cum = 1,
    interval = 1,
    area = 10,
    group_id = 1
  )

  class_df <- data.frame(
    group_id = 1,
    length = c(5, 10, 15),
    class = c(1, 2, 3)
  )

  biomass <- data.frame(
    site = 1,
    cum = 1,
    length = c(5, 10, 15),
    wt = c(2, 4, 6)
  )

  result <- .class_bio_sum(sample, class_df, biomass)

  expect_equal(result$mean_mass, c(2, 4, 6))
  expect_equal(result$n, c(1, 1, 1))
  expect_equal(result$area, c(10, 10, 10))
  expect_equal(result$interval, c(1, 1, 1))
})

test_that("missing biomass observations produce zero mean mass", {

  sample <- data.frame(
    site = 1,
    cum = 1,
    interval = 1,
    area = 10,
    group_id = 1
  )

  class_df <- data.frame(
    group_id = 1,
    length = c(5, 10),
    class = c(1, 2)
  )

  biomass <- data.frame(
    site = 1,
    cum = 1,
    length = 5,
    wt = 2
  )

  result <- .class_bio_sum(sample, class_df, biomass)

  expect_equal(result$mean_mass[result$class == 1], 2)
  expect_equal(result$mean_mass[result$class == 2], 0)
  expect_equal(result$n[result$class == 2], 0)
})

test_that("class biomass summary calculates mean mass across individuals", {

  sample <- data.frame(
    site = 1,
    cum = 1,
    interval = 1,
    area = 10,
    group_id = 1
  )

  class_df <- data.frame(
    group_id = 1,
    length = c(5, 6, 7),
    class = c(1, 1, 1)
  )

  biomass <- data.frame(
    site = 1,
    cum = 1,
    length = c(5, 6, 7),
    wt = c(2, 4, 6)
  )

  result <- .class_bio_sum(sample, class_df, biomass)

  expect_equal(result$mean_mass, 4)
  expect_equal(result$n, 3)
})


# ..class_growth_sum  ==========================================================


