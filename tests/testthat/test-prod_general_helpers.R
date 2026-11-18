
# .check_input_data  ===========================================================

test_that("incorrect input arguments are rejected", {
  expect_error(
    .check_input_data(data.frame(1:3), "a"),
    'type must be "sample","biomass","growth", or "class_df'
  )
})


# sample  ----------------------------------------------------------------------
test_that("sample contains all columns", {

  input <- data.frame(
    site = 1,
    cum = 1,
    interval = 1,
    area = 10,
    group_id = 1
  )

  lapply(seq_along(input), function (x) {
    df <- input
    colnames(df)[x] <- "a"
    expect_error(
      .check_input_data(df,"sample"),
      '"sample" must contain columns "site" ,"cum", "interval","area", and "group_id"'
    )
  })
})

test_that("class_df does not contain duplicated data", {

  input <- data.frame(
    site = 1,
    cum = c(1,2,3,3),
    interval = 1,
    area = c(10,11,10,11),
    group_id = 1
  )

  expect_error(
    .check_input_data(
      data.frame(
        site = 1,
        cum = c(1,2,3,3),
        interval = 1,
        area = c(10,11,10,11),
        group_id = 1
      ),
      "sample"
    ),
    '"sample" contains duplicated entries'
  )

  expect_error(
    .check_input_data(
      data.frame(
        site = 1,
        cum = c(1,2,3,3),
        interval = c(10,11,10,11),
        area = 1,
        group_id = 1
      ),
      "sample"
    ),
    '"sample" contains duplicated entries'
  )


})

test_that("NA area and interval warnings sound", {

  expect_error(
    .check_input_data(
      data.frame(
        site = 1,
        cum = 1,
        interval = 1,
        area = NA,
        group_id = 1
      ),
      "sample"
    ),
    '"area" has all NA values in "sample"'
  )

  expect_error(
    .check_input_data(
      data.frame(
        site = 1,
        cum = 1,
        interval = NA,
        area = 1,
        group_id = 1
      ),
      "sample"
    ),
  '"interval" has all NA values in "sample"'
  )

  expect_warning(
    .check_input_data(
      data.frame(
        site = 1,
        cum = c(1,2),
        interval = 1,
        area = c(1,NA),
        group_id = 1
      ),
      "sample"
    ),
    'NA values present for "area" in "sample". This will result in NAs for density, biomass, and production'
  )

  expect_warning(
    .check_input_data(
      data.frame(
        site = 1,
        cum = c(1,2),
        interval = c(1,NA),
        area = 1,
        group_id = 1
      ),
      "sample"
    ),
    'NA values present for some "intervals" in "sample". This will result in NA for production'
  )

})

test_that("NA group_id warnings sound", {

  expect_warning(
    .check_input_data(
      data.frame(
        site = 1,
        cum = c(1,2),
        interval = 1,
        area = 1,
        group_id = c(1,NA)
      ),
      "sample"
    ),
    'NA values present for "group_id" in "sample". These values will not be assigned a growth rate and will result in NA production estimate.'
  )

})

test_that("NA site and cum warnings sound", {

  expect_warning(
    .check_input_data(
      data.frame(
        site = 1,
        cum = c(1,NA),
        interval = 1,
        area = 1,
        group_id = 1
      ),
      "sample"
    ),
    'NA values present for "cum" and "site" in "sample". These groupings will be ignored'
  )

  expect_warning(
    .check_input_data(
      data.frame(
        site = c(1, NA),
        cum = 1,
        interval = 1,
        area = 1,
        group_id = 1
      ),
      "sample"
    ),
    'NA values present for "cum" and "site" in "sample". These groupings will be ignored'
  )

})


# biomass  ---------------------------------------------------------------------
test_that("biomass contains all columns", {

  input <- data.frame(
    cum = 1,
    site = 1,
    length = 1,
    wt = 1
  )

  lapply(seq_along(input), function (x) {
    df <- input
    colnames(df)[x] <- "a"
    expect_error(
      .check_input_data(df,"biomass"),
      '"biomass" must contain columns "cum", "site", "length", and "wt"'
    )
  })

})

test_that("biomass has numeric length and wt columns", {

  expect_error(
    .check_input_data(
      data.frame(
        cum = 1,
        site = 1,
        length = NA,
        wt = 1:3
      ),
      "biomass"
    ),
    '"length" and/or "wt" have all NA values in "biomass"'
  )

  expect_error(
    .check_input_data(
      data.frame(
        cum = 1,
        site = 1,
        length = 1,
        wt = NA
      ),
      "biomass"
    ),
    '"length" and/or "wt" have all NA values in "biomass"'
  )

  expect_error(
    .check_input_data(
      data.frame(
        cum = 1,
        site = 1,
        length = c("1","2..2","a3"),
        wt = 1:3
      ),
      "biomass"
    ),
    '"length" and "wt" must be provided as numeric in "biomass"'
  )

  expect_error(
    .check_input_data(
      data.frame(
        cum = 1,
        site = 1,
        length = 1:3,
        wt = c("1","2..2","a3")
      ),
      "biomass"
    ),
    '"length" and "wt" must be provided as numeric in "biomass"'
  )
})

test_that("NA values in biomass trigger warnings", {
  expect_warning(
    .check_input_data(
      data.frame(
        cum = NA,
        site = 1,
        length = 1,
        wt = 1
      ),
      "biomass"
    ),
    'NA values present for "cum", "site", and "length" in "biomass". These groupings will be ignored'
  )

  expect_warning(
    .check_input_data(
      data.frame(
        cum = 1,
        site = NA,
        length = 1,
        wt = 1
      ),
      "biomass"
    ),
    'NA values present for "cum", "site", and "length" in "biomass". These groupings will be ignored'
  )

  expect_warning(
    .check_input_data(
      data.frame(
        cum = 1,
        site = 1,
        length = c(NA,1),
        wt = 1
      ),
      "biomass"
    ),
    'NA values present for "cum", "site", and "length" in "biomass". These groupings will be ignored'
  )

  expect_warning(
    .check_input_data(
      data.frame(
        cum = 1,
        site = 1,
        length = 1,
        wt = c(NA,1)
      ),
      "biomass"
    ),
    'NA values present for "wt" in "biomass", these lines will be treated as no individual present'
  )
})


# growth  ----------------------------------------------------------------------

test_that("growth contains all columns", {
  input <- data.frame(
    group_id = 1,
    length = 1,
    interval = 1,
    interval_growth_pred =1
  )

  lapply(seq_along(input), function (x) {
    df <- input
    colnames(df)[x] <- "a"
    expect_error(
      .check_input_data(df,"growth"),
      '"growth" must contain columns "group_id" ,"length", "interval", and "interval_growth_pred"'
    )
  })
})

test_that("Length and growth errors sound", {
  input <- data.frame(
    group_id = 1,
    length = 1,
    interval = 1,
    interval_growth_pred =2
  )

  errors <- c(
    '"length" and "interval_growth_pred" must be provided as numeric in "growth"',
    '"interval_growth_pred" must be postive'
  )

  lapply(c("length","interval_growth_pred"), function (x){
    df <- input
    df[[x]] <- "a"
    expect_error(
      .check_input_data(df,"growth"),
      errors[1]
    )
  })

  df2 <- input
  df2$interval_growth_pred <- -2
  expect_error(
    .check_input_data(df2,"growth"),
    errors[2]
  )

})

test_that("NA warnings sound", {

  input <- data.frame(
    group_id = 1,
    length = c(1,2),
    interval = 1,
    interval_growth_pred =c(2,4)
  )

  cols <- c("group_id","length","interval","interval_growth_pred")
  warns <- c(
    'NA values present for "group_id" in "growth", these growth rates will be ignored',
    'NA values present for "length" in "growth", these growth rates will be ignored',
    'NA values present for "interval" in "growth", these growth rates will be ignored',
    'NA values present for "interval_growth_pred" in "growth"'
  )

  lapply(seq_along(cols), function (x) {
    df <- input
    df[[cols[x]]][2] <- NA
    expect_warning(
      .check_input_data(df,"growth"),
      warns[x]
    )
  })

})


# class_df  --------------------------------------------------------------------
test_that("class_df contains all columns", {
  input <- data.frame(
    length = 1,
    class = 1
  )

  lapply(seq_along(input), function (x) {
    df <- input
    colnames(df)[x] <- "a"
    expect_error(
      .check_input_data(df,"class_df"),
      '"class_df" must contain columns "length" and "class"'
    )
  })
})


test_that("class_df does not contain any NA data", {

  expect_error(
    .check_input_data(
      data.frame(
        length = 1,
        class = NA),
      "class_df"
    ),
    "'class_df contains NA values for atleast one column"
  )

  expect_error(
    .check_input_data(
      data.frame(
        length = NA,
        class = 1),
      "class_df"
    ),
    "'class_df contains NA values for atleast one column"
  )

})


# .class_fun  ==================================================================

# Input validation  ------------------------------------------------------------
test_that("class.type must be age or length", {
  expect_error(
    .class_fun(3, "siz", 5),
    'class.type must be "age" or "length"'
  )
})


test_that("class.size must be postive integer", {
  expect_error(
    .class_fun(4, "length", .5),
    "class.size must be postive interger"
  )

  expect_error(
    .class_fun(4, "length", -.5),
    "class.size must be postive interger"
  )

  expect_error(
    .class_fun(4, "length", -5),
    "class.size must be postive interger"
  )

  expect_error(
    .class_fun(4, "length", 0),
    "class.size must be postive interger"
  )
})


# Class assignment  ------------------------------------------------------------
test_that("size classes are assigned correctly", {

  input <- c(2, 5, 9, 10, 15, 20)

  result <- .class_fun(input, "length", 5)

  expect_equal(
    result$class,
    c(0, 1, 1, 2, 3, 4)
  )
})

test_that("age classes are assigned correctly", {

  input <- c(0, 1, 2, 3)

  result <- .class_fun(input, "age", 2)

  expect_equal(
    result$class,
    c(0, 0, 1, 1)
  )
})

test_that("observations on class boundaries are assigned correctly", {

  input <- c(5, 10, 15)

  result <- .class_fun(input, "length", 5)

  expect_equal(result$class, c(1, 2, 3))
})

test_that("Classes defined by NA lengths are removed", {

  input1 <- c(5, 10, 15,NA)
  input2 <- c(2,NA,4,6)

  expect_warning(
    result1 <- .class_fun(input1, "length", 5),
    'NA values present for "input", resulting size classes will be ignored'
    )
  expect_equal(result1$class, c(1, 2, 3))
  expect_equal(result1$length, c(5, 10, 15))

  expect_warning(
    result2 <- .class_fun(input2, "age", 2),
    'NA values present for "input", resulting size classes will be ignored'
  )
  expect_equal(result2$class, c(1, 2, 3))
  expect_equal(result2$age, c(2, 4, 6))

  expect_error(
    .class_fun(c(NA, NA),"length",5),
    'No values present for "input"'
  )

  expect_error(
    .class_fun(c(NA, NA),"age",5),
    'No values present for "input"'
  )

})


test_that("duplicate class length combinations are removed", {

  input <- c(5, 5, 10, 10)

  result <- .class_fun(input, "length", 5)

  expect_equal(nrow(result), 2)

  expect_equal(
    result$length,
    c(5, 10)
  )
})

test_that("output contains length, and class", {

  input <- c(5, 10, 15)

  result <- .class_fun(input, "length", 5)

  expect_named(
    result,
    c("length", "class")
  )
})

test_that("output contains age, and class", {

  input <- c(5, 10, 15)

  result <- .class_fun(input, "age", 5)

  expect_named(
    result,
    c("age", "class")
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
    length = c(5, 10),
    class = c(1, 2)
  )

  biomass <- data.frame(
    site = 1,
    cum = 1,
    length = 5,
    wt = 2
  )

  expect_no_warning(result <- .class_bio_sum(sample, class_df, biomass))

  expect_equal(result$mean_mass[result$class == 1], 2)
  expect_equal(result$mean_mass[result$class == 2], 0)
  expect_equal(result$n[result$class == 1], 1)
  expect_equal(result$n[result$class == 2], 0)
})

test_that("missing sites from biomass produce zero mean mass", {

  sample <- data.frame(
    site = 1,
    cum = c(1,2),
    interval = 1,
    area = 10,
    group_id = 1
  )

  class_df <- data.frame(
    length = c(5, 10),
    class = c(1, 2)
  )

  biomass <- data.frame(
    site = 1,
    cum = 1,
    length = c(5,10),
    wt = c(2,3)
  )

  expect_no_warning(result <- .class_bio_sum(sample, class_df, biomass))

  expect_equal(result$mean_mass[result$class == 1 & result$cum == 1], 2)
  expect_equal(result$mean_mass[result$class == 1 & result$cum == 2], 0)
  expect_equal(result$mean_mass[result$class == 2 & result$cum == 1], 3)
  expect_equal(result$mean_mass[result$class == 2 & result$cum == 2], 0)

  expect_equal(result$n[result$class == 1 & result$cum == 1], 1)
  expect_equal(result$n[result$class == 1 & result$cum == 2], 0)
  expect_equal(result$n[result$class == 2 & result$cum == 1], 1)
  expect_equal(result$n[result$class == 2 & result$cum == 2], 0)

})

test_that("NA wt values do not affect mean mass", {

  sample <- data.frame(
    site = 1,
    cum = 1,
    interval = 1,
    area = 10,
    group_id = 1
  )

  class_df <- data.frame(
    length = c(5, 10),
    class = c(1, 2)
  )

  biomass <- data.frame(
    site = 1,
    cum = 1,
    length = c(5,10,10),
    wt = c(2,NA,3)
  )

  expect_warning(
    result <- .class_bio_sum(sample, class_df, biomass),
    'NA values present for "wt" in "biomass", these lines will be treated as no individual present'
  )

  expect_equal(result$mean_mass[result$class == 1], 2)
  expect_equal(result$mean_mass[result$class == 2], 3)
  expect_equal(result$n[result$class == 1], 1)
  expect_equal(result$n[result$class == 2], 1)
})


test_that("NA length, cum, and site values are removed before grouping", {

   sample <- data.frame(
    site = c(1,1),
    cum = c(1,2),
    interval = c(1,1),
    area = 10,
    group_id = 1
  )

  class_df <- data.frame(
    length = c(5, 10),
    class = c(1, 2)
  )

  biomass <- data.frame(
    site = c(1,1,1),
    cum = c(1,2,2),
    length = c(5,10,10),
    wt = c(2,10,16)
  )

  lapply(c("site","cum","length"), function(x) {

    biomass_na <- biomass
    biomass_na[[x]][3] <- NA
    expect_warning(
      result <- .class_bio_sum(sample, class_df, biomass_na),
      'NA values present for "cum", "site", and "length" in "biomass". These groupings will be ignored'
      )

    expect_equal(result$mean_mass, c(2,0,0,10))
    expect_equal(result$n, c(1,0,0,1))
    expect_equal(result$site, rep(1,4))
    expect_equal(result$cum, c(1,1,2,2))

  })

  lapply(c("site","cum"), function(x) {

    sample_na <- rbind(sample,c(1,3,1,10,1))
    sample_na[[x]][3] <- NA
    expect_warning(
      result <- .class_bio_sum(sample_na, class_df, biomass),
      'NA values present for "cum" and "site" in "sample". These groupings will be ignored'
    )

    expect_equal(result$mean_mass, c(2,0,0,13))
    expect_equal(result$n, c(1,0,0,2))
    expect_equal(result$site, c(1,1,1,1))
    expect_equal(result$cum, c(1,1,2,2))

  })

})

test_that("class biomass summary ignores site/cums in biomass but not sample",{

  sample <- data.frame(
    site = 1,
    cum = 1,
    interval = 1,
    area = 10,
    group_id = 1
  )

  class_df <- data.frame(
    length = c(5, 6, 7),
    class = c(1, 1, 1)
  )

  biomass <- data.frame(
    site = c(1,1,1,2),
    cum = 1,
    length = c(5, 6, 7,7),
    wt = c(2, 4, 6,10)
  )

  expect_warning(
    result <- .class_bio_sum(sample, class_df, biomass),
    'Some site and cum values from "biomass" are missing from "sample". This will prevent biomass data to be assigned an age class or growth rate, and will be removed'
    )

  expect_equal(result$mean_mass, 4)
  expect_equal(result$n, 3)

})

test_that("class biomass summary fails if class_id is missing lengths from biomass", {

  sample <- data.frame(
    site = 1,
    cum = 1,
    interval = 1,
    area = 10,
    group_id = 1
  )

  class_df <- data.frame(
    length = c(5, 6, 7),
    class = c(1, 1, 1)
  )

  biomass <- data.frame(
    site = 1,
    cum = 1,
    length = c(5, 6, 7,8),
    wt = c(2, 4, 6,10)
  )

  expect_error(
    result <- .class_bio_sum(sample, class_df, biomass),
    '"class_id" does not contain all lengths in "biomass'
  )

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




# .class_growth_sum  ==========================================================
test_that("class growth summary calculates mean growth correctly", {

  growth <- data.frame(
    group_id = 1,
    length = c(5, 6, 7, 10, 11),
    interval = 1,
    interval_growth_pred = c(1, 2, 3, 4, 5)
  )

  class_df <- data.frame(
    length = c(5, 6, 7, 10, 11),
    class = c(1, 1, 1, 2, 2)
  )

  result <- .class_growth_sum(growth, class_df)

  result <- dplyr::arrange(result, class)

  expect_equal(result$mean_growth, c(2, 4.5))
  expect_equal(result$class, c(1, 2))
})

test_that("class growth summary preserves groups and intervals", {

  growth <- data.frame(
    group_id = c(1, 1, 2, 2),
    length = c(5, 10, 5, 10),
    interval = c(1, 1, 1, 1),
    interval_growth_pred = c(1, 2, 3, 4)
  )

  class_df <- data.frame(
    length = c(5,10),
    class = c(1,2)
  )

  result <- .class_growth_sum(growth, class_df)

  expect_equal(nrow(result), 4)
  expect_equal(
    result$mean_growth[order(result$group_id, result$class)],
    c(1, 2, 3, 4)
  )
})


test_that("NA length are removed", {
  growth <- data.frame(
    group_id = 1,
    length = c(5,5,5),
    interval = c(1,1,2),
    interval_growth_pred = c(3,1,2)
  )

  class_df <- data.frame(
    length = 5,
    class = 1
  )

  warns <- c(
    'NA values present for "group_id" in "growth", these growth rates will be ignored',
    'NA values present for "length" in "growth", these growth rates will be ignored',
    'NA values present for "interval" in "growth", these growth rates will be ignored'
  )
  lapply(1:3, function(x) {
    growth_na <- growth
    growth_na[[x]][3] <- NA
    expect_warning(
      results <- .class_growth_sum(growth_na, class_df),
      warns[x]
    )
    expect_equal(results$group_id, 1)
    expect_equal(results$class, 1)
    expect_equal(results$interval, 1)
    expect_equal(results$mean_growth, 2)
  })
})

test_that("growth summary warns if lengths are missing from class_df",{
  growth <- data.frame(
    group_id = c(1,2,3),
    length = c(5,6,10),
    interval = 1,
    interval_growth_pred = 1
  )

  class_df <- data.frame(
    length = c(5,6,7),
    class = 1
  )

  expect_error(
    result <- .class_growth_sum(growth, class_df),
    'Some values for "length" from "growth" are missing from "class_df"'
  )

})


# .class_igr  ==================================================================

test_that("class IGR calculates biomass and production correctly", {

  input <- data.frame(
    site = 1,
    class = 1,
    cum = 1:3,
    n = c(10, 20, 30),
    mean_mass = c(2, 2, 2),
    area = c(10, 10, 10),
    mean_growth = c(1, 2, 3)
  )

  result <- .class_igr(input)

  expect_equal(result$biomass, c(2, 4, 6))
  expect_equal(result$lead_biomass, c(4, 6, NA_real_))
  expect_equal(result$interval_biomass, c(3, 5, NA_real_))
  expect_equal(result$production, c(3, 10, NA_real_))
})

test_that("class IGR does not calculate production across missing intervals", {

  input <- data.frame(
    site = 1,
    class = 1,
    cum = c(1, 2, 4),
    n = c(10, 20, 30),
    mean_mass = c(2, 2, 2),
    area = c(10, 10, 10),
    mean_growth = c(1, 2, 3)
  )

  result <- .class_igr(input)

  expect_equal(result$lead_biomass[1], 4)
  expect_true(is.na(result$lead_biomass[2]))
  expect_true(is.na(result$lead_biomass[3]))

  expect_equal(result$production[1], 3)
  expect_true(all(is.na(result$production[2:3])))
})

test_that("class IGR calculates production independently by site and class", {

  input <- data.frame(
    site = c(1, 1, 1, 1, 1, 1),
    class = c(1, 1, 1, 2, 2, 2),
    cum = rep(1:3, 2),
    n = c(10, 20, 30, 40, 50, 60),
    mean_mass = 2,
    area = 10,
    mean_growth = 2
  )

  result <- .class_igr(input)

  expect_equal(result$interval_biomass, c(3, 5, NA, 9, 11, NA))
  expect_equal(result$production, c(6, 10, NA, 18, 22, NA))
})


# .prod_sum  ===================================================================

test_that("production summary aggregates across classes", {

  input <- data.frame(
    site = c(1, 1, 1, 1),
    cum = c(1, 1, 2, 2),
    class = c(1, 2, 1, 2),
    biomass = c(10, 20, 15, 25),
    interval_biomass = c(15, 25, 20, 30),
    production = c(2, 4, 3, 6)
  )

  result <- .prod_sum(input)

  expect_equal(result$biomass, c(30, 40))
  expect_equal(result$interval_biomass, c(40, 50))
  expect_equal(result$production, c(6, 9))
  expect_equal(result$PtoB, c(0.15, 0.18))
})


test_that("production summary preserves site groupings", {

  input <- data.frame(
    site = c(1, 1, 2, 2),
    cum = c(1, 1, 1, 1),
    class = c(1,2,1,2),
    biomass = c(10, 20, 30, 40),
    interval_biomass = c(10, 20, 30, 40),
    production = c(1, 2, 3, 4)
  )

  result <- .prod_sum(input)

  expect_equal(nrow(result), 2)
  expect_equal(result$biomass, c(30, 70))
  expect_equal(result$production, c(3, 7))
})

test_that("production summary does not convert missing values to zero", {

  input <- data.frame(
    site = c(1, 1),
    cum = c(1, 1),
    class = c(1,2),
    biomass = c(10, 10),
    interval_biomass = c(NA, NA),
    production = c(NA, NA)
  )

  result <- .prod_sum(input)

  expect_equal(result$biomass, 20)
  expect_true(is.na(result$interval_biomass))
  expect_true(is.na(result$production))
  expect_true(is.na(result$PtoB))
})

# test_that("production summary does not convert missing values to zero", {
#
#   input <- data.frame(
#     site = c(1, 1),
#     cum = c(1, 1),
#     class = c(1,2),
#     biomass = c(10, NA),
#     interval_biomass = c(10, NA),
#     production = c(2, NA)
#   )
#
#   result <- .prod_sum(input)
#
#   expect_true(is.na(result$biomass))
#   expect_true(is.na(result$interval_biomass))
#   expect_true(is.na(result$production))
#   expect_true(is.na(result$PtoB))
# })
# WHAT TO DO IF NA IS PRESENT IN ONE CLASS BUT NOT OTHERS (MISSING LENGTH DATA)

# # OLD .size_class tests
# # .class_fun  ==================================================================
#
# # Input validation  ------------------------------------------------------------
# test_that("class.type must be age or size", {
#   expect_error(
#     .class_fun(input, "length", 5),
#     'class.type must be "age" or "size"'
#   )
# })
#
# test_that("input must contain required columns", {
#   # expect_error(
#   #   .class_fun(data.frame(length = 10), "size", 5),
#   #   'input must contain columns "group_id" and "length"'
#   # )
#
#   expect_error(
#     .class_fun(data.frame(group_id = 1), "size", 5),
#     'input must contain columns "length"'
#   )
#
#   expect_error(
#     .class_fun(data.frame(group_id = 1, length = 4), "age", 5),
#     'input must contain column "age_pred"'
#   )
#
# })
#
#
# test_that("class.type must be age or size", {
#   expect_error(
#     .class_fun(input, "length", 5),
#     'class.type must be "age" or "size"'
#   )
# })
#
# test_that("class.size must be postive integer", {
#   expect_error(
#     .class_fun(data.frame(group_id = 1, length = 4), "size", .5),
#     "class.size must be postive interger"
#   )
#
#   expect_error(
#     .class_fun(data.frame(group_id = 1, length = 4), "size", -.5),
#     "class.size must be postive interger"
#   )
#
#   expect_error(
#     .class_fun(data.frame(group_id = 1, length = 4), "size", -5),
#     "class.size must be postive interger"
#   )
#
#   expect_error(
#     .class_fun(data.frame(group_id = 1, length = 4), "size", 0),
#     "class.size must be postive interger"
#   )
# })
#
#
# # Class assignment  ------------------------------------------------------------
# test_that("size classes are assigned correctly", {
#
#   input <- data.frame(
#     # group_id = 1,
#     length = c(2, 5, 9, 10, 15, 20)
#   )
#
#   result <- .class_fun(input, "size", 5)
#
#   expect_equal(
#     result$class,
#     c(0, 1, 1, 2, 3, 4)
#   )
# })
#
# test_that("age classes are assigned correctly", {
#
#   input <- data.frame(
#     # group_id = 1,
#     length = c(10, 15, 20, 25),
#     age_pred = c(0, 1, 2, 3)
#   )
#
#   result <- .class_fun(input, "age", 2)
#
#   expect_equal(
#     result$class,
#     c(0, 0, 1, 1)
#   )
# })
#
# test_that("observations on class boundaries are assigned correctly", {
#
#   input <- data.frame(
#     # group_id = 1,
#     length = c(5, 10, 15)
#   )
#
#   result <- .class_fun(input, "size", 5)
#
#   expect_equal(result$class, c(1, 2, 3))
# })
#
# test_that("Classes defined by NA lengths are removed", {
#
#   input1 <- data.frame(
#     # group_id = 1,
#     length = c(5, 10, 15,NA)
#   )
#
#   input2 <- data.frame(
#     # group_id = 1,
#     length = c(5, 10, 15,30),
#     age_pred = c(2,NA,4,6)
#   )
#
#   expect_warning(
#     result1 <- .class_fun(input1, "size", 5),
#     'NA values present for "length", resulting size classes will be ignored'
#   )
#   expect_equal(result1$class, c(1, 2, 3))
#   expect_equal(result1$length, c(5, 10, 15))
#
#   expect_warning(
#     result2 <- .class_fun(input2, "age", 2),
#     'NA values present for "age_pred", resulting age classes will be ignored'
#   )
#   expect_equal(result2$class, c(1, 2, 3))
#   expect_equal(result2$length, c(5, 15, 30))
#
#   expect_error(
#     .class_fun(data.frame(
#       length = c(NA, NA),
#       group_id = 1
#     ),
#     "size",
#     5
#     ),
#     'No values present for "length"'
#   )
#
#   expect_error(
#     .class_fun(data.frame(
#       length = c(4, 5),
#       age_pred = c(NA,NA),
#       group_id = 1
#     ),
#     "age",
#     5
#     ),
#     'No values present for "age_pred"'
#   )
#
# })
#
# # REMOVE - NO LONGER RELEVANT WITHOUT GORUP_ID
# # Groups have the same age classes  --------------------------------------------
# # test_that("identical classes across groups are accepted", {
# #
# #   input <- data.frame(
# #     group_id = rep(1:2, each = 3),
# #     length = rep(c(5, 10, 15), 2)
# #   )
# #
# #   expect_no_error(
# #     .class_fun(input, "size", 5)
# #   )
# # })
# #
# # test_that("groups with different lengths but identical classes are accepted", {
# #
# #   input <- data.frame(
# #     group_id = c(1, 1, 1, 2, 2, 2),
# #     length = c(5, 10, 15, 6, 11, 16)
# #   )
# #
# #   result <- .class_fun(input, "size", 5)
# #
# #   expect_no_error(
# #     .class_fun(input, "size", 5)
# #   )
# #
# #   expect_equal(
# #     sort(unique(result$class)),
# #     1:3
# #   )
# # })
# #
# # test_that("different classes across groups produce an error", {
# #
# #   input <- data.frame(
# #     group_id = c(1, 1, 1, 2, 2),
# #     length = c(5, 10, 15, 5, 10)
# #   )
# #
# #   expect_error(
# #     .class_fun(input, "size", 5),
# #     "Growth groups do not have the same size/age classes"
# #   )
# # })
