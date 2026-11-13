test_that("sample_interval calculates time between consecutive sampling periods", {

  df <- data.frame(
    site = c(1, 1, 1),
    cum = c(1, 2, 3),
    date = as.Date(c("2020-01-01", "2020-01-11", "2020-01-31"))
  )

  out <- sample_interval(df)

  expect_equal(out$interval, c(10, 20, NA_real_))
})


test_that("sample_interval calculates intervals independently by site", {

  df <- data.frame(
    site = c(1, 1, 2, 2),
    cum = c(1, 2, 1, 2),
    date = as.Date(c(
      "2020-01-01",
      "2020-01-11",
      "2020-02-01",
      "2020-02-21"
    ))
  )

  out <- sample_interval(df)

  expect_equal(out$interval, c(10, NA_real_, 20, NA_real_))
})


test_that("sample_interval does not calculate intervals across missing sampling periods", {

  df <- data.frame(
    site = 1,
    cum = c(1, 2, 4),
    date = as.Date(c(
      "2020-01-01",
      "2020-01-11",
      "2020-01-31"
    ))
  )

  out <- sample_interval(df)

  expect_equal(out$interval, c(10, NA_real_, NA_real_))
})


test_that("sample_interval rounds intervals to whole days", {

  df <- data.frame(
    site = c(1, 1),
    cum = c(1, 2),
    date = as.POSIXct(c(
      "2020-01-01 00:00:00",
      "2020-01-02 12:00:00"
    ), tz = "UTC")
  )

  out <- sample_interval(df)

  expect_equal(out$interval, c(2, NA_real_))
})


test_that("sample_interval preserves the original rows and columns", {

  df <- data.frame(
    site = c(1, 1, 1),
    cum = c(1, 2, 3),
    date = as.Date(c("2020-01-01", "2020-01-11", "2020-01-21")),
    extra = c("a", "b", "c")
  )

  out <- sample_interval(df)

  expect_equal(nrow(out), nrow(df))
  expect_true(all(names(df) %in% names(out)))
  expect_equal(out$site, df$site)
  expect_equal(out$cum, df$cum)
  expect_equal(out$date, df$date)
  expect_equal(out$extra, df$extra)
})
