
# .group_impute  ---------------------------------------------------------------

# Test warnings and errors
test_that("does inccorect aruments throw errors?",{
  df <- .impute_data_creation(3)
  expect_error(group_impute(df,length,c(loc,sp,time),20))
  expect_error(group_impute(df,wt,c(site,sp,time),20))
  expect_error(group_impute(df,wt,c(loc,species,time),20))
  expect_error(group_impute(df,wt,c(loc,sp,cum),20))
  expect_error(group_impute(df,wt,c(loc,sp,time),"high"))
  expect_error(group_impute(df,wt,c(loc,sp,time),0))
})

# No na warning
test_that(paste0(
  "does data with no missing values in specified column throw warning and ",
  "returns identical data.frame?"
  ),{
    df <- .impute_data_creation(3)
    expect_warning(out <- group_impute(df,wt_clean,c(loc,sp,time),20))
    expect_identical(out,df)
  })


# Output structure
test_that("output data.frame contains the same structure as input",{
  df <- .impute_data_creation(3)
  df_out <- group_impute(df,wt,c(loc,time,sp),20)

  rows_in <- nrow(df)
  cols_in <- ncol(df)
  rows_out <- nrow(df_out)
  cols_out <- ncol(df_out)
  names_in <- colnames(df)
  names_out <- colnames(df_out)
  dif_names <- setdiff(names_out,names_in)

  expect_all_equal(rows_in,rows_out)
  expect_true(cols_in+1 == cols_out)
  expect_true(length(dif_names) == 1)
  expect_true(dif_names == "impute")
  })

# Are non imputed values the same
test_that("Non impute values are the same",{
  df <- .impute_data_creation(3)
  df_out <- group_impute(df,wt,c(loc,time,sp),20)

  expect_identical(
    df[,-which(names(df) == "wt")],
    df_out[,-which(names(df_out) %in% c("wt","impute"))]
    )
  expect_identical(
    df$wt[!is.na(df$wt)],
    df_out$wt[!is.na(df$wt)]
  )
})

# Test limits
test_that("values are only imputed when groups are within limits",{

  df <- .impute_data_creation(2)
  n_groups <- df %>%
    dplyr::group_by(loc,time) %>%
    dplyr::summarize(n = sum(!is.na(wt)))

  # WHen all groups have sufficnet sample size
  out_low <- group_impute(df,wt,c(loc,time),10,T)
  expect_all_true(!is.na(out_low$wt[is.na(df$wt)]))


  # WHen some groups have insufficient sample size,
  lapply(c(23,25), function(limit) {
    out <- group_impute(df,wt,c(loc,time),limit,T) %>%
      dplyr::left_join(n_groups)
    expect_all_true(out$n[is.na(df$wt) & !is.na(out$impute)] > limit)
    expect_all_true(is.na(out$wt[is.na(df$wt) & out$n <= limit]))
    expect_all_true(is.na(out$impute[is.na(df$wt) & out$n <= limit]))
    expect_all_true(!is.na(out$impute[is.na(df$wt) & out$n > limit]))
  })

  # When no groups have sufficnet sample size
  out_high <- group_impute(df,wt,c(loc,time),50,T) %>%
    dplyr::left_join(n_groups)
  expect_all_true(is.na(out_high$impute))
  expect_all_true(is.na((out_high$wt[is.na(df$wt)])))
})

# Test group sampling
test_that("values are imputed from within groups",{
  df1 <- .impute_data_creation(1)

  range1 <- df1 %>%
    dplyr::group_by(loc) %>%
    dplyr::summarise(
      min = min(wt,na.rm=T),
      max = max(wt,na.rm=T)
    )
  cond1 <- group_impute(df1,wt,loc,10,T) %>%
    dplyr::left_join(range1) %>%
    dplyr::filter(!is.na(impute)) %>%
    dplyr::mutate(good_impute = dplyr::case_when(
      wt >= min & wt <= max ~ T,
      T~F
    )) %>%
    dplyr::pull(good_impute)
  expect_all_true(cond1)

  df2 <- .impute_data_creation(2)

  range2 <- df2 %>%
    dplyr::group_by(time,loc) %>%
    dplyr::summarise(
      min = min(wt,na.rm=T),
      max = max(wt,na.rm=T)
    )
  cond2 <- group_impute(df2,wt,c(loc,time),10,T) %>%
    dplyr::left_join(range2) %>%
    dplyr::filter(!is.na(impute)) %>%
    dplyr::mutate(good_impute = dplyr::case_when(
      wt >= min & wt <= max ~ T,
      T~F
    )) %>%
    dplyr::pull(good_impute)
  expect_all_true(cond2)

  # THree groupings
  df3 <- .impute_data_creation(3)

  range3 <- df3 %>%
    dplyr::group_by(time,loc,sp) %>%
    dplyr::summarise(
      min = min(wt,na.rm=T),
      max = max(wt,na.rm=T)
    )
  cond3 <- group_impute(df3,wt,c(loc,time,sp),10,T) %>%
    dplyr::left_join(range3) %>%
    dplyr::filter(!is.na(impute)) %>%
    dplyr::mutate(good_impute = dplyr::case_when(
      wt >= min & wt <= max ~ T,
      T~F
    )) %>%
    dplyr::pull(good_impute)
  expect_all_true(cond3)

})


# test impute tag
test_that("Impute tag is only assigned to imputed data",{
  df <- .impute_data_creation(2)
  n_groups <- df %>%
    dplyr::group_by(loc,time) %>%
    dplyr::summarize(n = sum(!is.na(wt)))

  # WHen all groups have sufficnet sample size
  out <- group_impute(df,wt,c(loc,time),10,T)
  expect_all_true(!is.na(out$impute[is.na(df$wt)]))
  expect_all_true(is.na(out$impute[!is.na(df$wt)]))
})
# expect_all_true(is.na(df_out$impute[!is.na(df$wt)]))


# .impute_sampler  -------------------------------------------------------------

# sample limit check
test_that("Samples are not taken if limit is not met",{
  limit <- 20
  input_good_clean <- c(runif(30,1,10))
  input_bad_clean <- c(runif(15,1,10))

  input_good <- input_good_clean
  input_bad <- input_bad_clean
  input_good[sample(1:length(input_good),5,T)] <- NA
  input_bad[sample(1:length(input_bad),5,T)] <- NA

  out_good <- .impute_sampler(input_good,limit)
  out_bad <- .impute_sampler(input_bad,limit)

  expect_true(all(!is.na(out_good)))
  expect_false(all(!is.na(out_bad)))

})

# are outputs identical
test_that("No data is imputed if no NAs",{
  limit <- 20
  input_good_clean <- c(runif(30,1,10))
  input_bad_clean <- c(runif(15,1,10))

  # add NAs
  input_good <- input_good_clean
  input_bad <- input_bad_clean
  input_good[sample(1:length(input_good),5,T)] <- NA
  input_bad[sample(1:length(input_bad),5,T)] <- NA

  # Impute
  out_good <- .impute_sampler(input_good,limit)
  out_bad <- .impute_sampler(input_bad,limit)
  out_good_clean <- .impute_sampler(input_good_clean,limit)
  out_bad_clean <- .impute_sampler(input_bad_clean,limit)

  # Tests
  expect_all_equal(length(input_good),length(out_good))
  expect_all_equal(length(input_bad),length(out_bad))
  expect_identical(input_bad,out_bad)
  expect_identical(input_good_clean, out_good_clean)
  expect_identical(input_bad_clean,out_bad_clean)

})

# Test sampling
test_that("Imputed data comes from all data and orginal data is retained",{
  limit <- 20
  input_good_clean <- c(runif(30,1,10))
  input_good <- input_good_clean
  input_good[sample(1:length(input_good),5,T)] <- NA
  out_good <- .impute_sampler(input_good,limit)

  new_value <- out_good[is.na(input_good)]
  expect_all_true(new_value %in% input_good)
  expect_identical(
    input_good[!is.na(input_good)],
    out_good[!is.na(input_good)]
    )
})
