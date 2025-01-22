test_that("Lego_fit.R works with 'lm'", {
  load("~/legor/data/lego_data.rda")
  model <- fit(lego_data, fit_type = "lm")
  expect_s3_class(model, "lego_fit")
  expect_true("lm" %in% class(model$model))
})

test_that("Lego_fit.R works with 'loess'", {
  load("~/legor/data/lego_data.rda")
  model <- fit(lego_data, fit_type = "loess")
  expect_s3_class(model, "lego_fit")
  expect_true("loess" %in% class(model$model))
})

test_that("Lego_fit.R works with 'polynomial'", {
  load("~/legor/data/lego_data.rda")
  model <- fit(lego_data, fit_type = "polynomial", polynomial_degree = 3)
  expect_s3_class(model, "lego_fit")
  expect_true("lm" %in% class(model$model))
})

test_that("Lego_fit.R filters data by year_range", {
  load("~/legor/data/lego_data.rda")
  model <- fit(lego_data, fit_type = "lm", year_range = c(2015, 2020))
  test_data <- subset(lego_data, year >= 2015 & year <= 2020)
  expect_equal(nrow(model$data), nrow(test_data))
})

test_that("Lego_fit.R handles missing columns", {
  missing_dat <- lego_data[, !colnames(lego_data) %in% "pieces", drop = FALSE]
  expect_error(
    fit(missing_dat, fit_type = "lm"),
    "The data must contain 'pieces' and 'us_retailprice' columns"
  )
})

test_that("Lego_fit.R validates year_range error", {
  load("~/legor/data/lego_data.rda")
  expect_error(
    fit(lego_data, fit_type = "lm", year_range = c(2014,2010)),
    "'Year range' must be in ascending order, try swapping your chosen years"
  )
})
