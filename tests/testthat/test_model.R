test_that("Test model fitting with 'lm'", {
  load("~/legor/data/lego_data.rda")
  model_lm <- fit(lego_data, fit_type = "lm")
  expect_s3_class(model_lm, "lego_fit")
  lm_model <- lm(us_retailprice ~ pieces, data = lego_data)
  expect_equal(coef(model_lm$model), coef(lm_model))
})

test_that("Test model fitting with 'loess'", {
  load("~/legor/data/lego_data.rda")
  model_loess <- fit(lego_data, fit_type = "loess")
  expect_s3_class(model_loess, "lego_fit")
})

test_that("Test model fitting with polynomial regression", {
  load("~/legor/data/lego_data.rda")
  model_poly <- fit(lego_data, fit_type = "polynomial", polynomial_degree = 2)
  expect_s3_class(model_poly, "lego_fit")
})
