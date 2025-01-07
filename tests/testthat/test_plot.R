test_that("Plot function works for 'lm' model", {
  load("~/legor/data/lego_data.rda")
  model_lm <- fit(lego_data, fit_type = "lm")
  plot_lm <- plot(model_lm)
  expect_s3_class(plot_lm, "gg")
})

test_that("Plot produces a ggplot object", {
  load("~/legor/data/lego_data.rda")
  model_lm <- fit(lego_data, fit_type = "lm")
  plot_lm <- plot(model_lm)
  expect_s3_class(plot_lm, "gg")
})

test_that("Plot function does not throw errors", {
  load("~/legor/data/lego_data.rda")
  model_lm <- fit(lego_data, fit_type = "lm")
  expect_error(plot(model_lm), NA)
})

test_that("Plot function works for 'loess' model", {
  load("~/legor/data/lego_data.rda")
  model_loess <- fit(lego_data, fit_type = "loess")
  plot_loess <- plot(model_loess)
  expect_s3_class(plot_loess, "gg")
})

test_that("Plot function works for 'polynomial' model", {
  load("~/legor/data/lego_data.rda")
  model_poly <- fit(lego_data, fit_type = "polynomial", polynomial_degree = 2)
  plot_poly <- plot(model_poly)
  expect_s3_class(plot_poly, "gg")
})
