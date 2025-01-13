#' Plot Lego Fit Model
#'
#' Makes a plot for the fitted Lego model such as a scatter plot of the data, with a line representing
#' the fitted model, and labels the top three most expensive lego sets.
#'
#' @param x An object of class \code{"lego_fit"}, outputted from the \code{\link{fit}} function.
#' @param pieces_grid A numeric vector specifying the grid of piece counts over which to calculate
#' predicted values for the fitted model. Defaults to \code{pretty(x$data$pieces, n = 200)}. This
#' is used to create the smooth fitted line.
#' @param ... Catches the unused arguments to \code{ggplot}.
#'
#' @details This function creates a plot to visualize the relationship between number of lego pieces
#' and the US retail price, overlaid with fitted model. The top three most expensive lego sets are
#' also highlighted in the plot by labels of their names.
#'
#' The fitted line is created by the model specified in \code{x$fit_type}. Supported models
#' include linear regression (\code{"lm"}), LOESS (\code{"loess"}), and polynomial regression (\code{"polynomial"}).
#'
#' @return A \code{ggplot} object showing the scatter plot of the data, the fitted model,
#' and labels for the top three most expensive lego sets.
#'
#' @import ggplot2
#' @importFrom dplyr arrange desc
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom stats na.omit predict
#' @importFrom utils head
#' @export
#'
#' @examples
#' model <- fit(lego_data, fit_type = "lm")
#' plot(model)
#'
#' model2 <- fit(lego_data, fit_type = "loess")
#' plot(model2)
#'
#' model3 <- fit(lego_data, fit_type = "polynomial", polynomial_degree = 3)
#' plot(model3)
plot.lego_fit <- function(x, pieces_grid = pretty(x$data$pieces, n = 200), ...) {

  ## Check that the input is a lego_fit object
  if (!inherits(x, "lego_fit")) {
    stop("The input must be a 'lego_fit' object.")
  }

  ## Extract the data
  df <- x$data

  ## Generate predicted values based on the pieces grid and the fit_type
  fits <- switch(x$fit_type,
                 lm = {
                   tibble(pieces = pieces_grid, pred = predict(x$model,
                                                               newdata = tibble(pieces = pieces_grid)))
                 },
                 loess = {
                   tibble(pieces = pieces_grid, pred = predict(x$model,
                                                               newdata = tibble(pieces = pieces_grid))) %>%
                     na.omit()
                 },
                 polynomial = {
                   tibble(pieces = pieces_grid, pred = predict(x$model,
                                                               newdata = tibble(pieces = pieces_grid)))
                 })
  ## Find the top 3 highest retail price lego sets.
  top3_df <- df %>%
    arrange(desc(us_retailprice)) %>%
    head(3)

  ## Create the plot with labels for the highest 3 points based on 'us_retailprice'
  ggplot(df, aes(x = pieces, y = us_retailprice)) +
    geom_point(aes(colour = us_retailprice)) +
    theme_linedraw() +
    xlab("Number of Pieces") +
    ylab("US Retail Price") +
    ggtitle(paste(x$fit_type, "fit based on lego_data (", min(df$year), "-", max(df$year), ")")) +
    geom_line(data = fits, aes(x = pieces, y = pred, colour = pred)) +
    geom_text(data = top3_df,
              aes(label = name),
              vjust = -0.5) +
    scale_color_viridis_c(option = "plasma")
}

