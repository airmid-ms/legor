#function 1

#idea
fit_pieces_model <- function(lego_data) {

  if (!all(c("year", "pieces") %in% names(lego_data))) {
    stop("The data must contain 'year' and 'pieces' columns.")
  }

  # Fit a linear model
  model <- lm(pieces ~ year, data = lego_data)

  return(summary(model))
}


lego_model_summary <- fit_pieces_model(lego_data)
(lego_model_summary)


#updated

#' Fit a linear model to predict the number of pieces based on year
#'
#' Fits a linear regression model to predict the number of pieces in LEGO sets based on the year.
#' @param lego_data A data frame or tibble containing LEGO set information, including "year" and "pieces" columns.
#' @param ... Catches unused arguments.
#'
#' @return An object of class \code{"lego_fit"} which includes the model details
#' as well as the data set used for fitting.
#'
#' @note This function is specifically designed for LEGO set data containing "year" and "pieces" columns.
#'
#' @export
#' @examples
#' mod <- fit_lego(lego_data)
#' print(mod)
fit_lego <- function(lego_data, ...) {
  # Ensure the input is a data frame or tibble
  if (!is.data.frame(lego_data)) stop("The input must be a data frame or tibble.")

  # Check for required columns
  if (!all(c("year", "pieces") %in% names(lego_data))) {
    stop("The data must contain 'year' and 'pieces' columns.")
  }

  # Fit the linear model
  mod <- stats::lm(pieces ~ year, data = lego_data)

  # Print model summary
  print(summary(mod))

  # Prepare the output object
  output <- list(
    model = mod,
    data = lego_data
  )

  # Assign class to the output
  class(output) <- c("lego_fit", "list")

  # Return invisibly
  invisible(output)
}





#Update to allow the user to input the columns to be plotted



#' fit_lego2
#'
#' @param lego_data A data frame or tibble containing LEGO set information, including columns like "year", "pieces" and "us_retailprice"
#' @param x_col A character string specifying the name of the x column
#' @param y_col A character string specifying the name of the y column
#' @param ... Additional arguments
#'
#' @returns An object of class "lego_fit" which includes the model details as well as the data set used for fitting.
#' @export
#'
#' @examples
#' fit_lego2(lego_data, "pieces", "us_retailprice")
#'
#' fit_lego2(lego_data, "year", "us_retailprice")

fit_lego2 <- function(lego_data, x_col, y_col, ...) {
  # Ensure the input is a data frame or tibble
  if (!is.data.frame(lego_data)){
    stop("The input must be a data frame or tibble.")
  }

  # Check for required columns
  if (!all(c(x_col, y_col) %in% names(lego_data))) {
    stop("The data must contain the specified columns.")
  }

  # Fit the linear model
  x <- as.formula(paste(y_col, "~", x_col))
  mod <- stats::lm(x, data = lego_data)

  # Print model summary
  print(summary(mod))

  # Prepare the output object
  output <- list(
    model = mod,
    data = lego_data,
    x_col = x_col,
    y_col = y_col
  )

  # Assign class to the output
  class(output) <- c("lego_fit", "list")

  # Return invisibly
  invisible(output)
}
fit_lego(lego_data, "pieces", "year")

#test

