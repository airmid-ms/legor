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





