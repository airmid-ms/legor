#' Fit LEGO Data Models
#'
#' This package provides functions to fit various models to LEGO datasets, including linear regression (`lm`), LOESS, and polynomial regression.
#'
#' ## Define the Generic Function
#'
#' The `fit()` function is a generic function for fitting models to LEGO data. It calls the appropriate method based on the class of the object passed to it.
#'
#' @param obj A data frame containing LEGO data. It must include columns such as `pieces` and `us_retailprice`.
#' @param ... Additional arguments passed to specific methods (e.g., `fit_type`, `polynomial_degree`).
#'
#' @return An object of class `"lego_fit"` which contains the fitted model, input data, and the fit type used.
#'
#' @export
fit <- function(obj, ...) {
  UseMethod("fit")
}

#' Fit LEGO Model Using Different Methods
#'
#' Fits a specified model to LEGO data using linear regression (`lm`), LOESS, or polynomial regression.
#'
#' @param obj A data frame containing LEGO data. It must include columns `pieces` and `us_retailprice`.
#' @param fit_type A character string specifying the type of fit to apply. Choices are: `"lm"`, `"loess"`, or `"polynomial"`. Default is `"lm"`.
#' @param polynomial_degree A numeric value specifying the degree of the polynomial for polynomial regression. Default is 3. Must be greater than or equal to 1 for polynomial regression.
#' @param ... Additional arguments (currently not being used) saved for future methods or extensions.
#'
#' @details
#' The function allows users to fit linear models (`lm`), local polynomial regression (`loess`), or polynomial regression to LEGO data. The input data frame must contain the columns `pieces` and `us_retailprice`.
#' If `"polynomial"` is chosen, the user can specify the degree of the polynomial using the `polynomial_degree` argument.
#'
#' @return An object of class `"lego_fit"`. This object contains:
#' \item{model}{The fitted model object (`lm`, `loess`, or `lm` for polynomial).}
#' \item{data}{The data frame passed to the function.}
#' \item{fit_type}{The type of model used (either `"lm"`, `"loess"`, or `"polynomial"`).}
#'
#' @examples
#' \dontrun{
#' # Fit a linear model to the LEGO data
#' model <- fit(lego_data, 'lm')
#'
#' # Fit a LOESS model to the LEGO data
#' model2 <- fit(lego_data, 'loess')
#'
#' # Fit a polynomial model to the LEGO data with degree 2
#' model3 <- fit.legor(lego_data, 'polynomial', polynomial_degree = 2)
#' }
#'
#' @export
fit.legor <- function(obj,
                fit_type = c("lm", "loess", "polynomial"),
                polynomial_degree = 3, ...) {

  ## To find which fitting type method to use
  fit_type <- match.arg(fit_type)

  ## Verify that the polynomial degree is larger than or equal to 1 if the fit is picked.
  if (fit_type == "polynomial" && (!is.numeric(polynomial_degree) || polynomial_degree < 1)) {
    stop("'polynomial_degree' must be a numeric value larger than or equal to 1")
  }

  ## Check the relevant columns exist
  if (!all(c("pieces", "us_retailprice") %in% colnames(obj))) {
    stop("The data must contain 'pieces' and 'us_retailprice' columns")
  }

  ## Fit specified model
  mod <- switch(fit_type,
                lm = {
                  lm(us_retailprice ~ pieces, data = obj)
                },
                loess = {
                  loess(us_retailprice ~ pieces, data = obj)
                },
                polynomial = {
                  lm(us_retailprice ~ poly(pieces, polynomial_degree), data = obj)
                })

  ## Print model summary
  print(summary(mod))

  ## Create output object
  output <- list(model = mod,
                 data = obj,
                 fit_type = fit_type)
  class(output) <- c("lego_fit", "listof")
  invisible(output)
}
