fit <- function(obj,
                fit_type = c("lm", "loess", "polynomial"),
                polynomial_degree = 3) {

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

## Example of use
model <- fit(lego_data, 'lm')
