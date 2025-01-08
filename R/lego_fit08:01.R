fit <- function(obj, ...) {
  UseMethod("fit")
}


fit.legor <- function(obj,
                      fit_type = c("lm", "loess", "polynomial"),
                      polynomial_degree = 3,
                      year_range = NULL,...) {

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

  ## If year_range is provided, filter data
  if (!is.null(year_range)) {
    available_years <- unique(obj$year)
    if (!all(year_range[1]:year_range[2] %in% available_years)) {
      stop("The years in 'year_range' do not appear in the dataset try another year range")
    }
    if (length(year_range) != 2 || !is.numeric(year_range)) {
      stop("'year_range' must be a numeric vector of length 2")
    }
    if (year_range[1] > year_range[2]) {
      stop("'year_range' must be in ascending order (smaller number first) try swapping the order of year range")
    }
    obj <- subset(lego_data, year >= year_range[1] & year <= year_range[2])
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
                 fit_type = fit_type
  )
  class(output) <- c("lego_fit", "listof")
  invisible(output)
}
fit(lego_data, "lm")
fit(lego_data, "lm", year_range = c(2018, 2020))

