lego_data <- as.data.frame(lego_data)

## Modify the fit function to handle data frames directly
fit <- function(obj,
                data_type = c("yearly"),
                fit_type = c("lm", "loess", "polynomial"),
                ploynomial_degree = 3) {
  ## Ensure the input is a data frame
  if (!is.data.frame(obj)) stop("The input object must be a data frame")

  ## To find which data set to use
  data_type <- match.arg(data_type)

  ## To find which fitting type method to use
  fit_type <- match.arg(fit_type)

  ## Filter and prepare data
  if (data_type == "yearly") {
    # Check the relevant columns exist
    if (!all(c("pieces", "us_retailprice") %in% colnames(obj))) {
      stop("The data must contain 'pieces' and 'us_retailprice' columns")
    }
    dat <- obj[!is.na(obj$pieces) & !is.na(obj$us_retailprice), ]
  } else {
    stop("Only 'yearly' data_type is supported currently.")
  }

  ## Fit specified model
  mod <- switch(fit_type,
                lm = {
                  lm(us_retailprice ~ pieces, data = dat)
                },
                loess = {
                  loess(us_retailprice ~ pieces, data = dat)
                },
                polynomial = {
                  lm(us_retailprice ~ poly(pieces, polynomial_degree), data = dat)
                })

  ## Print model summary
  print(summary(mod))

  ## Create output object
  output <- list(model = mod,
                 data = dat,
                 data_type = data_type,
                 fit_type = fit_type)

  ## Add class
  class(output) <- c("lego_fit", "listof")
  invisible(output)
}

## Example of use
model <- fit(lego_data, "yearly", 'lm')
