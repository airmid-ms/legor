\name{hello}
\alias{hello}
\title{Hello, World!}
\usage{
hello()
}
\description{
Prints 'Hello, world!'.
}
\examples{
hello()
}

#Fit a linear model to predict the number of pieces based on year

fit_pieces_model <- function(data) {

  if (!all(c("year", "pieces") %in% names(data))) {
    stop("The data must contain 'year' and 'pieces' columns.")
  }

  # Fit a linear model
  model <- lm(pieces ~ year, data = data)

  return(summary(model))
}


lego_model_summary <- fit_pieces_model(lego_data)
(lego_model_summary)



