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
    ggtitle(paste(x$fit_type, "Fit based on lego_data")) +
    geom_line(data = fits, aes(x = pieces, y = pred, colour = pred)) +
    geom_text(data = top3_df,
              aes(label = name),
              vjust = -0.5) +
    scale_color_viridis_c(option = "inferno")
}

## Examples
plot(model)
plot(model2)
plot(model3)
