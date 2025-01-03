# plot.legor_fit()

plot.lego_fit <- function(x, pieces_grid = pretty(x$data$pieces, n = 100), ...) {

  # Check that the input is a lego_fit object
  if (!inherits(x, "lego_fit")) {
    stop("The input must be a 'lego_fit' object.")
  }

  # Extract the data
  df <- x$data

  # Generate predicted values based on the pieces grid and the fit_type
  fits <- switch(x$fit_type,
                 lm = {
                   tibble(pieces = pieces_grid, pred = predict(x$model,
                                                               newdata = tibble(pieces = pieces_grid)))
                 },
                 loess = {
                   tibble(pieces = pieces_grid, pred = predict(x$model,
                                                               newdata = tibble(pieces = pieces_grid))) |> na.omit()
                 },
                 polynomial = {
                   tibble(pieces = pieces_grid, pred = predict(x$model,
                                                               newdata = tibble(pieces = pieces_grid)))
                 })

  top3_df <- df %>%
    arrange(desc(us_retailprice)) %>%  # Sort by 'us_retailprice' in descending order
    head(3)                           # Get the top 3 rows

  # Create the plot with labels for the highest 3 points based on 'us_retailprice'
  ggplot(df, aes(x = pieces, y = us_retailprice)) +
    geom_point(aes(colour = us_retailprice)) +
    theme_bw() +
    xlab("Number of Pieces") +
    ylab("US Retail Price") +
    ggtitle(paste(x$fit_type, "Fit based on", x$data_type, "data")) +
    geom_line(data = fits, aes(x = pieces, y = pred, colour = pred)) +
    geom_text(data = top3_df,         # Use the top 3 highest points
              aes(label = name),      # Label using the 'name' column
              vjust = -0.5,           # Adjust vertical position
              colour = "black") +     # Label color
    theme(legend.position = "none") +
    scale_color_viridis_c()
}

## Examples
plot(fit_result)
plot(fit_result2)
plot(fit_result3)
