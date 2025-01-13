
---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# legor R Package
### Written by [Airmid, Megan, Robert, Aaron]

<!-- badges: start -->
<!-- badges: end -->

## Description

`legor` is an R package designed for analyzing and visualizing LEGO datasets. It provides tools to:

1. Fit models (linear, LOESS, polynomial) to LEGO data using the `fit()` function.
2. Visualize data and models using the `plot()` method with `ggplot2`-based graphics.
3. Highlight key insights, such as top-priced LEGO sets, in visualizations.

Typically, these functions are applied sequentially. This README demonstrates a typical workflow for the `legor` package.

## Installation

You can install the development version of `legor` from [GitHub](https://github.com/airmid-ms/legor) with:

```r
# install.packages("devtools")
devtools::install_github("airmid-ms/legor")
```

## Functions Overview

- `fit()`: Fits regression models (linear, LOESS, polynomial) to LEGO datasets.
- `plot()`: Visualizes the fitted models and highlights important data points.
- Additional utility functions for data manipulation and analysis.

## Example

This is a basic example showing how to analyze LEGO data:

```r
library(legor)

# Load a LEGO dataset (ensure your data is in the correct format)
data <- lego_data  # Replace with your data

# Fit a polynomial regression model
fit_result <- fit(data, fit_type = "polynomial", polynomial_degree = 3, year_range = c(2010, 2020))

# Visualize the fitted model
plot(fit_result)
```

### Steps

1. **Fit a Model:**
   Fit a specified regression model to the LEGO data. Supported options include:
   - `lm` (Linear model)
   - `loess` (LOESS smoothing)
   - `polynomial` (Polynomial regression)

   ```r
   fit_result <- fit(data, fit_type = "lm")
   ```

2. **Plot the Results:**
   Visualize the fitted model with the dataset:
   ```r
   plot(fit_result)
   ```

3. **Highlight Insights:**
   The plot automatically labels the top 3 LEGO sets with the highest retail prices.

For more details, visit(https://github.com/airmid-ms/legor)
