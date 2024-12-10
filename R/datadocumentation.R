#' @title LEGO Dataset
#'
#' @description The data was taken from a dataset comprised of all lego sets from Kaggle.
#'
#' @format A data frame with 2,669 observations on 14 variables.
#' \describe{
#' \item{set_id}{Lego set identification number.}
#' \item{name}{Name of the Lego set.}
#' \item{year}{Release year of the lego set.}
#' \item{theme}{Theme or brand of the Lego set.}
#' \item{subtheme}{Subtheme of the Lego set}
#' \item{themegroup}{The generic group of themes a Lego set belongs to.}
#' \item{category}{Refers to whether a Lego set is Normal or Extended.}
#' \item{pieces}{Number of pieces in a Lego set.}
#' \item{minifigs}{Number of miniature figurines included in the Lego set.}
#' \item{agerange_min}{The recommended minimum age for the Lego set.}
#' \item{us_retailprice}{The US retail price in US dollars at the time of the datasets creation.}}
#'
#' @source unknown
#'
#' @examples
#' data(lego_data)
"lego_data"
