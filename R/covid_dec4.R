#' @name covid_dec4
#' @title COVID-19 deaths
#' @description This data set contains the number of confirmed and probable
#'   COVID-19 deaths in U.S. states through December 4th, 2020 along with some
#'   very basic demographic information. While the data are taken from official
#'   sources, they are only intended for demonstration purposes.
#'
#' @docType data
#' @usage data(covid_dec4)
#'
#' @format A data frame with 50 rows and 7 variables
#' \describe{
#'   \item{state_name}{state name}
#'   \item{state_abb}{state name abbreviation}
#'   \item{deaths}{confirmed and probable number of COVID-19 deaths}
#'   \item{population}{population 5-year estimate from the 2017 American
#'   Community Survey}
#'   \item{income}{median income (U.S. dollars) 5-year
#'   estimate from the 2017 American Community Survey}
#'   \item{hs}{estimated
#'   percentage of population > 25 years old with a high school diploma based on
#'   the 2010 Census}
#'   \item{bs}{estimated percentage of population > 25 years
#'   old with a Bachelor's degree based on the 2010 Census}
#' }
#' @source <https://covidtracking.com/data/download> (Accessed 2020-12-04) and <https://www.census.gov>
NULL
