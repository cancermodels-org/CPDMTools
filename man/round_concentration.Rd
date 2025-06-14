% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round_concentration.R
\name{round_concentration}
\alias{round_concentration}
\title{Round concentration variables}
\usage{
round_concentration(data_frame, round_by = 4, use_nearest_10 = TRUE)
}
\arguments{
\item{data_frame}{A data frame object created from the data wrangling RShiny}

\item{round_by}{A numeric variable specifying the number of digits to round
by. Defaults to 4.}

\item{use_nearest_10}{A logical value specying whether to round the
concentrations to nearest 10 unit. Defaults to TRUE.}
}
\value{
A data frame object with the concentration variables rounded
}
\description{
Round concentration variables
}
\examples{
round_concentration(
data_frame = joined_data,
round_by = 4)
}
