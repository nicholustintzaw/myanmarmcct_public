% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06-recode_chealth.R
\name{recode_chealth}
\alias{recode_chealth}
\title{Function to recode child health indicators for the Myanmar MCCT
Programme Evaluation Survey}
\usage{
recode_chealth(
  df,
  core.columns = c("KEY", "geo_state", "geo_rural", "geo_villward", "sample_component")
)
}
\arguments{
\item{df}{A data.frame collected for the Myanmar MCCT Programme Evaluation
Study containing child health data}

\item{core.columns}{A vector of variable names to include in resulting
data.frame}
}
\value{
A vector of recoded child health indicators
}
\description{
Function to recode child health indicators for the Myanmar MCCT
Programme Evaluation Survey
}
\examples{
chealth <- create_chealth(df = childHealth, x = hh, y = hhMembers)
recode_chealth(df = chealth)

}
