% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-recode_iycf.R
\name{recode_mmf}
\alias{recode_mmf}
\title{Function to recode minimum meal frequency indicator for the Myanmar MCCT
Programme Evaluation Survey}
\usage{
recode_mmf(df)
}
\arguments{
\item{df}{A data.frame collected for the Myanmar MCCT Programme Evaluation
Study containing IYCF/breastfeeding data}
}
\value{
A vector of recoded minimum meal frequency status
}
\description{
Function to recode minimum meal frequency indicator for the Myanmar MCCT
Programme Evaluation Survey
}
\examples{
iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
recode_mmf(df = iycfDF)

}
