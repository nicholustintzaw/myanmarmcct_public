% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-recode_iycf.R
\name{create_iycf}
\alias{create_iycf}
\title{Function to create raw IYCF dataset}
\usage{
create_iycf(df, x, y)
}
\arguments{
\item{df}{A data.frame collected for the Myanmar MCCT Programme Evaluation
Study containing IYCF data}

\item{x}{A data.frame collected for the Myanmar MCCT Programme Evaluation
Study containing household information that will be linked to the
IYCF data}

\item{y}{A data.frame collected for the Myanmar MCCT Programme Evaluation
Study containing household roster information that will be linked to the
IYCF data}
}
\value{
A data.frame containing raw IYCF data for the Myanmar
  MCCT Programme Evaluation Study
}
\description{
Function to create raw IYCF dataset
}
\examples{
create_iycf(df = iycf, x = hh, y = hhMembers)

}
