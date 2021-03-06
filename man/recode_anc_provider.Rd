% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-recode_anc.R
\name{recode_anc_provider}
\alias{recode_anc_provider}
\title{Function to recode the personnel type providing antenatal care services for
the Myanmar MCCT Programme Evaluation survey}
\usage{
recode_anc_provider(df, status = NULL)
}
\arguments{
\item{df}{A data.frame collected for the Myanmar MCCT Programme Evaluation
Study containing antenatal care data for currently pregnant women}

\item{status}{A character value indicating whether to recode data of
currently pregnant women ("current") or previously pregnant women ("past")}
}
\value{
A data.frame of recoded antenatal care providers visited for
  antenatal care
}
\description{
Function to recode the personnel type providing antenatal care services for
the Myanmar MCCT Programme Evaluation survey
}
\examples{
## Recode anc provider for currently pregnant women
x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
recode_anc_provider(df = x, status = "current")

## Recode anc provider for non-pregnant women
x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
recode_anc_provider(df = x, status = "past")

}
