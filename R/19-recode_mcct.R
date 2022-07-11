################################################################################
#
#' Function to create dataset with individual-level MCCT-related data from the
#' Myanmar MCCT Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing MCCT-related data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household member information that will be linked to the
#'   MCCT data
#'
#' @return A data.frame of recoded MCCT-related indicators for children and
#'   pregnant women
#'
#' @examples
#' ## Recode individual MCCT-related indicators
#' create_mcct(df = hh, x = hhMembers)
#'
#' @export
#
################################################################################

create_mcct <- function(df, x) {
  x1 <- df[ , c("KEY", "geo_rural", "geo_state",
                "geo_villward", "sample_component", "intrv_date",
                names(df)[stringr::str_detect(string = names(df),
                                              pattern = "mcct")])]
  ##
  x2 <- x[ , c("PARENT_KEY", "hh_mem_dob", "hh_mem_age", "hh_mem_pregnow")]
  ##
  x <- merge(x1, x2, by.x = "KEY", by.y = "PARENT_KEY", all.y = TRUE)
  ##
  dob <- lubridate::mdy(x$hh_mem_dob)
  # Calculate age in years
  age <- as.numeric(lubridate::mdy(x$intrv_date) - lubridate::date(dob)) / 365
  age <- ifelse(is.na(age), as.integer(x$hh_mem_age), age)
  ## Calculated age in days
  age_in_days <- as.numeric(lubridate::date(lubridate::mdy(x$intrv_date)) - lubridate::date(dob))
  ##
  age_in_days[is.na(age_in_days)] <- age[is.na(age_in_days)] * 365.25 #(365.25 / 12)
  ##
  dob[is.na(dob)] <- lubridate::mdy(x$intrv_date)[is.na(dob)] - age_in_days[is.na(dob)]
  ##
  eligible <- ifelse(lubridate::date(dob) %within%
                       lubridate::interval(start = lubridate::ymd("2018/10/01"),
                                           end = lubridate::ymd(Sys.Date())), 1,
                ifelse(as.integer(x$hh_mem_pregnow) == 1, 1, 0))
  ##
  x <- data.frame(x, dob, age_in_days, eligible)
  ##
  mcctDF <- x[x$eligible == 1 & !is.na(x$eligible), ]
  ##
  return(mcctDF)
}


################################################################################
#
#' Function to create dataset with household-level MCCT-related data from the
#' Myanmar MCCT Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing houehold-level MCCT-related data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household member information that will be linked to the
#'   MCCT data
#'
#' @return A data.frame of recoded MCCT-related indicators for households
#'
#' @examples
#' ## Recode household MCCT-related indicators
#' create_mcct_household(df = hh, x = hhMembers)
#'
#' @export
#
################################################################################

create_mcct_household <- function(df, x) {
  x1 <- df[ , c("KEY", "geo_rural", "geo_state",
                "geo_villward", "sample_component", "intrv_date",
                names(df)[stringr::str_detect(string = names(df),
                                              pattern = "mcct")])]
  ##
  x2 <- x[ , c("PARENT_KEY", "hh_mem_dob", "hh_mem_age", "hh_mem_pregnow")]
  ##
  x <- merge(x1, x2, by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
  ##
  dob <- lubridate::mdy(x$hh_mem_dob)
  #
  age <- as.numeric(lubridate::mdy(x$intrv_date) - lubridate::date(dob)) / 365
  age <- ifelse(is.na(age), as.integer(x$hh_mem_age), age)
  ## Calculated age in days
  age_in_days <- as.numeric(lubridate::date(lubridate::mdy(x$intrv_date)) - lubridate::date(dob))
  ##
  age_in_days[is.na(age_in_days)] <- age[is.na(age_in_days)] * 365.25 #(365.25 / 12)
  ##
  dob[is.na(dob)] <- lubridate::mdy(x$intrv_date)[is.na(dob)] - age_in_days[is.na(dob)]
  ##
  eligible <- ifelse(lubridate::date(dob) %within%
                       lubridate::interval(start = lubridate::ymd("2018/10/01"),
                                           end = lubridate::ymd(Sys.Date())), 1,
                     ifelse(as.integer(x$hh_mem_pregnow) == 1, 1, 0))
  #
  y <- data.frame(x, eligible)
  y <- aggregate(eligible ~ KEY, data = y, FUN = sum)
  ##
  #x <- data.frame(x, dob, age_in_days)
  ##
  x <- merge(x1, y, by = "KEY", all.x = TRUE)
  ##
  mcctDF <- x[x$eligible > 0 & !is.na(x$eligible), ]
  ##
  return(mcctDF)
}


################################################################################
#
#' Function to recode MCCT-related indicators for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing MCCT-related data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A data.frame of recoded MCCT-related indicators
#'
#' @examples
#' ## Recode MCCT-related indicators
#' mcctDF <- create_mcct(df = hh, x = hhMembers)
#' recode_mcct(df = mcctDF)
#'
#' @export
#'
#'
#
################################################################################

recode_mcct <- function(df,
                        core.columns = c("KEY",
                                         "geo_rural",
                                         "geo_state",
                                         "geo_villward",
                                         "sample_component")) {
  ## In MCCT?
  mcct1 <- suppressWarnings(as.integer(df$mcct_yesno))
  mcct1 <- bbw::recode(var = mcct1, recodes = "999=0")
  ## Number of transfers
  mcct2 <- suppressWarnings(as.integer(df$mcct_times))
  mcct2 <- bbw::recode(var = mcct2, recodes = "444=NA")
  ## Receipt of cash transfer
  mcct3 <- suppressWarnings(as.integer(df$mcct_org))
  ## Receipt: mobile programme
  mcct3a <- suppressWarnings(as.integer(df$mcct_org.1))
  mcct3a <- ifelse(is.na(mcct3), NA, mcct3a)
  ## Receipt: wave money
  mcct3b <- suppressWarnings(as.integer(df$mcct_org.2))
  mcct3b <- ifelse(is.na(mcct3), NA, mcct3b)
  ## Receipt: from village head/GAD
  mcct3c <- suppressWarnings(as.integer(df$mcct_org.3))
  mcct3c <- ifelse(is.na(mcct3), NA, mcct3c)
  ## Receipt: from EHO
  mcct3d <- suppressWarnings(as.integer(df$mcct_org.4))
  mcct3d <- ifelse(is.na(mcct3), NA, mcct3d)
  ## Receipt: from CBO
  mcct3e <- suppressWarnings(as.integer(df$mcct_org.5))
  mcct3e <- ifelse(is.na(mcct3), NA, mcct3e)
  ## Receipt: from midwife
  mcct3f <- suppressWarnings(as.integer(df$mcct_org.6))
  mcct3f <- ifelse(is.na(mcct3), NA, mcct3f)
  ## Receipt: other health staff
  mcct3g <- suppressWarnings(as.integer(df$mcct_org.7))
  mcct3g <- ifelse(is.na(mcct3), NA, mcct3g)
  ## check growth chart
  mcct4 <- suppressWarnings(as.integer(df$mcct_growth))
  mcct4 <- bbw::recode(var = mcct4, recodes = "999=0")
  ## use of cash
  mcct5 <- suppressWarnings(as.integer(df$mcct_cashusage))
  ## use: food
  mcct5a <- suppressWarnings(as.integer(df$mcct_cashusage.1))
  mcct5a <- ifelse(is.na(mcct5), NA, mcct5a)
  ## use: Education
  mcct5b <- suppressWarnings(as.integer(df$mcct_cashusage.2))
  mcct5b <- ifelse(is.na(mcct5), NA, mcct5b)
  ## use: Housing
  mcct5c <- suppressWarnings(as.integer(df$mcct_cashusage.3))
  mcct5c <- ifelse(is.na(mcct5), NA, mcct5c)
  ## use: clothes
  mcct5d <- suppressWarnings(as.integer(df$mcct_cashusage.4))
  mcct5d <- ifelse(is.na(mcct5), NA, mcct5d)
  ## Decision
  mcct6 <- suppressWarnings(as.integer(df$mcct_decision))
  ## Decision: self
  mcct6a <- suppressWarnings(as.integer(df$mcct_decision.1))
  mcct6a <- ifelse(is.na(mcct6), NA, mcct6a)
  ## Decision: Husband
  mcct6b <- suppressWarnings(as.integer(df$mcct_decision.2))
  mcct6b <- ifelse(is.na(mcct6), NA, mcct6b)
  ## Decision: other head of household
  mcct6c <- suppressWarnings(as.integer(df$mcct_decision.3))
  mcct6c <- ifelse(is.na(mcct6), NA, mcct6c)
  ## Decision: other
  mcct6d <- suppressWarnings(as.integer(df$mcct_decision.4))
  mcct6d <- ifelse(is.na(mcct6), NA, mcct6d)
  ## Reason for not receiving cash transfer
  mcct7 <- suppressWarnings(as.integer(df$mcct_no))
  ## Reason: don't know about programme
  mcct7a <- suppressWarnings(as.integer(df$mcct_no.1))
  mcct7a <- ifelse(is.na(mcct7), NA, mcct7a)
  ## Reason: cannot receive on phone
  mcct7b <- suppressWarnings(as.integer(df$mcct_no.2))
  mcct7b <- ifelse(is.na(mcct7), NA, mcct7b)
  ## Reason: difficult to reach transfer source
  mcct7c <- suppressWarnings(as.integer(df$mcct_no.3))
  mcct7c <- ifelse(is.na(mcct7), NA, mcct7c)
  ## Reason: not interested
  mcct7d <- suppressWarnings(as.integer(df$mcct_no.4))
  mcct7d <- ifelse(is.na(mcct7), NA, mcct7d)
  ## Reason: not eligible
  mcct7e <- suppressWarnings(as.integer(df$mcct_no.5))
  mcct7e <- ifelse(is.na(mcct7), NA, mcct7e)
  ## Reason: eligible but not included in registration
  mcct7f <- suppressWarnings(as.integer(df$mcct_no.6))
  mcct7f <- ifelse(is.na(mcct7), NA, mcct7f)
  ## Recode others here

  ## SBCC attendance
  mcct8 <- suppressWarnings(as.integer(df$mcct_bcc))
  mcct8 <- bbw::recode(var = mcct8, recodes = "999=0")
  ## How many times
  mcct9 <- suppressWarnings(as.integer(df$mcct_bcc_num))
  mcct9 <- bbw::recode(var = mcct9, recodes = "444=NA;999=NA")
  ## Concatenate MCCT-related indicators to data.frame
  mcct <- data.frame(df[ , core.columns],
                     mcct1, mcct2, mcct3, mcct3a, mcct3b, mcct3c, mcct3d,
                     mcct3e, mcct3f, mcct3g,
                     mcct4, mcct5, mcct5a, mcct5b, mcct5c, mcct5d,
                     mcct6, mcct6a, mcct6b, mcct6c, mcct6d,
                     mcct7, mcct7a, mcct7b, mcct7c, mcct7d, mcct7e, mcct7f,
                     mcct8, mcct9)
  ## Return data.frame
  return(mcct)
}



