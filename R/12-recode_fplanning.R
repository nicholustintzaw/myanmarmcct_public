################################################################################
#
#' Function to recode family planning indicators for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing family planning data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded family planning indicators
#'
#' @examples
#' recode_fplan(df = hh)
#'
#' @export
#'
#
################################################################################

recode_fplan <- function(df,
                         core.columns = c("KEY",
                                          "geo_state",
                                          "geo_rural",
                                          "geo_villward",
                                          "sample_component")) {
  ## How long to wait in months
  fp1 <- suppressWarnings(as.integer(df$plan_wait))
  fp1 <- bbw::recode(var = fp1, recodes = "0=NA;666=NA;999=NA")
  ## How long to wait in months if currently pregnant
  fp2 <- suppressWarnings(as.integer(df$plan_wait_preg))
  fp2 <- bbw::recode(var = fp2, recodes = "666=NA;999=NA")
  ## How long to wait - husband
  fp3 <- suppressWarnings(as.integer(df$plan_husb_wait))
  fp3 <- bbw::recode(var = fp3, recodes = "0=NA;666=NA;999=NA")
  ## Using method now?
  fp4 <- suppressWarnings(as.integer(df$plan_using_now))
  ## Used methods before?
  fp5 <- suppressWarnings(as.integer(df$plan_haveused))
  ## Methods used/using
  fp6 <- suppressWarnings(as.integer(df$plan_method))
  fp6 <- bbw::recode(var = fp6, recodes = "888=NA")
  ## Methods used/using: female sterilisation
  fp6a <- suppressWarnings(as.integer(df$plan_method.1))
  fp6a <- ifelse(is.na(fp6), NA, fp6a)
  ## Methods used/using: male sterilisation
  fp6b <- suppressWarnings(as.integer(df$plan_method.2))
  fp6b <- ifelse(is.na(fp6), NA, fp6b)
  ## Methods used/using: IUD
  fp6c <- suppressWarnings(as.integer(df$plan_method.3))
  fp6c <- ifelse(is.na(fp6), NA, fp6c)
  ## Methods used/using: Implant
  fp6d <- suppressWarnings(as.integer(df$plan_method.4))
  fp6d <- ifelse(is.na(fp6), NA, fp6d)
  ## Methods used/using: Injectable
  fp6e <- suppressWarnings(as.integer(df$plan_method.5))
  fp6e <- ifelse(is.na(fp6), NA, fp6e)
  ## Methods used/using: pills
  fp6f <- suppressWarnings(as.integer(df$plan_method.6))
  fp6f <- ifelse(is.na(fp6), NA, fp6f)
  ## Methods used/using: male condom
  fp6g <- suppressWarnings(as.integer(df$plan_method.7))
  fp6g <- ifelse(is.na(fp6), NA, fp6g)
  ## Methods used/using: female condom
  fp6h <- suppressWarnings(as.integer(df$plan_method.8))
  fp6h <- ifelse(is.na(fp6), NA, fp6h)
  ## Methods used/using: lactational amenorrhea
  fp6i <- suppressWarnings(as.integer(df$plan_method.9))
  fp6i <- ifelse(is.na(fp6), NA, fp6i)
  ## Methods used/using: rhythm method
  fp6j <- suppressWarnings(as.integer(df$plan_method.10))
  fp6j <- ifelse(is.na(fp6), NA, fp6j)
  ## Methods used/using: withrawal
  fp6k <- suppressWarnings(as.integer(df$plan_method.11))
  fp6k <- ifelse(is.na(fp6), NA, fp6k)
  ## Methods used/using: emergency contraception
  fp6l <- suppressWarnings(as.integer(df$plan_method.12))
  fp6l <- ifelse(is.na(fp6), NA, fp6l)
  ## Methods used/using: diaphragm
  fp6m <- suppressWarnings(as.integer(df$plan_method.13))
  fp6m <- ifelse(is.na(fp6), NA, fp6m)
  ## Methods used/using: foam/jelly
  fp6n <- suppressWarnings(as.integer(df$plan_method.14))
  fp6n <- ifelse(is.na(fp6), NA, fp6n)
  ## Where family planning services received?
  fp7 <- suppressWarnings(as.integer(df$plan_method_provider))
  fp7 <- bbw::recode(var = fp7, recodes = "666=NA;888=NA;999=NA")
  ## Where: government hospital
  fp7a <- suppressWarnings(as.integer(df$plan_method_provider.1))
  fp7a <- ifelse(is.na(fp7), NA, fp7a)
  ## Where: government health centre/health post
  fp7b <- suppressWarnings(as.integer(df$plan_method_provider.2))
  fp7b <- ifelse(is.na(fp7), NA, fp7b)
  ## Where: government village health worker
  fp7c <- suppressWarnings(as.integer(df$plan_method_provider.3))
  fp7c <- ifelse(is.na(fp7), NA, fp7c)
  ## Where: UHC/MCH centre
  fp7d <- suppressWarnings(as.integer(df$plan_method_provider.4))
  fp7d <- ifelse(is.na(fp7), NA, fp7d)
  ## Where: private hospital
  fp7e <- suppressWarnings(as.integer(df$plan_method_provider.5))
  fp7e <- ifelse(is.na(fp7), NA, fp7e)
  ## Where: private doctor
  fp7f <- suppressWarnings(as.integer(df$plan_method_provider.6))
  fp7f <- ifelse(is.na(fp7), NA, fp7f)
  ## Where: pharmacy
  fp7g <- suppressWarnings(as.integer(df$plan_method_provider.7))
  fp7g <- ifelse(is.na(fp7), NA, fp7g)
  ## Where: NGO
  fp7h <- suppressWarnings(as.integer(df$plan_method_provider.8))
  fp7h <- ifelse(is.na(fp7), NA, fp7h)
  ## Where: EHO clinic
  fp7i <- suppressWarnings(as.integer(df$plan_method_provider.9))
  fp7i <- ifelse(is.na(fp7), NA, fp7i)
  ## Where: auxilliary midwife
  fp7j <- suppressWarnings(as.integer(df$plan_method_provider.10))
  fp7j <- ifelse(is.na(fp7), NA, fp7j)
  ## Where: midwife
  fp7k <- suppressWarnings(as.integer(df$plan_method_provider.11))
  fp7k <- ifelse(is.na(fp7), NA, fp7k)
  ## Information on family planning in past 12 months
  fp8 <- suppressWarnings(as.integer(df$plan_info))
  ## Who provided information?
  fp9 <- suppressWarnings(as.integer(df$plan_info_who))
  fp9 <- bbw::recode(var = fp9, recodes = "888=NA;999=NA")
  ## Who: government hospital
  fp9a <- suppressWarnings(as.integer(df$plan_info_who.1))
  fp9a <- ifelse(is.na(fp9), NA, fp9a)
  ## Who: government health centre/health post
  fp9b <- suppressWarnings(as.integer(df$plan_info_who.2))
  fp9b <- ifelse(is.na(fp9), NA, fp9b)
  ## Who: government village health worker
  fp9c <- suppressWarnings(as.integer(df$plan_info_who.3))
  fp9c <- ifelse(is.na(fp9), NA, fp9c)
  ## Who: UHC/MCH centre
  fp9d <- suppressWarnings(as.integer(df$plan_info_who.4))
  fp9d <- ifelse(is.na(fp9), NA, fp9d)
  ## Who: private hospital
  fp9e <- suppressWarnings(as.integer(df$plan_info_who.5))
  fp9e <- ifelse(is.na(fp9), NA, fp9e)
  ## Who: private doctor
  fp9f <- suppressWarnings(as.integer(df$plan_info_who.6))
  fp9f <- ifelse(is.na(fp9), NA, fp9f)
  ## Who: pharmacy
  fp9g <- suppressWarnings(as.integer(df$plan_info_who.7))
  fp9g <- ifelse(is.na(fp9), NA, fp9g)
  ## Who: NGO
  fp9h <- suppressWarnings(as.integer(df$plan_info_who.8))
  fp9h <- ifelse(is.na(fp9), NA, fp9h)
  ## Who: EHO clinic
  fp9i <- suppressWarnings(as.integer(df$plan_info_who.9))
  fp9i <- ifelse(is.na(fp9), NA, fp9i)
  ## Who: auxilliary midwife
  fp9j <- suppressWarnings(as.integer(df$plan_info_who.10))
  fp9j <- ifelse(is.na(fp9), NA, fp9j)
  ## Who: midwife
  fp9k <- suppressWarnings(as.integer(df$plan_info_who.11))
  fp9k <- ifelse(is.na(fp9), NA, fp9k)
  ## Knowlege: what is the minimum wait?
  fp10 <- suppressWarnings(as.integer(df$plan_min_waittime))
  ## wait 1 year
  fp10a <- bbw::recode(var = fp10, recodes = "1=1;NA=NA;999=NA;else=0")
  ## wait 2 years
  fp10b <- bbw::recode(var = fp10, recodes = "1=1;NA=NA;999=NA;else=0")
  ## wait 3 years
  fp10c <- bbw::recode(var = fp10, recodes = "1=1;NA=NA;999=NA;else=0")
  ## wait over 3 years
  fp10d <- bbw::recode(var = fp10, recodes = "1=1;NA=NA;999=NA;else=0")
  ## Appropriate knowledge on minimum wait
  fp10 <- bbw::recode(var = fp10, recodes = "1=0;2:4=1;999=0;NA=NA")
  ## Concatenate family planning indicators to data.frame
  fp <- data.frame(df[ , core.columns],
                   fp1, fp2, fp3, fp4, fp5,
                   fp6, fp6a, fp6b, fp6c, fp6d, fp6e, fp6f, fp6g, fp6h,
                   fp6i, fp6j, fp6k, fp6l, fp6m, fp6n,
                   fp7, fp7a, fp7b, fp7c, fp7d, fp7e, fp7f, fp7g, fp7h,
                   fp7i, fp7j, fp7k,
                   fp8,
                   fp9, fp9a, fp9b, fp9c, fp9d, fp9e, fp9f, fp9g, fp9h,
                   fp9i, fp9j, fp9k,
                   fp10, fp10a, fp10b, fp10c, fp10d)
  ## Return data.frame
  return(fp)
}
