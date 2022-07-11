################################################################################
#
#' Function to recode the birth/delivery indicators for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing birth/delivery data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A data.frame of recoded birth/delivery indicators
#'
#' @examples
#' ## Recode birth/delivery indicators
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_birth(df = x)
#'
#' @export
#'
#'
#
################################################################################

recode_birth <- function(df,
                         core.columns = c("KEY",
                                          "geo_rural",
                                          "geo_state",
                                          "geo_villward",
                                          "sample_component")) {
  ## Where delivered?
  birth1 <- suppressWarnings(as.integer(df$deliv_place))
  birth1 <- bbw::recode(var = birth1, recodes = "888=NA")
  ## Where: home
  birth1a <- bbw::recode(var = birth1, recodes = "1=1;NA=NA;else=0")
  ## Where: government hospital
  birth1b <- bbw::recode(var = birth1, recodes = "2=1;NA=NA;else=0")
  ## Where: private doctor/clinic
  birth1c <- bbw::recode(var = birth1, recodes = "3=1;NA=NA;else=0")
  ## Where: SRHC/RHC
  birth1d <- bbw::recode(var = birth1, recodes = "4=1;NA=NA;else=0")
  ## Where: Routine ANC location in village/ward
  birth1e <- bbw::recode(var = birth1, recodes = "5=1;NA=NA;else=0")
  ## Where: EHO clinic
  birth1f <- bbw::recode(var = birth1, recodes = "6=1;NA=NA;else=0")
  ## Gestational age at birth - months
  gestmonths <- suppressWarnings(as.integer(df$deliv_gestmonth))
  gestmonths <- bbw::recode(var = gestmonths, recodes = "444=NA;999=NA")
  ## Gestational age at birth - weeks
  gestweeks <- suppressWarnings(as.integer(df$deliv_gestweek))
  gestweeks <- bbw::recode(var = gestweeks, recodes = "444=NA;999=NA")
  ## gestational age in weeks
  gestage <- rowSums(cbind(gestmonths * 4, gestweeks), na.rm = TRUE)
  gestage <- ifelse(is.na(gestmonths), NA, gestage)
  ## Why choose delivery place?
  birth2 <- suppressWarnings(as.integer(df$deliv_place_why))
  ## Why: convenience
  birth2a <- suppressWarnings(as.integer(df$deliv_place_why.1))
  birth2a <- ifelse(is.na(birth2), NA, birth2a)
  ## Why: tradition
  birth2b <- suppressWarnings(as.integer(df$deliv_place_why.2))
  birth2b <- ifelse(is.na(birth2), NA, birth2b)
  ## Why: close distance
  birth2c <- suppressWarnings(as.integer(df$deliv_place_why.3))
  birth2c <- ifelse(is.na(birth2), NA, birth2c)
  ## Why: safety for mother/baby
  birth2d <- suppressWarnings(as.integer(df$deliv_place_why.4))
  birth2d <- ifelse(is.na(birth2), NA, birth2d)
  ## Why: affordable cost
  birth2e <- suppressWarnings(as.integer(df$deliv_place_why.5))
  birth2e <- ifelse(is.na(birth2), NA, birth2e)
  ## Who assisted in delivery?
  birth3 <- suppressWarnings(as.integer(df$deliv_assist))
  ## Who: doctor
  birth3a <- bbw::recode(var = birth3, recodes = "1=1;NA=NA;else=0")
  ## Who: nurse
  birth3b <- bbw::recode(var = birth3, recodes = "2=1;NA=NA;else=0")
  ## Who: LHV
  birth3c <- bbw::recode(var = birth3, recodes = "3=1;NA=NA;else=0")
  ## Who: midwife
  birth3d <- bbw::recode(var = birth3, recodes = "4=1;NA=NA;else=0")
  ## Who: auxilliary midwife
  birth3e <- bbw::recode(var = birth3, recodes = "5=1;NA=NA;else=0")
  ## Who: traditional birth attendant
  birth3f <- bbw::recode(var = birth3, recodes = "6=1;NA=NA;else=0")
  ## Who: on my own
  birth3g <- bbw::recode(var = birth3, recodes = "7=1;NA=NA;else=0")
  ## Who: relatives
  birth3h <- bbw::recode(var = birth3, recodes = "8=1;NA=NA;else=0")
  ## Who: EHO cadres
  birth3i <- bbw::recode(var = birth3, recodes = "9=1;NA=NA;else=0")
  ## Delivery assisted by a skilled birth attendant
  birth3 <- bbw::recode(var = birth3, recodes = "1:5=1;NA=NA;else=0")
  ## Delivery method
  birth4 <- suppressWarnings(as.integer(df$deliv_method))
  ## method: normal
  birth4a <- bbw::recode(var = birth4, recodes = "1=1;NA=NA;else=0")
  ## method: caesarian
  birth4b <- bbw::recode(var = birth4, recodes = "2=1;NA=NA;else=0")
  ## method: vacuum
  birth4c <- bbw::recode(var = birth4, recodes = "3=1;NA=NA;else=0")
  ## method: forceps
  birth4d <- bbw::recode(var = birth4, recodes = "4=1;NA=NA;else=0")
  ## delivery costs?
  birth5 <- suppressWarnings(as.integer(df$deliv_cost))
  ## deliverty costs amount
  birth6 <- suppressWarnings(as.integer(df$deliv_cost_amount))
  birth6 <- bbw::recode(var = birth6, recodes = "444=NA;666=NA;999=NA")
  ## Borrow?
  birth7 <- suppressWarnings(as.integer(df$deliv_cost_loan))
  birth7 <- bbw::recode(var = birth7, recodes = "999=NA")
  ## Concatenate birth/delivery indicators
  birth <- data.frame(df[ , core.columns],
                      birth1, birth1a, birth1b, birth1c, birth1d, birth1e, birth1f,
                      gestage, birth2, birth2a, birth2b, birth2c, birth2d, birth2e,
                      birth3, birth3a, birth3b, birth3c, birth3d, birth3e, birth3f,
                      birth3g, birth3h, birth3i,
                      birth4, birth4a, birth4b, birth4c, birth4d,
                      birth5, birth6, birth7)
  ## return data.frame
  return(birth)
}
