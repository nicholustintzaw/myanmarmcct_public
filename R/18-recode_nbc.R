################################################################################
#
#' Function to recode newborn care indicators for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing newborn care data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A data.frame of recoded newborn care indicators
#'
#' @examples
#' ## Recode newborn care indicators
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_nbc(df = x)
#'
#' @export
#'
#'
#
################################################################################

recode_nbc <- function(df,
                       core.columns = c("KEY",
                                        "geo_rural",
                                        "geo_state",
                                        "geo_villward",
                                        "sample_component")) {
  ## within 24 hours after birth
  nbc1 <- suppressWarnings(as.integer(df$nbc_yn))
  nbc1 <- bbw::recode(var = nbc1, recodes = "999=0")
  ## within 48 hours after birth
  nbc2 <- suppressWarnings(as.integer(df$nbc_2days_yn))
  nbc2 <- bbw::recode(var = nbc2, recodes = "999=0")
  ## Who checked child?
  nbc3 <- suppressWarnings(as.integer(df$nbc_who))
  nbc3 <- bbw::recode(var = nbc3, recodes = "777=NA;999=NA")
  ## Who: doctor
  nbc3a <- suppressWarnings(as.integer(df$nbc_who.1))
  nbc3a <- ifelse(is.na(nbc3), NA, nbc3a)
  ## Who: nurse
  nbc3b <- suppressWarnings(as.integer(df$nbc_who.2))
  nbc3b <- ifelse(is.na(nbc3), NA, nbc3b)
  ## Who: LHV
  nbc3c <- suppressWarnings(as.integer(df$nbc_who.3))
  nbc3c <- ifelse(is.na(nbc3), NA, nbc3c)
  ## Who: midwife
  nbc3d <- suppressWarnings(as.integer(df$nbc_who.4))
  nbc3d <- ifelse(is.na(nbc3), NA, nbc3d)
  ## Who: auxilliary midwife
  nbc3e <- suppressWarnings(as.integer(df$nbc_who.5))
  nbc3e <- ifelse(is.na(nbc3), NA, nbc3e)
  ## Who: tba
  nbc3f <- suppressWarnings(as.integer(df$nbc_who.6))
  nbc3f <- ifelse(is.na(nbc3), NA, nbc3f)
  ## Who: relatives
  nbc3g <- suppressWarnings(as.integer(df$nbc_who.7))
  nbc3g <- ifelse(is.na(nbc3), NA, nbc3g)
  ## Who: EHO cadres
  nbc3h <- suppressWarnings(as.integer(df$nbc_who.8))
  nbc3h <- ifelse(is.na(nbc3), NA, nbc3h)
  ## How many times: doctor
  nbc4a <- suppressWarnings(as.integer(df$nbc_doc_freq))
  nbc4a <- bbw::recode(var = nbc4a, recodes = "444=NA;999=NA")
  ## How many times: nurse
  nbc4b <- suppressWarnings(as.integer(df$nbc_nurs_freq))
  nbc4b <- bbw::recode(var = nbc4b, recodes = "444=NA;999=NA")
  ## How many times: lhv
  nbc4c <- suppressWarnings(as.integer(df$nbc_lhv_freq))
  nbc4c <- bbw::recode(var = nbc4c, recodes = "444=NA;999=NA")
  ## How many times: midwife
  nbc4d <- suppressWarnings(as.integer(df$nbc_mw_freq))
  nbc4d <- bbw::recode(var = nbc4d, recodes = "444=NA;999=NA")
  ## How many times: auxilliary midwife
  nbc4e <- suppressWarnings(as.integer(df$nbc_amw_freq))
  nbc4e <- bbw::recode(var = nbc4e, recodes = "444=NA;999=NA")
  ## How many times: tba
  nbc4f <- suppressWarnings(as.integer(df$nbc_tba_freq))
  nbc4f <- bbw::recode(var = nbc4f, recodes = "444=NA;999=NA")
  ## How many times: relatives
  nbc4g <- suppressWarnings(as.integer(df$nbc_relative_freq))
  nbc4g <- bbw::recode(var = nbc4g, recodes = "444=NA;999=NA")
  ## How many times: EHO cadres
  nbc4h <- suppressWarnings(as.integer(df$nbc_eho_freq))
  nbc4h <- bbw::recode(var = nbc4h, recodes = "444=NA;999=NA")
  ## Costs?
  nbc5 <- suppressWarnings(as.integer(df$nbc_cost))
  nbc5 <- bbw::recode(var = nbc5, recodes = "999=NA")
  ## Amount?
  nbc6 <- suppressWarnings(as.integer(df$nbc_cost_amount))
  nbc6 <- bbw::recode(var = nbc6, recodes = "444=NA;999=NA")
  ## Cost items
  nbc7 <- suppressWarnings(as.integer(df$nbc_cost_items))
  ## items: transportation
  nbc7a <- suppressWarnings(as.integer(df$nbc_cost_items.1))
  nbc7a <- ifelse(is.na(nbc7), NA, nbc7a)
  ## items: registration fees
  nbc7b <- suppressWarnings(as.integer(df$nbc_cost_items.2))
  nbc7b <- ifelse(is.na(nbc7), NA, nbc7b)
  ## items: medicines
  nbc7c <- suppressWarnings(as.integer(df$nbc_cost_items.3))
  nbc7c <- ifelse(is.na(nbc7), NA, nbc7c)
  ## items: laboratory fees
  nbc7d <- suppressWarnings(as.integer(df$nbc_cost_items.4))
  nbc7d <- ifelse(is.na(nbc7), NA, nbc7d)
  ## items: provider fees
  nbc7e <- suppressWarnings(as.integer(df$nbc_cost_items.5))
  nbc7e <- ifelse(is.na(nbc7), NA, nbc7e)
  ## items: gifts
  nbc7f <- suppressWarnings(as.integer(df$nbc_cost_items.6))
  nbc7f <- ifelse(is.na(nbc7), NA, nbc7f)
  ## Borrow?
  nbc8 <- suppressWarnings(as.integer(df$nbc_cost_loan))
  nbc8 <- bbw::recode(var = nbc8, recodes = "999=NA")
  ## Colostrum?
  nbc9 <- suppressWarnings(as.integer(df$nbc_colostrum))
  nbc9 <- bbw::recode(var = nbc9, recodes = "999=0")
  ## Danger signs
  nbc10 <- suppressWarnings(as.integer(df$nbc_dangersigns))
  nbc10 <- bbw::recode(var = nbc10, recodes = "666=0;888=0;999=0")
  ## Danger signs: feeding less
  nbc10a <- suppressWarnings(as.integer(df$nbc_dangersigns.1))
  nbc10a <- ifelse(is.na(nbc10), NA, nbc10a)
  ## Danger signs: convulsion
  nbc10b <- suppressWarnings(as.integer(df$nbc_dangersigns.2))
  nbc10b <- ifelse(is.na(nbc10), NA, nbc10b)
  ## Danger signs: high or low temperature
  nbc10c <- suppressWarnings(as.integer(df$nbc_dangersigns.3))
  nbc10c <- ifelse(is.na(nbc10), NA, nbc10c)
  ## Danger signs: local infection
  nbc10d <- suppressWarnings(as.integer(df$nbc_dangersigns.4))
  nbc10d <- ifelse(is.na(nbc10), NA, nbc10d)
  ## Danger signs: no movement or less movement
  nbc10e <- suppressWarnings(as.integer(df$nbc_dangersigns.5))
  nbc10e <- ifelse(is.na(nbc10), NA, nbc10e)
  ## Danger signs: fast or difficulty in breathing
  nbc10f <- suppressWarnings(as.integer(df$nbc_dangersigns.6))
  nbc10f <- ifelse(is.na(nbc10), NA, nbc10f)
  ## Danger signs: yellow skin
  nbc10g <- suppressWarnings(as.integer(df$nbc_dangersigns.7))
  nbc10g <- ifelse(is.na(nbc10), NA, nbc10g)
  ## Does not know any of the danger signs
  nbc11 <- bbw::recode(var = nbc10, recodes = "0=1;NA=NA;else=0")
  ## Cord care
  nbc12 <- suppressWarnings(as.integer(df$nbc_cordcare))
  nbc12 <- bbw::recode(var = nbc12, recodes = "888=0;999=0")
  ## cord care: nothing
  nbc12a <- suppressWarnings(as.integer(df$nbc_cordcare.1))
  nbc12a <- ifelse(is.na(nbc12), NA, nbc12a)
  ## cord care: betadine
  nbc12b <- suppressWarnings(as.integer(df$nbc_cordcare.2))
  nbc12b <- ifelse(is.na(nbc12), NA, nbc12b)
  ## cord care: spirits
  nbc12c <- suppressWarnings(as.integer(df$nbc_cordcare.3))
  nbc12c <- ifelse(is.na(nbc12), NA, nbc12c)
  ## cord care: turmeric
  nbc12d <- suppressWarnings(as.integer(df$nbc_cordcare.4))
  nbc12d <- ifelse(is.na(nbc12), NA, nbc12d)
  ## cord care: Brick's powder
  nbc12e <- suppressWarnings(as.integer(df$nbc_cordcare.5))
  nbc12e <- ifelse(is.na(nbc12), NA, nbc12e)
  ## Inappropriate cord care
  nbc13 <- bbw::recode(var = nbc12, recodes = "1:2=0;NA=NA;else=1")
  ## delivery wound care
  nbc14 <- suppressWarnings(as.integer(df$nbc_deliwound))
  nbc14 <- bbw::recode(var = nbc14, recodes = "666=0;888=0;999=0")
  ## wound care: nothing
  nbc14a <- suppressWarnings(as.integer(df$nbc_deliwound.1))
  nbc14a <- ifelse(is.na(nbc14), NA, nbc14a)
  ## wound care: betadine
  nbc14b <- suppressWarnings(as.integer(df$nbc_deliwound.2))
  nbc14b <- ifelse(is.na(nbc14), NA, nbc14b)
  ## wound care: spirits
  nbc14c <- suppressWarnings(as.integer(df$nbc_deliwound.3))
  nbc14c <- ifelse(is.na(nbc14), NA, nbc14c)
  ## wound care: turmeric
  nbc14d <- suppressWarnings(as.integer(df$nbc_deliwound.4))
  nbc14d <- ifelse(is.na(nbc14), NA, nbc14d)
  ## wound care: Brick's powder
  nbc14e <- suppressWarnings(as.integer(df$nbc_deliwound.5))
  nbc14e <- ifelse(is.na(nbc14), NA, nbc14e)
  ## inappropriate wound care
  nbc15 <- bbw::recode(var = nbc14, recodes = "1:2=0;NA=NA;else=1")
  ## Concatenate newborn care indicators to data.frame
  nbc <- data.frame(df[, core.columns],
                    nbc1, nbc2, nbc3, nbc3a, nbc3b, nbc3c, nbc3d, nbc3e,
                    nbc3f, nbc3g, nbc3h, nbc4a, nbc4b, nbc4c, nbc4d, nbc4e,
                    nbc4f, nbc4g, nbc4h, nbc5, nbc6, nbc7, nbc7a, nbc7b, nbc7c,
                    nbc7d, nbc7e, nbc7f, nbc8, nbc9, nbc10, nbc10a, nbc10b,
                    nbc10c, nbc10d, nbc10e, nbc10f, nbc10g, nbc11, nbc12,
                    nbc12a, nbc12b, nbc12c, nbc12d, nbc12e, nbc13, nbc14,
                    nbc14a, nbc14b, nbc14c, nbc14d, nbc14e, nbc15)
  ## Return data.frame
  return(nbc)
}
