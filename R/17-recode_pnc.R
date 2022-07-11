################################################################################
#
#' Function to recode postnatal care indicators for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing postnatal care data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A data.frame of recoded postnatal care indicators
#'
#' @examples
#' ## Recode postnatal care indicators
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_pnc(df = x)
#'
#' @export
#'
#'
#
################################################################################

recode_pnc <- function(df,
                       core.columns = c("KEY",
                                        "geo_rural",
                                        "geo_state",
                                        "geo_villward",
                                        "sample_component")) {
  ## PNC?
  pnc1 <- suppressWarnings(as.integer(df$pnc_yn))
  ## Health check?
  pnc2 <- suppressWarnings(as.integer(df$pnc_checktime))
  pnc2 <- bbw::recode(var = pnc2, recodes = "444=NA")
  pnc2 <- ifelse(df$pnc_checkunit == "2", pnc2 * 24, pnc2)
  pnc2 <- bbw::recode(var = pnc2, recodes = "0=NA")
  ## PNC within 2 days of delivery
  pnc2a <- ifelse(pnc1 == 1 & pnc2 <= 48, 1, 0)
  pnc2a[is.na(pnc2)] <- NA
  ## Who did PNC check?
  pnc3 <- suppressWarnings(as.integer(df$pnc_who))
  pnc3 <- bbw::recode(var = pnc3, recodes = "777=NA;888=NA")
  ## Who: doctor
  pnc3a <- suppressWarnings(as.integer(df$pnc_who.1))
  pnc3a <- ifelse(is.na(pnc3), NA, pnc3a)
  ## Who: nurse
  pnc3b <- suppressWarnings(as.integer(df$pnc_who.2))
  pnc3b <- ifelse(is.na(pnc3), NA, pnc3b)
  ## Who: LHV
  pnc3c <- suppressWarnings(as.integer(df$pnc_who.3))
  pnc3c <- ifelse(is.na(pnc3), NA, pnc3c)
  ## Who: midwife
  pnc3d <- suppressWarnings(as.integer(df$pnc_who.4))
  pnc3d <- ifelse(is.na(pnc3), NA, pnc3d)
  ## Who: auxilliary midwife
  pnc3e <- suppressWarnings(as.integer(df$pnc_who.5))
  pnc3e <- ifelse(is.na(pnc3), NA, pnc3e)
  ## Who: traditional birth attendant
  pnc3f <- suppressWarnings(as.integer(df$pnc_who.6))
  pnc3f <- ifelse(is.na(pnc3), NA, pnc3f)
  ## Who: relatives
  pnc3g <- suppressWarnings(as.integer(df$pnc_who.7))
  pnc3g <- ifelse(is.na(pnc3), NA, pnc3g)
  ## Who: EHO cadres
  pnc3h <- suppressWarnings(as.integer(df$pnc_who.8))
  pnc3h <- ifelse(is.na(pnc3), NA, pnc3h)
  ## Frequency of doctor PNC check
  pnc4a <- suppressWarnings(as.integer(df$pnc_doc_freq))
  pnc4a <- bbw::recode(var = pnc4a, recodes = "444=NA;999=NA")
  ## Frequency of nurse PNC check
  pnc4b <- suppressWarnings(as.integer(df$pnc_nurs_freq))
  pnc4b <- bbw::recode(var = pnc4b, recodes = "444=NA;999=NA")
  ## Frequency of lhv PNC check
  pnc4c <- suppressWarnings(as.integer(df$pnc_lhv_freq))
  pnc4c <- bbw::recode(var = pnc4c, recodes = "444=NA;999=NA")
  ## Frequency of midwife PNC check
  pnc4d <- suppressWarnings(as.integer(df$pnc_mw_freq))
  pnc4d <- bbw::recode(var = pnc4d, recodes = "444=NA;999=NA")
  ## Frequency of auxilliary midwife PNC check
  pnc4e <- suppressWarnings(as.integer(df$pnc_amw_freq))
  pnc4e <- bbw::recode(var = pnc4d, recodes = "444=NA;999=NA")
  ## Frequency of traditional birth attendant PNC check
  pnc4f <- suppressWarnings(as.integer(df$pnc_tba_freq))
  pnc4f <- bbw::recode(var = pnc4f, recodes = "444=NA;999=NA")
  ## Frequency of relative PNC check
  pnc4g <- suppressWarnings(as.integer(df$pnc_relative_freq))
  pnc4g <- bbw::recode(var = pnc4g, recodes = "444=NA;999=NA")
  ## Frequency of eho PNC check
  pnc4h <- suppressWarnings(as.integer(df$pnc_eho_freq))
  pnc4h <- bbw::recode(var = pnc4h, recodes = "444=NA;999=NA")
  ## B1 supplementation
  postB1 <- suppressWarnings(as.integer(df$pnc_bone))
  postB1 <- bbw::recode(var = postB1, recodes = "999=0")
  ## months of B1 supplementation
  monthsB1 <- suppressWarnings(as.integer(df$pnc_bone_months))
  monthsB1 <- bbw::recode(var = monthsB1, recodes = "444=NA;999=NA")
  ## weeks of B1 supplementation
  weeksB1 <- suppressWarnings(as.integer(df$pnc_bone_weeks))
  weeksB1 <- bbw::recode(var = weeksB1, recodes = "444=NA;999=NA")
  ## B1 supplementation duration in weeks
  pnc5 <- rowSums(cbind(monthsB1 * 4, weeksB1), na.rm = TRUE)
  ## PNC costs?
  pnc6 <- suppressWarnings(as.integer(df$pnc_cost))
  pnc6 <- bbw::recode(var = pnc6, recodes = "777=NA;999=NA")
  ## cost items
  pnc7 <- suppressWarnings(as.integer(df$pnc_cost_items))
  pnc7 <- bbw::recode(var = pnc7, recodes = "888=NA")
  ## cost items: transportation
  pnc7a <- suppressWarnings(as.integer(df$pnc_cost_items.1))
  pnc7a <- ifelse(is.na(pnc7), NA, pnc7a)
  ## cost items: registration fees
  pnc7b <- suppressWarnings(as.integer(df$pnc_cost_items.2))
  pnc7b <- ifelse(is.na(pnc7), NA, pnc7b)
  ## cost items: medicines
  pnc7c <- suppressWarnings(as.integer(df$pnc_cost_items.3))
  pnc7c <- ifelse(is.na(pnc7), NA, pnc7c)
  ## cost items: laboratory fees
  pnc7d <- suppressWarnings(as.integer(df$pnc_cost_items.4))
  pnc7d <- ifelse(is.na(pnc7), NA, pnc7d)
  ## cost items: provider fees
  pnc7e <- suppressWarnings(as.integer(df$pnc_cost_items.5))
  pnc7e <- ifelse(is.na(pnc7), NA, pnc7e)
  ## cost items: gifts
  pnc7f <- suppressWarnings(as.integer(df$pnc_cost_items.6))
  pnc7f <- ifelse(is.na(pnc7), NA, pnc7f)
  ## Concatenate PNC indicators to a data.frame
  pnc <- data.frame(df[ , core.columns],
                    pnc1, pnc2, pnc2a, pnc3, pnc3a, pnc3b, pnc3c, pnc3d, pnc3e,
                    pnc3f, pnc3g, pnc3h, pnc4a, pnc4b, pnc4c, pnc4d, pnc4e,
                    pnc4f, pnc4g, pnc4h, pnc5, pnc6, pnc7, pnc7a, pnc7b,
                    pnc7c, pnc7d, pnc7e, pnc7f)
  ## Return data.frame
  return(pnc)
}
