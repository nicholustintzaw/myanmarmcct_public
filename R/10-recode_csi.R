################################################################################
#
#' Function to recode consumption-based Coping Strategies Index for the Myanmar
#' MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing consumption-based coping stratgies index data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded consumption-based coping strategies index
#'   indicators
#'
#' @examples
#' recode_csi_consumption(df = hh)
#'
#' @export
#'
#
################################################################################

recode_csi_consumption <- function(df,
                                   core.columns = c("KEY",
                                                    "geo_state",
                                                    "geo_rural",
                                                    "geo_villward",
                                                    "sample_component")) {
  ## b1: less preferred and less expensive food
  b1 <- as.integer(df$conindex_prices)
  b1 <- bbw::recode(var = b1, recodes = "444=0;666=0;999=0;8:100=7")
  ## b2: borrow food
  b2 <- as.integer(df$conindex_borrow)
  b2 <- bbw::recode(var = b2, recodes = "444=0;666=0;999=0;8:100=7")
  ## b3: limit portions
  b3 <- as.integer(df$conindex_sizelimit)
  b3 <- bbw::recode(var = b3, recodes = "444=0;666=0;999=0;8:100=7")
  ## b4: limit adult intake
  b4 <- as.integer(df$conindex_restrictage)
  b4 <- bbw::recode(var = b4, recodes = "444=0;666=0;999=0;8:100=7")
  ## b5: limit number of meals
  b5 <- as.integer(df$conindex_reducefreq)
  b5 <- bbw::recode(var = b5, recodes = "444=0;666=0;999=0;8:100=7")
  ## Apply weights
  b1w <- b1 * 1
  b2w <- b2 * 2
  b3w <- b3 * 1
  b4w <- b4 * 3
  b5w <- b5 * 1
  ## Total weighted
  bTotalWeighted <- b1w + b2w + b3w + b4w + b5w
  ## Concatenate indicators to data.frame
  ccsi <- data.frame(df[ , core.columns],
                     b1, b2, b3, b4, b5,
                     b1w, b2w, b3w, b4w, b5w,
                     bTotalWeighted)
  ## Return data.frame
  return(ccsi)
}


################################################################################
#
#' Function to recode livelihoods-based Coping Strategies Index for the Myanmar
#' MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing livelihoods-based coping stratgies index data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded livelihoods-based coping strategies index
#'   indicators
#'
#' @examples
#' recode_csi_livelihoods(df = hh)
#'
#' @export
#'
#
################################################################################

recode_csi_livelihoods <- function(df,
                                   core.columns = c("KEY",
                                                    "geo_state",
                                                    "geo_rural",
                                                    "geo_villward",
                                                    "sample_component")) {
  ## l1: sold household assets (STRESS)
  l1 <- as.integer(df$liveindex_soldhh)
  l1 <- bbw::recode(var = l1, recodes = "1:100=1;444=0;666=0;999=0")
  ## l2: sold more animals than usual (STRESS)
  l2 <- as.integer(df$liveindex_soldanimal)
  l2 <- bbw::recode(var = l2, recodes = "1:100=1;444=0;666=0;999=0")
  ## l3: sent household members to eat elsewhere (STRESS)
  l3 <- as.integer(df$liveindex_senthhmem)
  l3 <- bbw::recode(var = l3, recodes = "1:100=1;444=0;666=0;999=0")
  ## l4: food on credit or borrowed food (STRESS)
  l4 <- as.integer(df$liveindex_creditfood)
  l4 <- bbw::recode(var = l4, recodes = "1:100=1;444=0;666=0;999=0")
  ## l5: sold productive assets or means of transport (CRISIS)
  l5 <- as.integer(df$liveindex_soldassets)
  l5 <- bbw::recode(var = l5, recodes = "1:100=1;444=0;666=0;999=0")
  ## l6: sold house or land (EMERGENCY)
  l6 <- as.integer(df$liveindex_soldland)
  l6 <- bbw::recode(var = l6, recodes = "1:100=1;444=0;666=0;999=0")
  ## l7: withdrew children from school (CRISIS)
  l7 <- as.integer(df$liveindex_withdschool)
  l7 <- bbw::recode(var = l7, recodes = "1:100=1;444=0;666=0;999=0")
  ## l8: sold last female animals (EMERGENCY)
  l8 <- as.integer(df$liveindex_femaleanimals)
  l8 <- bbw::recode(var = l8, recodes = "1:100=1;444=0;666=0;999=0")
  ## l9: begged (EMERGENCY)
  l9 <- as.integer(df$liveindex_begged)
  l9 <- bbw::recode(var = l9, recodes = "1:100=1;444=0;666=0;999=0")
  ## l10: Entire household migrated (EMERGENCY)
  l10 <- as.integer(df$liveindex_hhmigrate)
  l10 <- bbw::recode(var = l10, recodes = "1:100=1;444=0;666=0;999=0")
  ## l11: Spent savings (STRESS)
  l11 <- as.integer(df$liveindex_spentsave)
  l11 <- bbw::recode(var = l11, recodes = "1:100=1;444=0;666=0;999=0")
  ## l12: Borrow money (STRESS)
  l12 <- as.integer(df$liveindex_borrow)
  l12 <- bbw::recode(var = l12, recodes = "1:100=1;444=0;666=0;999=0")
  ## l13: move children to less expensive school (STRESS)
  l13 <- as.integer(df$liveindex_moveschool)
  l13 <- bbw::recode(var = l13, recodes = "1:100=1;444=0;666=0;999=0")
  ## l14: Reduced expenses on health (CRISIS)
  l14 <- as.integer(df$liveindex_reducehealth)
  l14 <- bbw::recode(var = l14, recodes = "1:100=1;444=0;666=0;999=0")
  ## l15: Harvested immature crops (CRISIS)
  l15 <- as.integer(df$liveindex_harvest)
  l15 <- bbw::recode(var = l15, recodes = "1:100=1;444=0;666=0;999=0")
  ## l16: consumed seed stocks that were to be saved for next season (CRISIS)
  l16 <- as.integer(df$liveindex_conseed)
  l16 <- bbw::recode(var = l16, recodes = "1:100=1;444=0;666=0;999=0")
  ## l17: Decrease expenditure on fertilizers, etc. (CRISIS)
  l17 <- as.integer(df$liveindex_farmexp)
  l17 <- bbw::recode(var = l17, recodes = "1:100=1;444=0;666=0;999=0")
  ## l18: illegal activities (EMERGENCY)
  l18 <- as.integer(df$liveindex_illegal)
  l18 <- bbw::recode(var = l18, recodes = "1:100=1;444=0;666=0;999=0")
  ## l19: new job (?)
  l19 <- as.integer(df$liveindex_newjob)
  l19 <- bbw::recode(var = l19, recodes = "1:100=1;444=0;666=0;999=0")
  ## l20: advance salary (?)
  l20 <- as.integer(df$liveindex_advance)
  l20 <- bbw::recode(var = l20, recodes = "1:100=1;444=0;666=0;999=0")
  ## Count stress behavious
  countStress <- rowSums(cbind(l1, l2, l11, l12), na.rm = TRUE)
  ## Count crisis behaviours
  countCrisis <- rowSums(cbind(l5, l7, l14), na.rm = TRUE)
  ## Count emergency behaviours
  countEmergency <- rowSums(cbind(l6, l8, l9), na.rm = TRUE)
  ## Classify food secure household
  secure <- ifelse(countStress == 0 & countCrisis == 0 & countEmergency == 0, 1, 0)
  ## Classify households in stress based on most severe strategy
  stress <- ifelse(countStress > 0 & countCrisis == 0 & countEmergency == 0, 1, 0)
  ## Classify households in crisis based on most severe strategy
  crisis <- ifelse(countCrisis > 0 & countEmergency == 0, 1, 0)
  ## Classify households in emergency based on most severe strategy
  emergency <- ifelse(countEmergency > 0, 1, 0)
  ## Concatenate indicators to data.frame
  lcsi <- data.frame(df[ , core.columns],
                     l1, l2, l5, l6, l7, l8, l9, l11, l12, l14,
                     countStress, countCrisis, countEmergency,
                     secure, stress, crisis, emergency)
  ## Return data.frame
  return(lcsi)
}
