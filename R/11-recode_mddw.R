################################################################################
#
#' Function to recode minimum dietary diversity for women for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing minimum dietary diversity for women data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded minimum dietary diversity for women indicators
#'
#' @examples
#' recode_mddw(df = hh)
#'
#' @export
#'
#
################################################################################

recode_mddw <- function(df,
                        core.columns = c("KEY",
                                         "geo_state",
                                         "geo_rural",
                                         "geo_villward",
                                         "sample_component")) {
  ## fg1: grains, white roots, tubers and plantain
  wfg1 <- sum(as.integer(df$mom_rice), as.integer(df$mom_potatoes), na.rm = TRUE)
  wfg1 <- bbw::recode(var = wfg1, recodes = "1:hi=1;NA=NA;else=0")
  ## fg2: pulses
  wfg2 <- as.integer(df$mom_beans)
  ## fg3: nuts and seeds
  wfg3 <- as.integer(df$mom_nuts)
  ## fg4: dairy
  wfg4 <- as.integer(df$mom_yogurt)
  ## fg5: meat, poultry, fish
  wfg5 <- sum(as.integer(df$mom_beef), as.integer(df$mom_fish),
              as.integer(df$mom_organ), na.rm = TRUE)
  wfg5 <- bbw::recode(var = wfg5, recodes = "1:hi=1;NA=NA;else=0")
  ## fg6: eggs
  wfg6 <- as.integer(df$mom_eggs)
  ## fg7: dark green leafy vegetables
  wfg7 <- as.integer(df$mom_leafyveg)
  ## fg8: other vitamin A-rich fruits and vegetables
  wfg8 <- sum(as.integer(df$mom_pumpkin), as.integer(df$mom_mango), na.rm = TRUE)
  wfg8 <- bbw::recode(var = wfg8, recodes = "1:hi=1;NA=NA;else=0")
  ## fg9: other vegetables
  wfg9 <- sum(as.integer(df$mom_veg), as.integer(df$mom_fruit), na.rm = TRUE)
  wfg9 <- bbw::recode(var = wfg9, recodes = "1:hi=1;NA=NA;else=0")
  ## fg10: other fruits
  wfg10 <- as.integer(df$mom_fruit)
  ## Total number of food groups eaten
  wfg <- wfg1 + wfg2 + wfg3 + wfg4 + wfg5 + wfg6 + wfg7 + wfg8 + wfg9 + wfg10
  ## MDD-W indicator
  mddw <- bbw::recode(var = wfg, recodes = "5:hi=1;else=0")
  ## Concatenate MDDW indicators
  mddw <- data.frame(df[ , core.columns], wfg1, wfg2, wfg3, wfg4, wfg5,
                     wfg6, wfg7, wfg8, wfg9, wfg10, wfg, mddw)
  ## Return data.frame
  return(mddw)
}
