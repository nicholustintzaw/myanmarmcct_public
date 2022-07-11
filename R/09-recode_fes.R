################################################################################
#
#' Function to recode household food expenditure share for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household food expenditure data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded household food expenditure share indicators
#'
#' @examples
#' recode_hfes(df = hh)
#'
#' @export
#'
#
################################################################################

recode_hfes <- function(df,
                        core.columns = c("KEY",
                                         "geo_state",
                                         "geo_rural",
                                         "geo_villward",
                                         "sample_component")) {
  ## non-food expenditure
  x <- df[ , c("exp_tution_cash", "exp_eduoth_cash",
               "exp_healthadult_cash", "exp_healthchild_cash",
               "exp_healthu5_cash", "exp_transpt_cash",
               "exp_remitt_cash", "exp_construct_cash",
               "exp_trade_cash", "exp_farm_cash", "exp_livestock_cash",
               "exp_fish_cash", "exp_social_cash", "exp_othreg_cash",
               "exp_debt_cash", "exp_donate_cash")]
  x <- apply(X = x, MARGIN = 2, FUN = as.integer)
  nonfood <- rowSums(x = x, na.rm = TRUE)

  ## food expenditure
  y <- df[ , c("hhrice_cost", "hhpotatoes_cost", "hhpumpkin_cost",
               "hhbeans_cost", "hhnuts_cost", "hhyogurt_cost", "hhorgan_cost",
               "hhbeef_cost", "hhfish_cost", "hheggs_cost", "hhleafyveg_cost",
               "hhvitveg_cost", "hhmango_cost", "hhveg_cost", "hhfruit_cost",
               "hhfat_cost", "hhsweets_cost", "hhcondi_cost")]
  y <- apply(X = y, MARGIN = 2, FUN = as.integer)
  food <- rowSums(x = y, na.rm = TRUE)

  ## calculate HFES
  hfes <- food / (food + nonfood)

  ## Classify households
  vulnerable <- ifelse(hfes > 0.75, 1, 0)
  high <- ifelse(hfes <= 0.75 & hfes > 0.65, 1, 0)
  medium <- ifelse(hfes <= 0.65 & hfes >= 0.5, 1, 0)
  low <- ifelse(hfes < 0.5, 1, 0)

  ## Concatenate HFES indicators data.frame
  hfes <- data.frame(df[ , core.columns], nonfood, food, hfes, vulnerable, high, medium, low)

  ## Return data.frame
  return(hfes)
}
