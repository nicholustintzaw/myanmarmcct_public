################################################################################
#
#' Function to recode Food Consumption Score for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing food consumption score data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing identifying information that will be linked to the
#'   food consumption score data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded food consumption score indicators
#'
#' @examples
#' recode_fcs(df = hh)
#'
#' @export
#'
#
################################################################################

recode_fcs <- function(df,
                       x = hhMembers,
                       core.columns = c("KEY",
                                        "geo_state",
                                        "geo_rural",
                                        "geo_villward",
                                        "sample_component",
                                        "hh_mem_sex")) {
  ## Get sex of household head
  hhHead <- x[x$hh_mem_relation == "1", c("PARENT_KEY", "hh_mem_sex")]
  hhHead$hh_mem_sex <- bbw::recode(var = hhHead$hh_mem_sex,
                                   recodes = "1=1;0=2;NA=NA")
  ##
  df <- merge(df, hhHead, by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
  ## Food groups - starch
  fg1 <- rowSums(cbind(as.integer(df$hhrice_freq),
                       as.integer(df$hhpotatoes_freq),
                       as.integer(df$hhpumpkin_freq)),
                 na.rm = TRUE)
  fg1 <- ifelse(fg1 > 7, 7, fg1)
  ## Food groups - pulses
  fg2 <- rowSums(cbind(as.integer(df$hhbeans_freq),
                       as.integer(df$hhnuts_freq)),
                 na.rm = TRUE)
  fg2 <- ifelse(fg2 > 7, 7, fg2)
  ## Food groups - vegetables
  fg3 <- rowSums(cbind(as.integer(df$hhleafyveg_freq),
                       as.integer(df$hhvitveg_freq),
                       as.integer(df$hhveg_freq)),
                 na.rm = TRUE)
  fg3 <- ifelse(fg3 > 7, 7, fg3)
  ## Food groups - fruits
  fg4 <- rowSums(cbind(as.integer(df$hhmango_freq),
                       as.integer(df$hhfruit_freq)),
                 na.rm = TRUE)
  fg4 <- ifelse(fg4 > 7, 7, fg4)
  ## food groups - meat
  fg5 <- rowSums(cbind(as.integer(df$hhorgan_freq),
                       as.integer(df$hhbeef_freq),
                       as.integer(df$hhfish_freq),
                       as.integer(df$hheggs_freq)),
                 na.rm = TRUE)
  fg5 <- ifelse(fg5 > 7, 7, fg5)
  ## food groups - dairy
  fg6 <- as.integer(df$hhyogurt_freq)
  fg6 <- ifelse(is.na(fg6), 0, fg6)
  fg6 <- ifelse(fg6 > 7, 7, fg6)
  ## food groups - fats
  fg7 <- as.integer(df$hhfat_freq)
  fg7 <- ifelse(is.na(fg7), 0, fg7)
  fg7 <- ifelse(fg7 > 7, 7, fg7)
  ## food groups - sugar
  fg8 <- as.integer(df$hhsweets_freq)
  fg8 <- ifelse(is.na(fg8), 0, fg8)
  fg8 <- ifelse(fg8 > 7, 7, fg8)
  ## FCS
  fcs <- (fg1 * 2) + (fg2 * 3) + (fg3 * 1) + (fg4 * 1) + (fg5 * 4) + (fg6 * 4) + (fg7 * 0.5) + (fg8 * 0.5)
  ##
  poor <- ifelse(fcs <= 21, 1, 0)
  ##
  borderline <- ifelse(fcs > 21 & fcs <= 35, 1, 0)
  ##
  acceptable <- ifelse(fcs > 35, 1, 0)
  ##
  fcs <- data.frame(df[ , core.columns],
                    fg1, fg2, fg3, fg4, fg5, fg6, fg7, fg8,
                    fcs, poor, borderline, acceptable)
  ##
  names(fcs)[6] <- "sex"
  ##
  return(fcs)
}

################################################################################
#
#' Function to recode Food Consumption Score for Nutirtion for the
#' Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing food consumption score data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing identifying information that will be linked to the
#'   food consumption score data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded food consumption score for nutrition
#'   indicators
#'
#' @examples
#' recode_fcs_nutrition(df = hh)
#'
#' @export
#'
#
################################################################################

recode_fcs_nutrition <- function(df,
                                 x = hhMembers,
                                 core.columns = c("KEY",
                                                  "geo_state",
                                                  "geo_rural",
                                                  "geo_villward",
                                                  "sample_component",
                                                  "hh_mem_sex")) {
  ## Get sex of household head
  hhHead <- x[x$hh_mem_relation == "1", c("PARENT_KEY", "hh_mem_sex")]
  hhHead$hh_mem_sex <- bbw::recode(var = hhHead$hh_mem_sex,
                                   recodes = "1=1;0=2;NA=NA")
  ##
  df <- merge(df, hhHead, by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
  ##
  vita <- rowSums(cbind(as.integer(df$hhyogurt_freq),
                        as.integer(df$hhorgan_freq),
                        as.integer(df$hheggs_freq),
                        as.integer(df$hhpumpkin_freq),
                        as.integer(df$hhleafyveg_freq),
                        as.integer(df$hhvitveg_freq),
                        as.integer(df$hhmango_freq)),
                  na.rm = TRUE)
  vita <- ifelse(vita > 7, 7, vita)
  ## Vitamin A - never
  vita1 <- bbw::recode(var = vita, recodes = "0=1;NA=NA;else=0")
  ## Vitamin A - sometimes
  vita2 <- bbw::recode(var = vita, recodes = "1:6=1;NA=NA;else=0")
  ## Vitamin A - at least daily
  vita3 <- bbw::recode(var = vita, recodes = "7=1;NA=NA;else=0")
  ##
  protein <- rowSums(cbind(as.integer(df$hhbeans_freq),
                           as.integer(df$hhnuts_freq),
                           as.integer(df$hhyogurt_freq),
                           as.integer(df$hhbeef_freq),
                           as.integer(df$hhfish_freq),
                           as.integer(df$hhorgan_freq),
                           as.integer(df$hheggs_freq)),
                     na.rm = TRUE)
  protein <- ifelse(protein > 7, 7, protein)
  ## Protein - never
  protein1 <- bbw::recode(var = protein, recodes = "0=1;NA=NA;else=0")
  ## Protein - sometimes
  protein2 <- bbw::recode(var = protein, recodes = "1:6=1;NA=NA;else=0")
  ## Protein - at least daily
  protein3 <- bbw::recode(var = protein, recodes = "7=1;NA=NA;else=0")
  ## iron-rich foods
  iron <- rowSums(cbind(as.integer(df$hhbeef_freq),
                        as.integer(df$hhfish_freq),
                        as.integer(df$hhorgan_freq)),
                  na.rm = TRUE)
  iron <- ifelse(iron > 7, 7, iron)
  ## Iron - never
  iron1 <- bbw::recode(var = iron, recodes = "0=1;NA=NA;else=0")
  ## Iron - sometimes
  iron2 <- bbw::recode(var = iron, recodes = "1:6=1;NA=NA;else=0")
  ## Iron - at least daily
  iron3 <- bbw::recode(var = iron, recodes = "7=1;NA=NA;else=0")
  ## Concatenate FCS-nutrition indicatotrs
  fcsn <- data.frame(df[ , core.columns],
                     vita, vita1, vita2, vita3,
                     protein, protein1, protein2, protein3,
                     iron, iron1, iron2, iron3)
  ##
  names(fcsn)[6] <- "sex"
  ## Return data.frame
  return(fcsn)
}






