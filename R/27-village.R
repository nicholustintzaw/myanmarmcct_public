################################################################################
#
#' Function to recode village data
#'
#' @param df A data.frame containing village data for the MCCT baseline study
#'
#' @return A data.frame containing recoded variables for village information
#'
#' @examples
#' recode_village(df = villageData1)
#'
#' @export
#'
#
################################################################################

recode_village <- function(df) {
  ## Recode population
  df$hh_tot <- bbw::recode(var = df$hh_tot, recodes = "999=NA;800=NA;444=NA")
  df$pop_tot <- bbw::recode(var = df$pop_tot, recodes = "999=NA;800=NA;444=NA")
  df$pop_male <- bbw::recode(var = df$pop_male, recodes = "999=NA;800=NA;444=NA")
  df$pop_female <- bbw::recode(var = df$pop_female, recodes = "999=NA;800=NA;444=NA")
  df$pop_u2 <- bbw::recode(var = df$pop_u2, recodes = "999=NA;800=NA;444=NA")
  df$pop_u5 <- bbw::recode(var = df$pop_u5, recodes = "999=NA;800=NA;444=NA")
  df$pop_5_10 <- bbw::recode(var = df$pop_5_10, recodes = "999=NA;800=NA;444=NA")
  df$pop_11_60 <- bbw::recode(var = df$pop_11_60, recodes = "999=NA;800=NA;444=NA")
  df$pop_over60 <- bbw::recode(var = df$pop_over60, recodes = "999=NA;800=NA;444=NA")
  df$pop_u2_male <- bbw::recode(var = df$pop_u2_male, recodes = "999=NA")
  df$pop_u2_female <- bbw::recode(var = df$pop_u2_female, recodes = "999=NA;800=NA;444=NA")
  df$pop_u5_male <- bbw::recode(var = df$pop_u5_male, recodes = "999=NA;800=NA;444=NA")
  df$pop_u5_female <- bbw::recode(var = df$pop_u5_female, recodes = "999=NA;800=NA;444=NA")
  df$pop_5_10_male <- bbw::recode(var = df$pop_5_10_male, recodes = "999=NA;800=NA;444=NA")
  df$pop_5_10_female <- bbw::recode(var = df$pop_5_10_female, recodes = "999=NA;800=NA;444=NA")
  df$pop_11_60_male <- bbw::recode(var = df$pop_11_60_male, recodes = "999=NA;800=NA;444=NA")
  df$pop_11_60_female <- bbw::recode(var = df$pop_11_60_female, recodes = "999=NA;800=NA;444=NA")
  df$pop_over60_male <- bbw::recode(var = df$pop_over60_male, recodes = "999=NA;800=NA;444=NA")
  df$pop_over60_female <- bbw::recode(var = df$pop_over60_female, recodes = "999=NA;800=NA;444=NA")
  ## Recode education
  df$edu_young <- as.numeric(df$edu_young)
  df$edu_young <- bbw::recode(var = df$edu_young, recodes = "999=NA;800=NA;444=NA")
  df$edu_aduld <- as.numeric(df$edu_aduld)
  df$edu_aduld <- bbw::recode(var = df$edu_aduld, recodes = "999=NA;800=NA;444=NA")
  df$edu_old <- as.numeric(df$edu_old)
  df$edu_old <- bbw::recode(var = df$edu_old, recodes = "999=NA;800=NA;444=NA")
  df$edu_tot <- as.numeric(df$edu_tot)
  df$edu_tot <- bbw::recode(var = df$edu_tot, recodes = "999=NA;800=NA;444=NA")
  ## Recode occupation
  df$occup_type.1 <- as.numeric(df$occup_type.1)
  df$occup_type.2 <- as.numeric(df$occup_type.2)
  df$occup_type.3 <- as.numeric(df$occup_type.3)
  df$occup_type.4 <- as.numeric(df$occup_type.4)
  df$occup_type.5 <- as.numeric(df$occup_type.5)
  df$occup_type.6 <- as.numeric(df$occup_type.6)
  df$occup_type.7 <- as.numeric(df$occup_type.7)
  ## Recode wages - agriculture
  df$agri_male_wage <- bbw::recode(var = df$agri_male_wage, recodes = "999=NA")
  ## Recode wages - fishery
  df$fish_male_wage <- bbw::recode(var = df$fish_male_wage, recodes = "999=NA")
  ## Recode wages - livestock
  df$lstock_male_wage <- bbw::recode(var = df$lstock_male_wage, recodes = "999=NA")
  ## Recode wages - microenterprise
  df$micro_male_wage <- bbw::recode(var = df$micro_male_wage, recodes = "999=NA")
  ## Recode wages - casual
  df$casual_male_wage <- bbw::recode(var = df$casual_male_wage, recodes = "999=NA")
  ## Recode wages - government job
  df$govjob_male_wage <- bbw::recode(var = df$govjob_male_wage, recodes = "999=NA")
  ## Recode wages - private
  df$private_male_wage <- bbw::recode(var = df$private_male_wage, recodes = "999=NA")
  ## Agroecological zone
  df$agroeco_dry <- as.numeric(df$agroeco_dry)
  df$agroeco_highland <- as.numeric(df$agroeco_highland)
  df$agroeco_irigated <- as.numeric(df$agroeco_irrgated)
  ## Access to town
  df$acc_town_dist <- as.numeric(df$acc_town_dist)
  df$acc_town_dist <- bbw::recode(var = df$acc_town_dist, recodes = "999=NA")
  df$acc_town_trans_dry <- as.numeric(df$acc_town_trans_dry)
  df$acc_town_trans_dry <- bbw::recode(var = df$acc_town_trans_dry, recodes = "999=NA;888=NA")
  df$acc_town_time_dry <- as.numeric(df$acc_town_time_dry)
  df$acc_town_time_dry <- bbw::recode(var = df$acc_town_time_dry, recodes = "999=NA")
  df$acc_town_cost_dry <- as.numeric(df$acc_town_cost_dry)
  df$acc_town_cost_dry <- bbw::recode(var = df$acc_town_cost_dry, recodes = "999=NA")
  df$acc_town_trans_wet <- as.numeric(df$acc_town_trans_wet)
  df$acc_town_trans_wet <- bbw::recode(var = df$acc_town_trans_wet, recodes = "999=NA;888=NA")
  df$acc_town_time_wet <- as.numeric(df$acc_town_time_wet)
  df$acc_town_time_wet <- bbw::recode(var = df$acc_town_time_wet, recodes = "999=NA")
  df$acc_town_cost_wet <- as.numeric(df$acc_town_cost_wet)
  df$acc_town_cost_wet <- bbw::recode(var = df$acc_town_cost_wet, recodes = "999=NA")
  ## Access to SRHC
  df$acc_srhc_dist <- as.numeric(df$acc_srhc_dist)
  df$acc_srhc_dist <- bbw::recode(var = df$acc_srhc_dist, recodes = "999=NA")
  df$acc_srhc_trans_dry <- as.numeric(df$acc_srhc_trans_dry)
  df$acc_srhc_trans_dry <- bbw::recode(var = df$acc_srhc_trans_dry, recodes = "999=NA;888=NA")
  df$acc_srhc_time_dry <- as.numeric(df$acc_srhc_time_dry)
  df$acc_srhc_time_dry <- bbw::recode(var = df$acc_srhc_time_dry, recodes = "999=NA")
  df$acc_srhc_cost_dry <- as.numeric(df$acc_srhc_cost_dry)
  df$acc_srhc_cost_dry <- bbw::recode(var = df$acc_srhc_cost_dry, recodes = "999=NA")
  df$acc_srhc_trans_wet <- as.numeric(df$acc_srhc_trans_wet)
  df$acc_srhc_trans_wet <- bbw::recode(var = df$acc_srhc_trans_wet, recodes = "999=NA;888=NA")
  df$acc_srhc_time_wet <- as.numeric(df$acc_srhc_time_wet)
  df$acc_srhc_time_wet <- bbw::recode(var = df$acc_srhc_time_wet, recodes = "999=NA")
  df$acc_srhc_cost_wet <- as.numeric(df$acc_srhc_cost_wet)
  df$acc_srhc_cost_wet <- bbw::recode(var = df$acc_srhc_cost_wet, recodes = "999=NA")
  ## Access to State Hospital
  df$acc_sthosp_dist <- as.numeric(df$acc_sthosp_dist)
  df$acc_sthosp_dist <- bbw::recode(var = df$acc_sthosp_dist, recodes = "999=NA")
  df$acc_sthosp_trans_dry <- as.numeric(df$acc_sthosp_trans_dry)
  df$acc_sthosp_trans_dry <- bbw::recode(var = df$acc_sthosp_trans_dry, recodes = "999=NA;888=NA")
  df$acc_sthosp_time_dry <- as.numeric(df$acc_sthosp_time_dry)
  df$acc_sthosp_time_dry <- bbw::recode(var = df$acc_sthosp_time_dry, recodes = "999=NA")
  df$acc_sthosp_cost_dry <- as.numeric(df$acc_sthosp_cost_dry)
  df$acc_sthosp_cost_dry <- bbw::recode(var = df$acc_sthosp_cost_dry, recodes = "999=NA")
  df$acc_sthosp_trans_wet <- as.numeric(df$acc_sthosp_trans_wet)
  df$acc_sthosp_trans_wet <- bbw::recode(var = df$acc_sthosp_trans_wet, recodes = "999=NA;888=NA")
  df$acc_sthosp_time_wet <- as.numeric(df$acc_sthosp_time_wet)
  df$acc_sthosp_time_wet <- bbw::recode(var = df$acc_sthosp_time_wet, recodes = "999=NA")
  df$acc_sthosp_cost_wet <- as.numeric(df$acc_sthosp_cost_wet)
  df$acc_sthosp_cost_wet <- bbw::recode(var = df$acc_sthosp_cost_wet, recodes = "999=NA")
  ## Access to Primary School
  df$acc_pschool_dist <- as.numeric(df$acc_pschool_dist)
  df$acc_pschool_dist <- bbw::recode(var = df$acc_pschool_dist, recodes = "999=NA")
  df$acc_pschool_trans_dry <- as.numeric(df$acc_pschool_trans_dry)
  df$acc_pschool_trans_dry <- bbw::recode(var = df$acc_pschool_trans_dry, recodes = "999=NA;888=NA")
  df$acc_pschool_time_dry <- as.numeric(df$acc_pschool_time_dry)
  df$acc_pschool_time_dry <- bbw::recode(var = df$acc_pschool_time_dry, recodes = "999=NA")
  df$acc_pschool_cost_dry <- as.numeric(df$acc_pschool_cost_dry)
  df$acc_pschool_cost_dry <- bbw::recode(var = df$acc_pschool_cost_dry, recodes = "999=NA")
  df$acc_pschool_trans_wet <- as.numeric(df$acc_pschool_trans_wet)
  df$acc_pschool_trans_wet <- bbw::recode(var = df$acc_pschool_trans_wet, recodes = "999=NA;888=NA")
  df$acc_pschool_time_wet <- as.numeric(df$acc_pschool_time_wet)
  df$acc_pschool_time_wet <- bbw::recode(var = df$acc_pschool_time_wet, recodes = "999=NA")
  df$acc_pschool_cost_wet <- as.numeric(df$acc_pschool_cost_wet)
  df$acc_pschool_cost_wet <- bbw::recode(var = df$acc_pschool_cost_wet, recodes = "999=NA")
  ## Access to Middle School
  df$acc_mschool_dist <- as.numeric(df$acc_mschool_dist)
  df$acc_mschool_dist <- bbw::recode(var = df$acc_mschool_dist, recodes = "999=NA")
  df$acc_mschool_trans_dry <- as.numeric(df$acc_mschool_trans_dry)
  df$acc_mschool_trans_dry <- bbw::recode(var = df$acc_mschool_trans_dry, recodes = "999=NA;888=NA")
  df$acc_mschool_time_dry <- as.numeric(df$acc_mschool_time_dry)
  df$acc_mschool_time_dry <- bbw::recode(var = df$acc_mschool_time_dry, recodes = "999=NA")
  df$acc_mschool_cost_dry <- as.numeric(df$acc_mschool_cost_dry)
  df$acc_mschool_cost_dry <- bbw::recode(var = df$acc_mschool_cost_dry, recodes = "999=NA")
  df$acc_mschool_trans_wet <- as.numeric(df$acc_mschool_trans_wet)
  df$acc_mschool_trans_wet <- bbw::recode(var = df$acc_mschool_trans_wet, recodes = "999=NA;888=NA")
  df$acc_mschool_time_wet <- as.numeric(df$acc_mschool_time_wet)
  df$acc_mschool_time_wet <- bbw::recode(var = df$acc_mschool_time_wet, recodes = "999=NA")
  df$acc_mschool_cost_wet <- as.numeric(df$acc_mschool_cost_wet)
  df$acc_mschool_cost_wet <- bbw::recode(var = df$acc_mschool_cost_wet, recodes = "999=NA")
  ## Access to High School
  df$acc_hschool_dist <- as.numeric(df$acc_hschool_dist)
  df$acc_hschool_dist <- bbw::recode(var = df$acc_hschool_dist, recodes = "999=NA")
  df$acc_hschool_trans_dry <- as.numeric(df$acc_hschool_trans_dry)
  df$acc_hschool_trans_dry <- bbw::recode(var = df$acc_hschool_trans_dry, recodes = "999=NA;888=NA")
  df$acc_hschool_time_dry <- as.numeric(df$acc_hschool_time_dry)
  df$acc_hschool_time_dry <- bbw::recode(var = df$acc_hschool_time_dry, recodes = "999=NA")
  df$acc_hschool_cost_dry <- as.numeric(df$acc_hschool_cost_dry)
  df$acc_hschool_cost_dry <- bbw::recode(var = df$acc_hschool_cost_dry, recodes = "999=NA")
  df$acc_hschool_trans_wet <- as.numeric(df$acc_hschool_trans_wet)
  df$acc_hschool_trans_wet <- bbw::recode(var = df$acc_hschool_trans_wet, recodes = "999=NA;888=NA")
  df$acc_hschool_time_wet <- as.numeric(df$acc_hschool_time_wet)
  df$acc_hschool_time_wet <- bbw::recode(var = df$acc_hschool_time_wet, recodes = "999=NA")
  df$acc_hschool_cost_wet <- as.numeric(df$acc_hschool_cost_wet)
  df$acc_hschool_cost_wet <- bbw::recode(var = df$acc_hschool_cost_wet, recodes = "999=NA")
  ## Access to bank
  df$acc_bank_dist <- as.numeric(df$acc_bank_dist)
  df$acc_bank_dist <- bbw::recode(var = df$acc_bank_dist, recodes = "999=NA")
  df$acc_bank_trans_dry <- as.numeric(df$acc_bank_trans_dry)
  df$acc_bank_trans_dry <- bbw::recode(var = df$acc_bank_trans_dry, recodes = "999=NA;888=NA")
  df$acc_bank_time_dry <- as.numeric(df$acc_bank_time_dry)
  df$acc_bank_time_dry <- bbw::recode(var = df$acc_bank_time_dry, recodes = "999=NA")
  df$acc_bank_cost_dry <- as.numeric(df$acc_bank_cost_dry)
  df$acc_bank_cost_dry <- bbw::recode(var = df$acc_bank_cost_dry, recodes = "999=NA")
  df$acc_bank_trans_wet <- as.numeric(df$acc_bank_trans_wet)
  df$acc_bank_trans_wet <- bbw::recode(var = df$acc_bank_trans_wet, recodes = "999=NA;888=NA")
  df$acc_bank_time_wet <- as.numeric(df$acc_bank_time_wet)
  df$acc_bank_time_wet <- bbw::recode(var = df$acc_bank_time_wet, recodes = "999=NA")
  df$acc_bank_cost_wet <- as.numeric(df$acc_bank_cost_wet)
  df$acc_bank_cost_wet <- bbw::recode(var = df$acc_bank_cost_wet, recodes = "999=NA")
  ## Available health post
  df$health_post_vill.1 <- as.numeric(df$health_post_vill.1)
  df$health_post_vill.2 <- as.numeric(df$health_post_vill.2)
  df$health_post_vill.3 <- as.numeric(df$health_post_vill.3)
  df$health_post_vill.4 <- as.numeric(df$health_post_vill.4)
  df$health_post_vill.5 <- as.numeric(df$health_post_vill.5)
  df$health_post_vill.6 <- as.numeric(df$health_post_vill.6)
  df$health_post_vill.7 <- as.numeric(df$health_post_vill.7)
  df$health_post_vill.8 <- as.numeric(df$health_post_vill.8)
  ## Health human resources
  df$health_hr_mw <- as.numeric(df$health_hr_mw)
  df$health_hr_mw <- bbw::recode(var = df$health_hr_mw, recodes = "999=NA")
  df$health_hr_mw_num <- as.numeric(df$health_hr_mw_num)
  df$health_hr_mw_num <- bbw::recode(var = df$health_hr_mw_num, recodes = "999=NA")
  df$health_hr_amw <- as.numeric(df$health_hr_amw)
  df$health_hr_amw <- bbw::recode(var = df$health_hr_amw, recodes = "999=NA")
  df$health_hr_amw_num <- as.numeric(df$health_hr_amw_num)
  df$health_hr_amw_num <- bbw::recode(var = df$health_hr_amw_num, recodes = "999=NA")
  df$health_hr_chw <- as.numeric(df$health_hr_chw)
  df$health_hr_chw <- bbw::recode(var = df$health_hr_chw, recodes = "999=NA")
  df$health_hr_chw_num <- as.numeric(df$health_hr_chw_num)
  df$health_hr_chw_num <- bbw::recode(var = df$health_hr_chw_num, recodes = "999=NA")
  df$health_hr_ttba <- as.numeric(df$health_hr_ttba)
  df$health_hr_ttba <- bbw::recode(var = df$health_hr_ttba, recodes = "999=NA")
  df$health_hr_ttba_num <- as.numeric(df$health_hr_ttba_num)
  df$health_hr_ttba_num <- bbw::recode(var = df$health_hr_ttba_num, recodes = "999=NA")
  ## Electricity
  df$electric_gov <- as.numeric(df$electric_gov)
  df$electric_gov <- bbw::recode(var = df$electric_gov, recodes = "999=NA")
  df$electrichh_gov_num <- as.numeric(df$electrichh_gov_num)
  df$electrichh_gov_num <- bbw::recode(var = df$electrichh_gov_num, recodes = "999=NA")
  df$electric_vill <- as.numeric(df$electric_vill)
  df$electric_vill <- bbw::recode(var = df$electric_vill, recodes = "999=NA")
  df$electrichh_vill_num <- as.numeric(df$electrichh_vill_num)
  df$electrichh_vill_num <- bbw::recode(var = df$electrichh_vill_num, recodes = "999=NA")
  df$electric_private <- as.numeric(df$electric_private)
  df$electric_private <- bbw::recode(var = df$electric_private, recodes = "999=NA")
  df$electrichh_private_num <- as.numeric(df$electrichh_private_num)
  df$electrichh_private_num <- bbw::recode(var = df$electrichh_private_num, recodes = "999=NA")
  df$electric_solar <- as.numeric(df$electric_solar)
  df$electric_solar <- bbw::recode(var = df$electric_solar, recodes = "999=NA")
  df$electric_solar_num <- as.numeric(df$electric_solar_num)
  df$electric_solar_num <- bbw::recode(var = df$electric_solar_num, recodes = "999=NA")
  ## Market
  df$market_large <- as.numeric(df$market_large)
  df$market_small <- as.numeric(df$market_small)
  df$market_homeshop <- as.numeric(df$market_homeshop)
  ## CBO
  df$cbo_yn <- as.numeric(df$cbo_yn)
  df$cbo_yn <- bbw::recode(var = df$cbo_yn, recodes = "999=NA")
  df$num <- as.numeric(df$cbo_num)
  df$cbo_num <- bbw::recode(var = df$cbo_num, recodes = "999=NA")
  return(df)
}

################################################################################
#
#' Function to create a description and summary of village data information
#'
#' @param df A data.frame with recoded village data information for analysis
#' @param state State for which to produce analysis for.
#'
#' @return A list of descriptions and summaries.
#'
#' @examples
#' describe_village(df = recode_village(df = villageData1), state = "Kayah")
#'
#' @export
#'
#
################################################################################

describe_village <- function(df, state = c("Kayah", "Kayin")) {
  x <- subset(df, state_region %in% state)
  desc <- vector(mode = "list", length = length(unique(x$township)))
  names(desc) <- unique(x$township)
  for(i in unique(x$township)) {
    y <- subset(x, township == i)
    ## Population
    z1 <- paste("The surveyed villages in ", i, " Township had an average of ",
                round(mean(y$hh_tot, na.rm = TRUE), digits = 2), " households and an average population of ",
                round(mean(y$pop_tot, na.rm = TRUE), digits = 2), " with an average of ",
                round(mean(y$pop_male, na.rm = TRUE), digits = 2), " males and ",
                round(mean(y$pop_female, na.rm = TRUE), digits = 2), " females. The average population of the surveyed villages by age group is as follows:",
                sep = "")
    z1 <- stringr::str_replace_all(string = z1, pattern = "NA", replacement = "unknown")
    ## Population table
    z2 <- data.frame(as.character(codebookVillage$question[35:49]),
                     round(colMeans(x = y[ , as.character(codebookVillage$variable)[35:49]], na.rm = TRUE), digits = 2))
    row.names(z2) <- 1:nrow(z2)
    names(z2) <- c("Indicator", "Value")
    ## Education
    z3 <- paste("The surveyed villages in ", i, " Township has on average ",
                round(mean(y$edu_young, na.rm = TRUE), digits = 1), "% of its young population, ",
                round(mean(y$edu_aduld, na.rm = TRUE), digits = 1), "% of its adult population, and ",
                round(mean(y$edu_old, na.rm = TRUE), digits = 1), "% of its old population who have had any basic education. Overall average across the villages is ",
                round(mean(y$edu_tot, na.rm = TRUE), digits = 1), "% having had a basic education.",
                sep = "")
    ## Occupation
    z4 <- data.frame("Occupation" = c("Agriculture/Crop Production",
                                      "Inland Fishery",
                                      "Livestock",
                                      "Small Micro-enterprise",
                                      "Casual labor",
                                      "Government job",
                                      "Private job"),
                     "Percentage" = round(colMeans(x = y[ , paste("occup_type.", 1:7, sep = "")], na.rm = TRUE), digits = 2) * 100)
    row.names(z4) <- 1:7
    ## Wages - agriculture
    z5 <- paste("The average seasonal wage for a household with a male member doing agriculture/crop production in ", i, " Township is ",
                round(mean(y$agri_male_wage[y$agri_male_wagecate == 3], na.rm = TRUE), digits = 2),
                " MMK. For male members of households who earn a monthly wage for agriculture/crop production, the average monthly wage is ",
                round(mean(y$agri_male_wage[y$agri_male_wagecate == 2], na.rm = TRUE), digits = 2), " MMK.", sep = "")
    z5 <- stringr::str_replace_all(string = z5, pattern = "NA|NaN", replacement = "unknown")
    ## Wages - fishery
    z6 <- paste("The average seasonal wage for a household with a male member doing inland fishery in ", i, " Township is ",
                round(mean(y$fish_male_wage[y$fish_male_wagecate == 3], na.rm = TRUE), digits = 2),
                " MMK. For male members of households who earn a monthly wage for inland fishery, the average monthly wage is ",
                round(mean(y$fish_male_wage[y$fish_male_wagecate == 2], na.rm = TRUE), digits = 2), " MMK.", sep = "")
    z6 <- stringr::str_replace_all(string = z6, pattern = "NA|NaN", replacement = "unknown")
    ## Wages - livestock
    z7 <- paste("The average seasonal wage for a household with a male member doing livestock in ", i, " Township is ",
                round(mean(y$lstock_male_wage[y$lstock_male_wagecate == 3], na.rm = TRUE), digits = 2),
                " MMK. For male members of households who earn a monthly wage for livestock, the average monthly wage is ",
                round(mean(y$lstock_male_wage[y$lstock_male_wagecate == 2], na.rm = TRUE), digits = 2), " MMK.", sep = "")
    z7 <- stringr::str_replace_all(string = z7, pattern = "NA|NaN", replacement = "unknown")
    ## Wages - microenterprise
    z8 <- paste("The average seasonal wage for a household with a male member doing microenterprise in ", i, " Township is ",
                round(mean(y$micro_male_wage[y$micro_male_wagecate == 3], na.rm = TRUE), digits = 2),
                " MMK. For male members of households who earn a monthly wage for microenterprise, the average monthly wage is ",
                round(mean(y$micro_male_wage[y$micro_male_wagecate == 2], na.rm = TRUE), digits = 2), " MMK.", sep = "")
    z8 <- stringr::str_replace_all(string = z8, pattern = "NA|NaN", replacement = "unknown")
    ## Wages - casual labour
    z9 <- paste("The average seasonal wage for a household with a male member doing casual labour in ", i, " Township is ",
                round(mean(y$casual_male_wage[y$casual_male_wagecate == 3], na.rm = TRUE), digits = 2),
                " MMK. For male members of households who earn a monthly wage for casual labour, the average monthly wage is ",
                round(mean(y$casual_male_wage[y$casual_male_wagecate == 2], na.rm = TRUE), digits = 2), " MMK.", sep = "")
    z9 <- stringr::str_replace_all(string = z9, pattern = "NA|NaN", replacement = "unknown")
    ## Wages - government job
    z10 <- paste("The average seasonal wage for a household with a male member in a government job in ", i, " Township is ",
                 round(mean(y$govjob_male_wage[y$govjob_male_wagecate == 3], na.rm = TRUE), digits = 2),
                 " MMK. For male members of households who earn a monthly wage for a government job, the average monthly wage is ",
                 round(mean(y$govjob_male_wage[y$govjob_male_wagecate == 2], na.rm = TRUE), digits = 2), " MMK.", sep = "")
    z10 <- stringr::str_replace_all(string = z10, pattern = "NA|NaN", replacement = "unknown")
    ## Wages - private sector
    z11 <- paste("The average seasonal wage for a household with a male member in a private sector job in ", i, " Township is ",
                 round(mean(y$private_male_wage[y$private_male_wagecate == 3], na.rm = TRUE), digits = 2),
                 " MMK. For male members of households who earn a monthly wage for a private sector job, the average monthly wage is ",
                 round(mean(y$private_male_wage[y$private_male_wagecate == 2], na.rm = TRUE), digits = 2), " MMK.", sep = "")
    z11 <- stringr::str_replace_all(string = z11, pattern = "NA|NaN", replacement = "unknown")
    ## Access to town
    z12 <- merge(data.frame(round(prop.table(nipnTK::fullTable(y$acc_town_trans_dry, values = 1:7)), digits = 2) * 100),
                 data.frame(round(prop.table(nipnTK::fullTable(y$acc_town_trans_wet, values = 1:7)), digits = 2) * 100),
                 by = "Var1", all = TRUE)
    names(z12) <- c("Mode of transport", "Dry season", "Wet season")
    z12[ , 1] <- c("On foot", "Ox cart/horse cart", "Trailer jeep", "Bicycle", "Motorcycle", "Car", "Boat")
    z13 <- paste("The average distance of surveyed villages from the nearest town in ",
                 i, " Township is ",
                 round(mean(y$acc_town_dist, na.rm = TRUE), digits = 1),
                 " miles. During the dry season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z12[which(z12[ , 3] == max(z12[ , 3])), 1]),
                 " to access the nearest town and it takes on average ",
                 round(mean(y$acc_town_time_dry, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_town_cost_dry, na.rm = TRUE), digits = 2),
                 " MMK. During the wet season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z12[which(z12[ , 3] == max(z12[ , 3])), 1]),
                 " to access the nearest town and it takes on average ",
                 round(mean(y$acc_town_time_wet, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_town_cost_wet, na.rm = TRUE), digits = 2),
                 " MMK.",
                 sep = "")
    ## Access to SRHC
    z14 <- merge(data.frame(round(prop.table(nipnTK::fullTable(y$acc_srhc_trans_dry, values = 1:7)), digits = 2) * 100),
                 data.frame(round(prop.table(nipnTK::fullTable(y$acc_srhc_trans_wet, values = 1:7)), digits = 2) * 100),
                 by = "Var1", all = TRUE)
    names(z14) <- c("Mode of transport", "Dry season", "Wet season")
    z14[ , 1] <- c("On foot", "Ox cart/horse cart", "Trailer jeep", "Bicycle", "Motorcycle", "Car", "Boat")
    z15 <- paste("The average distance of surveyed villages from the nearest SRHC in ",
                 i, " Township is ",
                 round(mean(y$acc_srhc_dist, na.rm = TRUE), digits = 1),
                 " miles. During the dry season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z14[which(z14[ , 3] == max(z14[ , 3])), 1]),
                 " to access the nearest SRHC and it takes on average ",
                 round(mean(y$acc_srhc_time_dry, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_srhc_cost_dry, na.rm = TRUE), digits = 2),
                 " MMK. During the wet season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z14[which(z14[ , 3] == max(z14[ , 3])), 1]),
                 " to access the nearest SRHC and it takes on average ",
                 round(mean(y$acc_srhc_time_wet, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_srhc_cost_wet, na.rm = TRUE), digits = 2),
                 " MMK.",
                 sep = "")
    ## Access to State Hospital
    z16 <- merge(data.frame(round(prop.table(nipnTK::fullTable(y$acc_sthosp_trans_dry, values = 1:7)), digits = 2) * 100),
                 data.frame(round(prop.table(nipnTK::fullTable(y$acc_sthosp_trans_wet, values = 1:7)), digits = 2) * 100),
                 by = "Var1", all = TRUE)
    names(z16) <- c("Mode of transport", "Dry season", "Wet season")
    z16[ , 1] <- c("On foot", "Ox cart/horse cart", "Trailer jeep", "Bicycle", "Motorcycle", "Car", "Boat")
    z17 <- paste("The average distance of surveyed villages in ",
                 i, " Township from State Hospital is ",
                 round(mean(y$acc_sthosp_dist, na.rm = TRUE), digits = 1),
                 " miles. During the dry season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z16[which(z16[ , 3] == max(z16[ , 3])), 1]),
                 " to access the State Hospital and it takes on average ",
                 round(mean(y$acc_sthosp_time_dry, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_sthosp_cost_dry, na.rm = TRUE), digits = 2),
                 " MMK. During the wet season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z16[which(z16[ , 3] == max(z16[ , 3])), 1]),
                 " to access the State Hospital and it takes on average ",
                 round(mean(y$acc_sthosp_time_wet, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_sthosp_cost_wet, na.rm = TRUE), digits = 2),
                 " MMK.",
                 sep = "")
    ## Access to Primary School
    z18 <- merge(data.frame(round(prop.table(nipnTK::fullTable(y$acc_pschool_trans_dry, values = 1:7)), digits = 2) * 100),
                 data.frame(round(prop.table(nipnTK::fullTable(y$acc_pschool_trans_wet, values = 1:7)), digits = 2) * 100),
                 by = "Var1", all = TRUE)
    names(z18) <- c("Mode of transport", "Dry season", "Wet season")
    z18[ , 1] <- c("On foot", "Ox cart/horse cart", "Trailer jeep", "Bicycle", "Motorcycle", "Car", "Boat")
    z19 <- paste("The average distance of surveyed villages in ",
                 i, " Township from nearest primary school is ",
                 round(mean(y$acc_pschool_dist, na.rm = TRUE), digits = 1),
                 " miles. During the dry season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z18[which(z18[ , 3] == max(z18[ , 3])), 1]),
                 " to access the nearest primary school and it takes on average ",
                 round(mean(y$acc_pschool_time_dry, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_pschool_cost_dry, na.rm = TRUE), digits = 2),
                 " MMK. During the wet season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z18[which(z18[ , 3] == max(z18[ , 3])), 1]),
                 " to access the nearest primary school and it takes on average ",
                 round(mean(y$acc_pschool_time_wet, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_pschool_cost_wet, na.rm = TRUE), digits = 2),
                 " MMK.",
                 sep = "")
    ## Access to Middle School
    z20 <- merge(data.frame(round(prop.table(nipnTK::fullTable(y$acc_mschool_trans_dry, values = 1:7)), digits = 2) * 100),
                 data.frame(round(prop.table(nipnTK::fullTable(y$acc_mschool_trans_wet, values = 1:7)), digits = 2) * 100),
                 by = "Var1", all = TRUE)
    names(z20) <- c("Mode of transport", "Dry season", "Wet season")
    z20[ , 1] <- c("On foot", "Ox cart/horse cart", "Trailer jeep", "Bicycle", "Motorcycle", "Car", "Boat")
    z21 <- paste("The average distance of surveyed villages in ",
                 i, " Township from the nearest middle school is ",
                 round(mean(y$acc_mschool_dist, na.rm = TRUE), digits = 1),
                 " miles. During the dry season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z20[which(z20[ , 3] == max(z20[ , 3])), 1]),
                 " to access the nearest middle school and it takes on average ",
                 round(mean(y$acc_mschool_time_dry, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_mschool_cost_dry, na.rm = TRUE), digits = 2),
                 " MMK. During the wet season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z20[which(z20[ , 3] == max(z20[ , 3])), 1]),
                 " to access the nearest middle school and it takes on average ",
                 round(mean(y$acc_mschool_time_wet, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_mschool_cost_wet, na.rm = TRUE), digits = 2),
                 " MMK.",
                 sep = "")
    ## Access to High School
    z22 <- merge(data.frame(round(prop.table(nipnTK::fullTable(y$acc_hschool_trans_dry, values = 1:7)), digits = 2) * 100),
                 data.frame(round(prop.table(nipnTK::fullTable(y$acc_hschool_trans_wet, values = 1:7)), digits = 2) * 100),
                 by = "Var1", all = TRUE)
    names(z22) <- c("Mode of transport", "Dry season", "Wet season")
    z22[ , 1] <- c("On foot", "Ox cart/horse cart", "Trailer jeep", "Bicycle", "Motorcycle", "Car", "Boat")
    z23 <- paste("The average distance of surveyed villages in ",
                 i, " Township from the nearest high school is ",
                 round(mean(y$acc_hschool_dist, na.rm = TRUE), digits = 1),
                 " miles. During the dry season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z22[which(z22[ , 3] == max(z22[ , 3])), 1]),
                 " to access the nearest high school and it takes on average ",
                 round(mean(y$acc_hschool_time_dry, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_hschool_cost_dry, na.rm = TRUE), digits = 2),
                 " MMK. During the wet season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z22[which(z22[ , 3] == max(z22[ , 3])), 1]),
                 " to access the nearest high school and it takes on average ",
                 round(mean(y$acc_hschool_time_wet, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_hschool_cost_wet, na.rm = TRUE), digits = 2),
                 " MMK.",
                 sep = "")
    ## Access to bank
    z24 <- merge(data.frame(round(prop.table(nipnTK::fullTable(y$acc_bank_trans_dry, values = 1:7)), digits = 2) * 100),
                 data.frame(round(prop.table(nipnTK::fullTable(y$acc_bank_trans_wet, values = 1:7)), digits = 2) * 100),
                 by = "Var1", all = TRUE)
    names(z24) <- c("Mode of transport", "Dry season", "Wet season")
    z24[ , 1] <- c("On foot", "Ox cart/horse cart", "Trailer jeep", "Bicycle", "Motorcycle", "Car", "Boat")
    z25 <- paste("The average distance of surveyed villages in ",
                 i, " Township from the nearest bank is ",
                 round(mean(y$acc_bank_dist, na.rm = TRUE), digits = 1),
                 " miles. During the dry season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z22[which(z22[ , 3] == max(z22[ , 3])), 1]),
                 " to access the nearest bank and it takes on average ",
                 round(mean(y$acc_bank_time_dry, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_bank_cost_dry, na.rm = TRUE), digits = 2),
                 " MMK. During the wet season, most people in the surveyed villages use the ",
                 stringr::str_to_lower(string = z22[which(z22[ , 3] == max(z22[ , 3])), 1]),
                 " to access the nearest bank and it takes on average ",
                 round(mean(y$acc_bank_time_wet, na.rm = TRUE), digits = 2),
                 " minutes to get there costing them on average about ",
                 round(mean(y$acc_bank_cost_wet, na.rm = TRUE), digits = 2),
                 " MMK.",
                 sep = "")
    ## Type of health post
    z26 <- data.frame("Type of Health Facility" = c("Hospital",
                                                    "UHC",
                                                    "RHC",
                                                    "Sub-RHC",
                                                    "EHO clinic",
                                                    "NGO clinic",
                                                    "Private clinic",
                                                    "Pharmacy"),
                      "Percentage" = round(colMeans(x = y[ , paste("health_post_vill.", 1:8, sep = "")], na.rm = TRUE), digits = 2) * 100)
    row.names(z26) <- 1:8
    ## Type of health worker
    z27 <- data.frame("Type of Health Worker" = c("Midwife",
                                                  "Assistant midwife",
                                                  "Community health worker",
                                                  "Trained traditional birth attendant"),
                      "Percentage" = round(colMeans(x = y[ , c("health_hr_mw", "health_hr_amw", "health_hr_chw", "health_hr_ttba")], na.rm = TRUE), digits = 2) * 100,
                      "Mean number of health workers" = round(colMeans(x = y[ , c("health_hr_mw_num", "health_hr_amw_num", "health_hr_chw_num", "health_hr_ttba_num")], na.rm = TRUE), digits = 2))
    row.names(z27) <- 1:4
    ## Electricity source
    z28 <- data.frame("Source of Electricity" = c("Government",
                                                  "Village",
                                                  "Private",
                                                  "Solar"),
                      "Percentage" = round(colMeans(x = y[ , c("electric_gov", "electric_vill", "electric_private", "electric_solar")], na.rm = TRUE), digits = 2) * 100,
                      "Mean number of households with this source of electricity" = round(colMeans(x = y[ , c("electrichh_gov_num", "electrichh_vill_num", "electrichh_private_num", "electric_solar_num")], na.rm = TRUE), digits = 2))
    row.names(z28) <- 1:4
    ## CBO
    z29 <- paste("There are about ", round(mean(y$cbo_yn, na.rm = TRUE), digits = 2),
                 "% of villages in ", i, " Township with community-based organisations. On average, there are ",
                 round(mean(y$cbo_num, na.rm = TRUE), digits = 2), " CBOs per village in the township.", sep = "")
    ##
    z <- list(z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16, z17, z18, z19, z20, z21, z22, z23, z24, z25, z26, z27, z28, z29)
    desc[[i]] <- z
  }
  return(desc)
}
