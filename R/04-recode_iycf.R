################################################################################
#
#' Function to create raw IYCF dataset
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household information that will be linked to the
#'   IYCF data
#' @param y A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household roster information that will be linked to the
#'   IYCF data
#'
#' @return A data.frame containing raw IYCF data for the Myanmar
#'   MCCT Programme Evaluation Study
#'
#' @examples
#' create_iycf(df = iycf, x = hh, y = hhMembers)
#'
#' @export
#'
#
################################################################################

create_iycf <- function(df, x, y) {
  ## Create data.frame from hh dataset to get needed information
  x <- x[ , c("KEY", "intrv_date", "geo_state", "geo_rural", "geo_villward",
              "sample_component", "will_participate")]

  ## Create data.frame from hhMembers dataset to get needed information
  y <- y[ , c("PARENT_KEY", "KEY", "hh_mem_sex", "hh_mem_age",
              "hh_mem_age_month", "hh_mem_dob")]

  ## change KEY to match child health KEY
  y$KEY <- stringr::str_replace_all(string = y$KEY,
                                    pattern = "grp_hh",
                                    replacement = "grp_q2_5_to_q2_7")

  ## Merge hhMembers data with iycf to get hhMember information
  xy <- merge(y, df, by.x = "KEY", by.y = "KEY", all.y = TRUE)

  ## Merge hh data with merged iycf data to get hh information
  z <- merge(x, xy, by.x = "KEY", by.y = "PARENT_KEY.y", all.y = TRUE)

  ## Calculate age in days
  age <- as.numeric(lubridate::mdy(z$intrv_date) -  lubridate::mdy(z$hh_mem_dob))
  age <- ifelse(is.na(age),
                (as.integer(z$hh_mem_age) * 365.25) +
                  (as.integer(z$hh_mem_age_month) * (365.25/ 12)),
                age)

  ## Add age to data.frame
  z$age <- age

  ## Subset to those whose age is less than 730 days
  z <- z[z$age < (2 * 365.25) & !is.na(z$age), ]

  ## remove unnecessary variables
  z <- z[ , names(z)[!names(z) %in% c("KEY.y",
                                      "PARENT_KEY.x",
                                      "SET.OF.child_vc_rep")]]

  ## return data.frame
  return(z)
}

################################################################################
#
#' Function to recode early initiation of breastfeeding indicator for the
#' Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF/breastfeeding data
#'
#' @return A vector of recoded early initiation of breastfeeding status
#'
#' @examples
#' iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
#' recode_eibf(df = iycfDF)
#'
#' @export
#'
#
################################################################################

recode_eibf <- function(df) {
  eibf <- bbw::recode(var = df$child_eibf, recodes = "0=1;1:3=0;777=NA;999=NA")
  eibf[as.integer(df$child_eibf_hrs) <= 1] <- 1
  return(eibf)
}


################################################################################
#
#' Function to recode exclusive breastfeeding indicator for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF/breastfeeding data
#'
#' @return A vector of recoded exclusive breastfeeding status
#'
#' @examples
#' iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
#' recode_ebf(df = iycfDF)
#'
#' @export
#'
#
################################################################################

recode_ebf <- function(df) {
  ebf <- as.integer(df$child_bfyest)
  ebf[as.integer(df$hh_mem_age_month) >= 6] <- NA
  ebf[as.integer(df$child_bfyest) == 1] <- 1
  ebf[as.integer(df$child_vitdrop) == 1] <- 0
  ebf[as.integer(df$child_ors) == 1] <- 0
  ebf[as.integer(df$child_water) == 1] <- 0
  ebf[as.integer(df$child_juice) == 1] <- 0
  ebf[as.integer(df$child_broth) == 1] <- 0
  ebf[as.integer(df$child_porridge) == 1] <- 0
  ebf[as.integer(df$child_bms) == 1] <- 0
  ebf[as.integer(df$child_milk) == 1] <- 0
  ebf[as.integer(df$child_mproduct) == 1] <- 0
  ebf[as.integer(df$child_liquid) == 1] <- 0
  ebf[as.integer(df$child_rice) == 1] <- 0
  ebf[as.integer(df$child_potatoes) == 1] <- 0
  ebf[as.integer(df$child_pumpkin) == 1] <- 0
  ebf[as.integer(df$child_beans) == 1] <- 0
  ebf[as.integer(df$child_leafyveg) == 1] <- 0
  ebf[as.integer(df$child_mango) == 1] <- 0
  ebf[as.integer(df$child_fruit) == 1] <- 0
  ebf[as.integer(df$child_organ) == 1] <- 0
  ebf[as.integer(df$child_beef) == 1] <- 0
  ebf[as.integer(df$child_fish) == 1] <- 0
  ebf[as.integer(df$child_insects) == 1] <- 0
  ebf[as.integer(df$child_eggs) == 1] <- 0
  ebf[as.integer(df$child_yogurt) == 1] <- 0
  ebf[as.integer(df$child_fat) == 1] <- 0
  ebf[as.integer(df$child_plam) == 1] <- 0
  ebf[as.integer(df$child_sweets) == 1] <- 0
  ebf[as.integer(df$child_condiments) == 1] <- 0
  ebf[as.integer(df$hh_mem_age_month) >= 6] <- NA
  ebf <- as.numeric(ebf)
  return(ebf)
}


################################################################################
#
#' Function to recode minimum meal frequency indicator for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF/breastfeeding data
#'
#' @return A vector of recoded minimum meal frequency status
#'
#' @examples
#' iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
#' recode_mmf(df = iycfDF)
#'
#' @export
#'
#
################################################################################

recode_mmf <- function(df) {
  mmf <- vector(mode = "numeric", length = nrow(df))
  ## Recode all MMF
  mmf[df$hh_mem_age_month < 6] <- NA
  mmf[as.integer(df$child_bfyest) == 1 & as.integer(df$hh_mem_age_month) %in% 6:8 & as.integer(df$child_food_freq) >= 2] <- 1
  mmf[as.integer(df$child_bfyest) == 1 & as.integer(df$hh_mem_age_month) %in% 6:8 & as.integer(df$child_food_freq) < 2] <- 0
  mmf[as.integer(df$child_bfyest) == 1 & as.integer(df$hh_mem_age_month) %in% 9:23 & as.integer(df$child_food_freq) >= 3] <- 1
  mmf[as.integer(df$child_bfyest) == 1 & as.integer(df$hh_mem_age_month) %in% 9:23 & as.integer(df$child_food_freq) < 3] <- 0
  mmf[as.integer(df$child_bfyest) == 0 & as.integer(df$hh_mem_age_month) %in% 6:23 & as.integer(df$child_food_freq) >= 4] <- 1
  mmf[as.integer(df$child_bfyest) == 0 & as.integer(df$hh_mem_age_month) %in% 6:23 & as.integer(df$child_food_freq) < 4] <- 0
  ## Recode MMF breastfed 6 to 8 years old
  mmf1 <- vector(mode = "numeric", length = nrow(df))
  mmf1 <- NA
  mmf1[as.integer(df$child_bfyest) == 1 & as.integer(df$hh_mem_age_month) %in% 6:8 & as.integer(df$child_food_freq) >= 2] <- 1
  mmf1[as.integer(df$child_bfyest) == 1 & as.integer(df$hh_mem_age_month) %in% 6:8 & as.integer(df$child_food_freq) < 2] <- 0
  ## Recode MMF breastfed 9 to 23 years old
  mmf2 <- vector(mode = "numeric", length = nrow(df))
  mmf2 <- NA
  mmf2[as.integer(df$child_bfyest) == 1 & as.integer(df$hh_mem_age_month) %in% 9:23 & as.integer(df$child_food_freq) >= 3] <- 1
  mmf2[as.integer(df$child_bfyest) == 1 & as.integer(df$hh_mem_age_month) %in% 9:23 & as.integer(df$child_food_freq) < 3] <- 0
  ## Recode MMF non-breastfed 6-23 months
  mmf3 <- vector(mode = "numeric", length = nrow(df))
  mmf3 <- NA
  mmf3[as.integer(df$child_bfyest) == 0 & as.integer(df$hh_mem_age_month) %in% 6:23 & as.integer(df$child_food_freq) >= 4] <- 1
  mmf3[as.integer(df$child_bfyest) == 0 & as.integer(df$hh_mem_age_month) %in% 6:23 & as.integer(df$child_food_freq) < 4] <- 0
  ## Concatenate MMF indicators
  mmf <- data.frame(mmf1, mmf2, mmf3, mmf)
  ## Return MMF indicators
  return(mmf)
}


################################################################################
#
#' Function to recode child food groups for the Myanmar MCCT Programme
#' Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF/breastfeeding data
#'
#' @return A vector of recoded food groups
#'
#' @examples
#' iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
#' recode_mdd(df = iycfDF)
#'
#' @export
#'
#
################################################################################

recode_mdd <- function(df) {
  ## fg1 - grains, roots and tubers
  porridge <- as.numeric(df$child_porridge)
  grains <- as.numeric(df$child_rice)
  roots <- as.numeric(df$child_potatoes)
  fg1 <- porridge + grains + roots
  fg1 <- bbw::recode(var = fg1,
                     recodes = "1:3=1;1000:1001=1;999=0;1998=0;2997=0")
  ## fg2 - legumes and nuts
  fg2 <- as.numeric(df$child_beans)
  fg2 <- bbw::recode(var = fg2, recodes = "999=0")
  ## fg3 - dairy products
  formula <- as.numeric(df$child_bms)
  yogurt <- as.numeric(df$child_mproduct)
  milk <- as.numeric(df$child_milk)
  milkProduct <- as.numeric(df$child_yogurt)
  fg3 <- formula + yogurt + milk + milkProduct
  fg3 <- bbw::recode(var = fg3,
                     recodes = "1:4=1;1000:1002=1;999=0;1998=0;2997=0;3996=0")
  ## fg4 - flesh foods
  meat <- as.numeric(df$child_beef)
  fish <- as.numeric(df$child_fish)
  organ <- as.numeric(df$child_organ)
  insect <- as.numeric(df$child_insects)
  fg4 <- meat + fish + organ + insect
  fg4 <- bbw::recode(var = fg4,
                     recodes = "1:4=1;1000:1002=1;999=0;1998=0;2997=0;3996=0")
  ## fg5 - eggs
  fg5 <- as.numeric(df$child_eggs)
  fg5 <- bbw::recode(var = fg5, recodes = "999=0")
  ## fg6 - Vitamin A-rich fruits and vegetables
  pumpkin <- as.numeric(df$child_pumpkin)
  leafy <- as.numeric(df$child_leafyveg)
  mango <- as.numeric(df$child_mango)
  palm <- as.numeric(df$child_plam)
  fg6 <- pumpkin + leafy + mango + palm
  fg6 <- bbw::recode(var = fg6,
                     recodes = "1:4=1;1000:1002=1;999=0;1998=0;2997=0;3996=0")
  ## fg7 - Other fruits and vegetables
  fg7 <- as.numeric(df$child_fruit)
  fg7 <- bbw::recode(var = fg7, recodes = "999=0")
  ## fgscore
  fgscore <- fg1 + fg2 + fg3 + fg4 + fg5 + fg6 + fg7
  ## Minimum dietary diversity
  mdd <- bbw::recode(var = fgscore, recodes = "4:7=1;NA=NA;else=0")
  ##
  mdda <- bbw::recode(var = fgscore - fg3, recodes = "4:7=1;NA=NA;else=0")
  ## Create mdd data.frame
  mdd <- data.frame(fg1, fg2, fg3, fg4, fg5, fg6, fg7, fgscore, mdd, mdda)
  ## Return mdd data.frame
  return(mdd)
}


################################################################################
#
#' Function to recode minimum acceptable diet indicator for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF/breastfeeding data
#'
#' @return A vector of recoded minimum acceptable diet status
#'
#' @examples
#' iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
#' recode_mad(df = iycfDF)
#'
#' @export
#'
#
################################################################################

recode_mad <- function(df) {
  ##
  mmf <- recode_mmf(df = df)
  ##
  mdd <- recode_mdd(df = df)
  ##
  mad <- vector(mode = "numeric", length = nrow(df))
  ##
  mad[is.na(mmf$mmf)] <- NA
  ## Determine which kids meet MAD criteria
  mad[as.integer(df$child_bfyest) == 1 &
        as.integer(df$hh_mem_age_month) %in% 6:23 &
        mmf$mmf == 1 & mdd$mdd == 1] <- 1
  mad[as.integer(df$child_bfyest) == 0 &
        as.integer(df$hh_mem_age_month) %in% 6:23 &
        mmf$mmf == 1 & mdd$mdda == 1 &
        (as.integer(df$child_bms_freq) +
           as.integer(df$child_milk_freq) +
           as.integer(df$child_mproduct_freq)) >= 2] <- 1
  ## MAD for breastfed kids
  mad1 <- vector(mode = "numeric", length = nrow(df))
  ##
  mad1[is.na(mmf$mmf)] <- NA
  ##
  mad1[as.integer(df$child_bfyest) == 0] <- NA
  mad1[as.integer(df$child_bfyest) == 1 &
         as.integer(df$hh_mem_age_month) %in% 6:23 &
         mmf$mmf == 1 & mdd$mdd == 1] <- 1
  ## MAD for non-breastfed kids
  mad2 <- vector(mode = "numeric", length = nrow(df))
  ##
  mad2[is.na(mmf$mmf)] <- NA
  ##
  mad2[as.integer(df$child_bfyest) == 1] <- NA
  mad2[as.integer(df$child_bfyest) == 0 &
         as.integer(df$hh_mem_age_month) %in% 6:23 &
         mmf$mmf == 1 & mdd$mdda == 1 &
         (as.integer(df$child_bms_freq) +
            as.integer(df$child_milk_freq) +
            as.integer(df$child_mproduct_freq)) >= 2] <- 1
  ## Concatenate MAD
  mad <- data.frame(mad1, mad2, mad)
  ## Return MAD
  return(mad)
}


################################################################################
#
#' Function to recode timely complementary feeding
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF/breastfeeding data
#'
#' @return A vector of recoded timely complementary feeding status
#'
#' @examples
#' iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
#' recode_comp_feed(df = iycfDF)
#'
#' @export
#'
#
################################################################################

recode_comp_feed <- function(df) {
  compFeeding <- vector(mode = "numeric", length = nrow(df))
  compFeeding[!as.integer(df$hh_mem_age_month) %in% 6:8] <- NA
  compFeeding[as.integer(df$child_food_freq) >= 1 &
                as.integer(df$hh_mem_age_month) %in% 6:8] <- 1
  return(compFeeding)
}


################################################################################
#
#' Function to recode continued breastfeeding indicators
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF/breastfeeding data
#'
#' @return A vector of recoded continued breastfeeding status
#'
#' @examples
#' iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
#' recode_cont_bf(df = iycfDF)
#'
#' @export
#'
#
################################################################################

recode_cont_bf <- function(df) {
  ##
  contBF1 <- vector(mode = "numeric", length = nrow(df))
  contBF1[!as.integer(df$hh_mem_age_month) %in% 12:15] <- NA
  contBF1[as.integer(df$child_bfyest) == 1 &
            as.integer(df$hh_mem_age_month) %in% 12:15] <- 1
  ##
  contBF2 <- vector(mode = "numeric", length = nrow(df))
  contBF2[!as.integer(df$hh_mem_age_month) %in% 20:23] <- NA
  contBF2[as.integer(df$child_bfyest == 1)
          & as.integer(df$hh_mem_age_month) %in% 20:23] <- 1
  ##
  contBF <- data.frame(contBF1, contBF2)
  ##
  return(contBF)
}

################################################################################
#
#' Function to recode relevant IYCF indicators for the Myanmar MCCT Programme
#' evaluation study
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing IYCF/breastfeeding data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A data.frame of recoded IYCF indicators
#'
#' @examples
#' iycfDF <- create_iycf(df = iycf, x = hh, y = hhMembers)
#' recode_iycf(df = iycfDF)
#'
#' @export
#'
#
################################################################################

recode_iycf <- function(df,
                        core.columns = c("KEY",
                                         "geo_state",
                                         "geo_rural",
                                         "geo_villward",
                                         "sample_component",
                                         "hh_mem_sex")) {
  ## Recode early initation of breastfeeding
  eibf <- recode_eibf(df = df)
  ## Recode exclusive breastfeeding
  ebf <- recode_ebf(df = df)
  ## Recode continued breastfeeding
  cbf <- recode_cont_bf(df = df)
  ## Recode complementary feeding
  compFeeding <- recode_comp_feed(df = df)
  ## Recode minimum meal frequency
  mmf <- recode_mmf(df = df)
  ## Recode minimum dietary diversity
  mdd <- recode_mdd(df = df)
  ## Recode minimum acceptable diet
  mad <- recode_mad(df = df)
  ## Concatenate IYCF indicators to data.frame with core columns
  iycfDF <- data.frame(df[ , core.columns],
                       eibf, ebf, cbf, compFeeding, mmf, mdd, mad)
  names(iycfDF)[6] <- "sex"
  iycfDF$sex <- bbw::recode(var = iycfDF$sex, recodes = "1=1;0=2;NA=NA")
  ## Return data.frame
  return(iycfDF)
}
