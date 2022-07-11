################################################################################
#
#' Function to create raw child health dataset
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child health data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household information that will be linked to the
#'   child health data
#' @param y A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household roster information that will be linked to the
#'   child health data
#'
#' @return A data.frame containing raw child health data for the Myanmar
#'   MCCT Programme Evaluation Study
#'
#' @examples
#' create_chealth(df = childHealth, x = hh, y = hhMembers)
#'
#' @export
#'
#
################################################################################

create_chealth <- function(df, x, y) {
  ## Create data.frame from hh dataset to get needed information
  x <- x[ , c("KEY", "intrv_date", "geo_state", "geo_rural", "geo_villward",
              "sample_component", "will_participate")]

  ## Create data.frame from hhMembers dataset to get needed information
  y <- y[ , c("PARENT_KEY", "KEY", "hh_mem_sex", "hh_mem_age",
              "hh_mem_age_month", "hh_mem_dob")]

  ## change KEY to match child health KEY
  y$KEY <- stringr::str_replace_all(string = y$KEY,
                                    pattern = "grp_hh",
                                    replacement = "child_vc_rep")

  ## Merge hhMembers data with childHealth to get hhMember information
  xy <- merge(y, df, by.x = "KEY", by.y = "KEY", all.y = TRUE)

  ## Merge hh data with merged childHealth data to get hh information
  z <- merge(x, xy, by.x = "KEY", by.y = "PARENT_KEY.y", all.y = TRUE)

  ## Calculate age in days
  age <- as.numeric(lubridate::mdy(z$intrv_date) - lubridate::mdy(z$hh_mem_dob))
  age <- ifelse(is.na(age),
                (as.integer(z$hh_mem_age) * 365.25) +
                  (as.integer(z$hh_mem_age_month) * (365.25/ 12)),
                age)

  ## Add age to data.frame
  z$age <- age

  ## Subset to those whose age is less than 1825 days
  z <- z[z$age < (5 * 365.25) & !is.na(z$age), ]

  ## remove unnecessary variables
  z <- z[ , names(z)[!names(z) %in% c("KEY.y",
                                      "PARENT_KEY.x",
                                      "SET.OF.child_vc_rep")]]

  ## return data.frame
  return(z)
}


################################################################################
#
#' Function to recode vaccine coverage indicators for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child health data
#'
#' @return A vector of recoded vaccination coverage indicators
#'
#' @examples
#' chealth <- create_chealth(df = childHealth, x = hh, y = hhMembers)
#' recode_epi(df = chealth)
#'
#' @export
#'
#
################################################################################

recode_epi <- function(df) {
  ## Any vaccination
  vac1 <- as.integer(df$child_vaccin)
  vac1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
            ifelse(df$age / (365.25 / 12) >= 24, NA, vac1))
  ## immunisation card retention
  vac2 <- as.integer(df$child_vaccin_card)
  vac2 <- ifelse(df$age / (365.25 / 12) < 12, NA,
            ifelse(df$age / (365.25 / 12) >= 24, NA, vac2))
  ## BCG
  bcg1 <- as.integer(df$child_vc_bcg)
  bcg1 <- bbw::recode(var = bcg1, recodes = "999=0")
  bcg1 <- ifelse(is.na(vac1), NA, bcg1)
  bcg1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
            ifelse(df$age / (365.25 / 12) >= 24, NA, bcg1))
  ## Hepatitis B
  hepb1 <- as.integer(df$child_vc_hepb)
  hepb1 <- ifelse(is.na(vac1), NA, hepb1)
  hepb1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
            ifelse(df$age / (365.25 / 12) >= 24, NA, hepb1))
  ## Penta 1
  penta1a <- as.integer(df$child_vc_penta1)
  penta1a <- ifelse(is.na(vac1), NA, penta1a)
  penta1a <- ifelse(df$age / (365.25 / 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, penta1a))
  ## Penta 2
  penta2a <- as.integer(df$child_vc_penta2)
  penta2a <- ifelse(is.na(vac1), NA, penta2a)
  penta2a <- ifelse(df$age / (365.25 / 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, penta2a))
  ## Penta 3
  penta3a <- as.integer(df$child_vc_penta3)
  penta3a <- ifelse(is.na(vac1), NA, penta3a)
  penta3a <- ifelse(df$age / (365.25 / 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, penta3a))
  ## Polio 1
  polio1a <- as.integer(df$child_vc_polio1)
  polio1a <- ifelse(is.na(vac1), NA, polio1a)
  polio1a <- ifelse(df$age / (365.25 / 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, polio1a))
  ## Polio 2
  polio2a <- as.integer(df$child_vc_polio2)
  polio2a <- ifelse(is.na(vac1), NA, polio2a)
  polio2a <- ifelse(df$age / (365.25 / 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, polio2a))
  ## Polio 3
  polio3a <- as.integer(df$child_vc_polio3)
  polio3a <- ifelse(is.na(vac1), NA, polio3a)
  polio3a <- ifelse(df$age / (365.25 / 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, polio3a))
  ## Polio injection
  polioinj1 <- as.integer(df$child_vc_polioinj)
  polioinj1 <- ifelse(is.na(vac1), NA, polioinj1)
  polioinj1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
                 ifelse(df$age / (365.25 / 12) >= 24, NA, polioinj1))
  ## Measles 1
  measles1a <- as.integer(df$child_vc_measel1)
  measles1a <- ifelse(is.na(vac1), NA, measles1a)
  measles1a <- ifelse(df$age / (365.25 / 12) < 12, NA,
                 ifelse(df$age / (365.25 / 12) >= 24, NA, measles1a))
  ## Measles 2
  measles2a <- as.integer(df$child_vc_measel2)
  measles2a <- ifelse(is.na(vac1), NA, measles2a)
  measles2a <- ifelse(df$age / (365.25 / 12) < 12, NA,
                 ifelse(df$age / (365.25 / 12) >= 24, NA, measles2a))
  ## Rubella
  rubella1 <- as.integer(df$child_vc_rubella)
  rubella1 <- ifelse(is.na(vac1), NA, rubella1)
  rubella1 <- bbw::recode(var = rubella1, recodes = "999=0")
  rubella1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
                ifelse(df$age / (365.25 / 12) >= 24, NA, rubella1))
  ## Encephalitis
  enc1 <- as.integer(df$child_vc_encephalitis)
  enc1 <- ifelse(is.na(vac1), NA, enc1)
  enc1 <- bbw::recode(var = enc1, recodes = "999=0")
  enc1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
            ifelse(df$age / (365.25 / 12) >= 24, NA, enc1))
  ## BCG - no card
  bcg2 <- as.integer(df$child_novc_bcg)
  bcg2 <- bbw::recode(var = bcg2, recodes = "999=0;2=1")
  bcg2 <- ifelse(is.na(vac1), NA, bcg2)
  bcg2 <- ifelse(df$age / (365.25 / 12) < 12, NA,
            ifelse(df$age / (365.25 / 12) >= 24, NA, bcg2))
  ## Hepatitis B - no card
  hepb2 <- as.integer(df$child_novc_hepb)
  hepb2 <- bbw::recode(var = hepb2, recodes = "999=0")
  hepb2 <- ifelse(is.na(vac1), NA, hepb2)
  hepb2 <- ifelse(df$age / (365.25 / 12) < 12, NA,
             ifelse(df$age / (365.25 / 12) >= 24, NA, hepb2))
  ## Penta 1 - no card
  penta1b <- as.integer(df$child_novc_penta)
  penta1b <- ifelse(df$child_novc_penta_num >= 1, 1, 0)
  penta1b <- ifelse(is.na(vac1), NA, penta1b)
  penta1b <- ifelse(df$age / (365.25/ 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, penta1b))
  ## Penta 2 - no card
  penta2b <- as.integer(df$child_novc_penta2)
  penta2b <- ifelse(df$child_novc_penta_num >= 2, 1, 0)
  penta2b <- ifelse(is.na(vac1), NA, penta2b)
  penta2b <- ifelse(df$age / (365.25/ 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, penta2b))
  ## Penta 3 - no card
  penta3b <- as.integer(df$child_novc_penta3)
  penta3b <- ifelse(df$child_novc_penta_num == 3, 1, 0)
  penta3b <- ifelse(is.na(vac1), NA, penta3b)
  penta3b <- ifelse(df$age / (365.25/ 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, penta3b))
  ## Polio 1 - no card
  polio1b <- as.integer(df$child_novc_polio1)
  polio1b <- ifelse(df$child_novc_polio_num >= 1, 1, 0)
  polio1b <- ifelse(is.na(vac1), NA, polio1b)
  polio1b <- ifelse(df$age / (365.25/ 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, polio1b))
  ## Polio 2 - no card
  polio2b <- as.integer(df$child_novc_polio2)
  polio2b <- ifelse(df$child_novc_polio_num >= 2, 1, 0)
  polio2b <- ifelse(is.na(vac1), NA, polio2b)
  polio2b <- ifelse(df$age / (365.25/ 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, polio2b))
  ## Polio 3 - no card
  polio3b <- as.integer(df$child_novc_polio3)
  polio3b <- ifelse(df$child_novc_polio_num == 3, 1, 0)
  polio3b <- ifelse(is.na(vac1), NA, polio3b)
  polio3b <- ifelse(df$age / (365.25/ 12) < 12, NA,
               ifelse(df$age / (365.25 / 12) >= 24, NA, polio3b))
  ## Polio injection - no card
  polioinj2 <- as.integer(df$child_novc_polioinj)
  polioinj2 <- ifelse(is.na(vac1), NA, polioinj2)
  polioinj2 <- ifelse(df$age / (365.25/ 12) < 12, NA,
                 ifelse(df$age / (365.25 / 12) >= 24, NA, polioinj2))
  ## Measles 1 - no card
  measles1b <- as.integer(df$child_novc_measel1)
  measles1b <- ifelse(df$child_novc_measel_num >= 1, 1, 0)
  measles1b <- ifelse(is.na(vac1), NA, measles1b)
  measles1b <- ifelse(df$age / (365.25/ 12) < 12, NA,
                 ifelse(df$age / (365.25 / 12) >= 24, NA, measles1b))
  ## Measles 2 - no card
  measles2b <- as.integer(df$child_novc_measel2)
  measles2b <- ifelse(df$child_novc_measel_num == 2, 1, 0)
  measles2b <- ifelse(is.na(vac1), NA, measles2b)
  measles2b <- ifelse(df$age / (365.25/ 12) < 12, NA,
                 ifelse(df$age / (365.25 / 12) >= 24, NA, measles2b))
  ## Rubella - no card
  rubella2 <- as.integer(df$child_novc_rubella)
  rubella2 <- ifelse(is.na(vac1), NA, rubella2)
  rubella2 <- bbw::recode(var = rubella2, recodes = "999=0")
  rubella2 <- ifelse(df$age / (365.25/ 12) < 12, NA,
                ifelse(df$age / (365.25 / 12) >= 24, NA, rubella2))
  ## Encephalitis - no card
  enc2 <- as.integer(df$child_novc_encephalitis)
  enc2 <- ifelse(is.na(vac1), NA, enc2)
  enc2 <- bbw::recode(var = enc2, recodes = "999=0")
  enc2 <- ifelse(df$age / (365.25/ 12) < 12, NA,
            ifelse(df$age / (365.25 / 12) >= 24, NA, enc2))
  ## BCG - card or no card
  bcg <- rowSums(cbind(bcg1, bcg2), na.rm = TRUE)
  bcg <- ifelse(is.na(vac1), NA, bcg)
  ## Hepatitis B - card or no card
  hepb <- rowSums(cbind(hepb1, hepb2), na.rm = TRUE)
  hepb <- ifelse(is.na(vac1), NA, hepb)
  ## Penta 1 - card or no card
  penta1 <- rowSums(cbind(penta1a, penta1b), na.rm = TRUE)
  penta1 <- ifelse(is.na(vac1), NA, penta1)
  ## Penta 2 - card or no card
  penta2 <- rowSums(cbind(penta2a, penta2b), na.rm = TRUE)
  penta2 <- ifelse(is.na(vac1), NA, penta2)
  ## Penta 3 - card or no card
  penta3 <- rowSums(cbind(penta3a, penta3b), na.rm = TRUE)
  penta3 <- ifelse(is.na(vac1), NA, penta3)
  ## Polio 1 - card or no card
  polio1 <- rowSums(cbind(polio1a, polio1b), na.rm = TRUE)
  polio1 <- ifelse(is.na(vac1), NA, polio1)
  ## Polio 2 - card or no card
  polio2 <- rowSums(cbind(polio2a, polio2b), na.rm = TRUE)
  polio2 <- ifelse(is.na(vac1), NA, polio2)
  ## Polio 3 - card or no card
  polio3 <- rowSums(cbind(polio3a, polio3b), na.rm = TRUE)
  polio3 <- ifelse(is.na(vac1), NA, polio3)
  ## Polio injection - card or no card
  polioinj <- rowSums(cbind(polioinj1, polioinj2), na.rm = TRUE)
  polioinj <- ifelse(is.na(vac1), NA, polioinj)
  ## Measles 1
  measles1 <- rowSums(cbind(measles1a, measles1b), na.rm = TRUE)
  measles1 <- ifelse(is.na(vac1), NA, measles1)
  ## Measles 2
  measles2 <- rowSums(cbind(measles2a, measles2b), na.rm = TRUE)
  measles2 <- ifelse(is.na(vac1), NA, measles2)
  ## Rubella - card or no card
  rubella <- rowSums(cbind(rubella1, rubella2), na.rm = TRUE)
  rubella <- ifelse(is.na(vac1), NA, rubella)
  ## Encephalitis - card or no card
  enc <- rowSums(cbind(enc1, enc2), na.rm = TRUE)
  enc <- ifelse(is.na(vac1), NA, enc)
  ## Immunisation system access
  vac3 <- bcg
  #vac3 <- ifelse(df$age / (365.25 / 12) < 12, NA,
  #          ifelse(df$age / (365 / 12) >= 24, NA, vac3))
  ## Immunisation system utilization
  vac4 <- ifelse(penta3 == 1 & penta1 == 1, 1, 0)
  #vac4 <- ifelse(df$age / (365.25 / 12) < 12, NA,
  #          ifelse(df$age / (365 / 12) >= 24, NA, vac4))
  ## Fully immunised - 12-23 months
  vac5 <- ifelse(bcg == 1 & hepb == 1 &
                   penta1 == 1 & penta2 == 1 & penta3 == 1 &
                   polio1 == 1 & polio2 == 1 & polio3 == 1 &
                   measles1 == 1 & rubella == 1 & enc == 1, 1, 0)
  ## Full-immunised - 12-23 months compatible with DHS
  vac5a <- ifelse(bcg == 1 & hepb == 1 &
                    penta1 == 1 & penta2 == 1 & penta3 == 1 &
                    polio1 == 1 & polio2 == 1 & polio3 == 1 &
                    measles1 == 1, 1, 0)
  #vac5 <- ifelse(df$age / (365.25 / 12) < 12, NA,
  #          ifelse(df$age / (365 / 12) >= 24, NA, vac5))
  ## Hepatitis B vaccine within 24 hours
  vac6 <- as.integer(df$child_hepatitisb)
  vac6 <- bbw::recode(var = vac6, recodes = "999=0;NA=NA")
  ## Vitamin A supplementation
  vita <- as.integer(df$child_vita)
  vita <- bbw::recode(var = vita, recodes = "999=0;NA=NA")
  ## Deworming
  worm <- as.integer(df$child_deworm)
  worm <- bbw::recode(var = worm, recodes = "999=0;NA=NA")
  ## Concatenate EPI data.frame
  epi <- data.frame(bcg, hepb, penta1, penta2, penta3,
                    polio1, polio2, polio3, polioinj,
                    measles1, measles2, rubella, enc,
                    vac1, vac2, vac3, vac4, vac5, vac5a,
                    vac6, vita, worm)
  ## Return
  return(epi)
}


################################################################################
#
#' Function to recode age-appropriate vaccine coverage indicators for the
#' Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child health data
#'
#' @return A vector of recoded vaccination coverage indicators
#'
#' @examples
#' chealth <- create_chealth(df = childHealth, x = hh, y = hhMembers)
#' recode_age_epi(df = chealth)
#'
#' @export
#'
#
################################################################################

recode_age_epi <- function(df) {
  ## Any vaccination
  vac1 <- as.integer(df$child_vaccin)
  vac1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
                 ifelse(df$age / (365 / 12) >= 24, NA, vac1))
  ## immunisation card retention
  vac2 <- as.integer(df$child_vaccin_card)
  vac2 <- ifelse(df$age / (365.25 / 12) < 12, NA,
                 ifelse(df$age / (365 / 12) >= 24, NA, vac2))
  ## BCG - age appropriate - at birth
  bcg1 <- as.integer(df$child_vc_bcg)
  bcg1 <- bbw::recode(var = bcg1, recodes = "999=0")
  bcg1 <- ifelse(is.na(vac1), NA, bcg1)
  #bcg1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
  #               ifelse(df$age / (365 / 12) >= 24, NA, bcg1))
  ## Hepatitis B - age appropriate - at birth
  hepb1 <- as.integer(df$child_vc_hepb)
  hepb1 <- ifelse(is.na(vac1), NA, hepb1)
  #hepb1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
  #                ifelse(df$age / (365 / 12) >= 24, NA, hepb1))
  ## Penta 1 - age-appropriate - 2 months
  penta1a <- as.integer(df$child_vc_penta1)
  penta1a <- ifelse(is.na(vac1), NA, penta1a)
  penta1a <- ifelse(df$age / (365.25 / 12) < 2, NA, penta1a)
  ## Penta 2
  penta2a <- as.integer(df$child_vc_penta2)
  penta2a <- ifelse(is.na(vac1), NA, penta2a)
  penta2a <- ifelse(df$age / (365.25 / 12) < 4, NA, penta2a)
  ## Penta 3
  penta3a <- as.integer(df$child_vc_penta3)
  penta3a <- ifelse(is.na(vac1), NA, penta3a)
  penta3a <- ifelse(df$age / (365.25 / 12) < 6, NA, penta3a)
  ## Polio 1
  polio1a <- as.integer(df$child_vc_polio1)
  polio1a <- ifelse(is.na(vac1), NA, polio1a)
  polio1a <- ifelse(df$age / (365.25 / 12) < 2, NA, polio1a)
  ## Polio 2
  polio2a <- as.integer(df$child_vc_polio2)
  polio2a <- ifelse(is.na(vac1), NA, polio2a)
  polio2a <- ifelse(df$age / (365.25 / 12) < 4, NA, polio2a)
  ## Polio 3
  polio3a <- as.integer(df$child_vc_polio3)
  polio3a <- ifelse(is.na(vac1), NA, polio3a)
  polio3a <- ifelse(df$age / (365.25 / 12) < 6, NA, polio3a)
  ## Polio injection
  polioinj1 <- as.integer(df$child_vc_polioinj)
  polioinj1 <- ifelse(is.na(vac1), NA, polioinj1)
  polioinj1 <- ifelse(df$age / (365.25 / 12) < 4, NA, polioinj1)
  ## Measles 1
  measles1a <- as.integer(df$child_vc_measel1)
  measles1a <- ifelse(is.na(vac1), NA, measles1a)
  measles1a <- ifelse(df$age / (365.25 / 12) < 9, NA, measles1a)
  ## Measles 2
  measles2a <- as.integer(df$child_vc_measel2)
  measles2a <- ifelse(is.na(vac1), NA, measles2a)
  measles2a <- ifelse(df$age / (365.25 / 12) < 18, NA, measles2a)
  ## Rubella
  rubella1 <- as.integer(df$child_vc_rubella)
  rubella1 <- ifelse(is.na(vac1), NA, rubella1)
  rubella1 <- bbw::recode(var = rubella1, recodes = "999=0")
  rubella1 <- ifelse(df$age / (365.25 / 12) < 9, NA, rubella1)
  ## Encephalitis
  #enc1 <- as.integer(df$child_vc_encephalitis)
  #enc1 <- ifelse(is.na(vac1), NA, enc1)
  #enc1 <- bbw::recode(var = enc1, recodes = "999=0")
  #enc1 <- ifelse(df$age / (365.25 / 12) < 12, NA,
  #               ifelse(df$age / (365 / 12) >= 24, NA, enc1))
  ## BCG - no card
  bcg2 <- as.integer(df$child_novc_bcg)
  bcg2 <- bbw::recode(var = bcg2, recodes = "999=0")
  bcg2 <- ifelse(is.na(vac1), NA, bcg2)
  #bcg2 <- ifelse(df$age / (365.25 / 12) < 12, NA,
  #               ifelse(df$age / (365 / 12) >= 24, NA, bcg2))
  ## Hepatitis B - no card
  hepb2 <- as.integer(df$child_novc_hepb)
  hepb2 <- ifelse(is.na(vac1), NA, hepb2)
  #hepb2 <- ifelse(df$age / (365.25 / 12) < 12, NA,
  #                ifelse(df$age / (365 / 12) >= 24, NA, hepb2))
  ## Penta 1 - no card
  penta1b <- as.integer(df$child_novc_penta1)
  penta1b <- ifelse(is.na(vac1), NA, penta1b)
  penta1b <- ifelse(df$child_novc_penta_num >= 1, 1, 0)
  penta1b <- ifelse(df$age / (365.25/ 12) < 2, NA, penta1b)
  ## Penta 2 - no card
  penta2b <- as.integer(df$child_novc_penta2)
  penta2b <- ifelse(is.na(vac1), NA, penta2b)
  penta2b <- ifelse(df$child_novc_penta_num >= 2, 1, 0)
  penta2b <- ifelse(df$age / (365.25/ 12) < 4, NA, penta2b)
  ## Penta 3 - no card
  penta3b <- as.integer(df$child_novc_penta3)
  penta3b <- ifelse(is.na(vac1), NA, penta3b)
  penta3b <- ifelse(df$child_novc_penta_num == 3, 1, 0)
  penta3b <- ifelse(df$age / (365.25/ 12) < 6, NA, penta3b)
  ## Polio 1 - no card
  polio1b <- as.integer(df$child_novc_polio1)
  polio1b <- ifelse(is.na(vac1), NA, polio1b)
  polio1b <- ifelse(df$child_novc_polio_num >= 1, 1, 0)
  polio1b <- ifelse(df$age / (365.25/ 12) < 2, NA, polio1b)
  ## Polio 2 - no card
  polio2b <- as.integer(df$child_novc_polio2)
  polio2b <- ifelse(is.na(vac1), NA, polio2b)
  polio2b <- ifelse(df$child_novc_polio_num >= 2, 1, 0)
  polio2b <- ifelse(df$age / (365.25/ 12) < 4, NA, polio2b)
  ## Polio 3 - no card
  polio3b <- as.integer(df$child_novc_polio3)
  polio3b <- ifelse(is.na(vac1), NA, polio3b)
  polio3b <- ifelse(df$child_novc_polio_num == 3, 1, 0)
  polio3b <- ifelse(df$age / (365.25/ 12) < 6, NA, polio3b)
  ## Polio injection - no card
  polioinj2 <- as.integer(df$child_novc_polioinj)
  polioinj2 <- ifelse(is.na(vac1), NA, polioinj2)
  polioinj2 <- ifelse(df$age / (365.25/ 12) < 4, NA, polioinj2)
  ## Measles 1 - no card
  measles1b <- as.integer(df$child_novc_measel1)
  measles1b <- ifelse(is.na(vac1), NA, measles1b)
  measles1b <- ifelse(df$child_novc_measel_num >= 1, 1, 0)
  measles1b <- ifelse(df$age / (365.25/ 12) < 9, NA, measles1b)
  ## Measles 2 - no card
  measles2b <- as.integer(df$child_novc_measel2)
  measles2b <- ifelse(is.na(vac1), NA, measles2b)
  measles2b <- ifelse(df$child_novc_measel_num == 2, 1, 0)
  measles2b <- ifelse(df$age / (365.25/ 12) < 18, NA, measles2b)
  ## Rubella - no card
  rubella2 <- as.integer(df$child_novc_rubella)
  rubella2 <- ifelse(is.na(vac1), NA, rubella2)
  rubella2 <- bbw::recode(var = rubella2, recodes = "999=0")
  rubella2 <- ifelse(df$age / (365.25/ 12) < 9, NA, rubella2)
  ## Encephalitis - no card
  #enc2 <- as.integer(df$child_novc_encephalitis)
  #enc2 <- ifelse(is.na(vac1), NA, enc2)
  #enc2 <- bbw::recode(var = enc2, recodes = "999=0")
  #enc2 <- ifelse(df$age / (365.25/ 12) < 12, NA,
  #               ifelse(df$age / (365 / 12) >= 24, NA, enc2))
  ## BCG - card or no card
  bcg <- rowSums(cbind(bcg1, bcg2), na.rm = TRUE)
  bcg <- ifelse(is.na(vac1), NA, bcg)
  ## Hepatitis B - card or no card
  hepb <- rowSums(cbind(hepb1, hepb2), na.rm = TRUE)
  hepb <- ifelse(is.na(vac1), NA, hepb)
  ## Penta 1 - card or no card
  penta1 <- rowSums(cbind(penta1a, penta1b), na.rm = TRUE)
  penta1 <- ifelse(is.na(vac1), NA, penta1)
  ## Penta 2 - card or no card
  penta2 <- rowSums(cbind(penta2a, penta2b), na.rm = TRUE)
  penta2 <- ifelse(is.na(vac1), NA, penta2)
  ## Penta 3 - card or no card
  penta3 <- rowSums(cbind(penta3a, penta3b), na.rm = TRUE)
  penta3 <- ifelse(is.na(vac1), NA, penta3)
  ## Polio 1 - card or no card
  polio1 <- rowSums(cbind(polio1a, polio1b), na.rm = TRUE)
  polio1 <- ifelse(is.na(vac1), NA, polio1)
  ## Polio 2 - card or no card
  polio2 <- rowSums(cbind(polio2a, polio2b), na.rm = TRUE)
  polio2 <- ifelse(is.na(vac1), NA, polio2)
  ## Polio 3 - card or no card
  polio3 <- rowSums(cbind(polio3a, polio3b), na.rm = TRUE)
  polio3 <- ifelse(is.na(vac1), NA, polio3)
  ## Polio injection - card or no card
  polioinj <- rowSums(cbind(polioinj1, polioinj2), na.rm = TRUE)
  polioinj <- ifelse(is.na(vac1), NA, polioinj)
  ## Measles 1
  measles1 <- rowSums(cbind(measles1a, measles1b), na.rm = TRUE)
  measles1 <- ifelse(is.na(vac1), NA, measles1)
  ## Measles 2
  measles2 <- rowSums(cbind(measles2a, measles2b), na.rm = TRUE)
  measles2 <- ifelse(is.na(vac1), NA, measles2)
  ## Rubella - card or no card
  rubella <- rowSums(cbind(rubella1, rubella2), na.rm = TRUE)
  rubella <- ifelse(is.na(vac1), NA, rubella)
  ## Age appropriate immunisation status - less than 2 months old
  ageEPI1 <- ifelse(df$age / (365.25 / 12) < 2 & bcg == 1, 1,
               ifelse(df$age / (365.25 / 12) >= 2, NA, 0))
  ## Age appropriate immunisation status - 2 months to less than 4 months
  ageEPI2 <- ifelse(df$age / (365.25 / 12) >= 2 &
                      df$age / (365.25 / 12) < 4 &
                      bcg == 1 & penta1 == 1 & polio1 == 1, 1,
               ifelse(df$age / (365.25 / 12) < 2 |
                        df$age / (365.25 / 12) >= 4, NA, 0))
  ## Age appropriate immunisation status - 4 months to less than 6 months
  ageEPI3 <- ifelse(df$age / (365.25 / 12) >= 4 &
                      df$age / (365.25 / 12) < 6 &
                      bcg == 1 & penta1 == 1 & penta2 == 1 &
                      polio1 == 1 & polio2 == 1, 1,
                    ifelse(df$age / (365.25 / 12) < 4 |
                             df$age / (365.25 / 12) >= 6, NA, 0))
  ## Age appropriate immunisation status - 6 months to less than 9 months
  ageEPI4 <- ifelse(df$age / (365.25 / 12) >= 6 &
                      df$age / (365.25 / 12) < 9 &
                      bcg == 1 & penta1 == 1 & penta2 == 1 & penta3 == 1 &
                      polio1 == 1 & polio2 == 1 & polio3 == 1, 1,
                    ifelse(df$age / (365.25 / 12) < 6 |
                             df$age / (365.25 / 12) >= 9, NA, 0))
  ## Age appropriate immunisation status - 9 months to less than 18 months
  ageEPI5 <- ifelse(df$age / (365.25 / 12) >= 9 &
                      df$age / (365.25 / 12) < 18 &
                      bcg == 1 & penta1 == 1 & penta2 == 1 & penta3 == 1 &
                      polio1 == 1 & polio2 == 1 & polio3 == 1 &
                      measles1 == 1 & rubella == 1, 1,
                    ifelse(df$age / (365.25 / 12) < 9 |
                             df$age / (365.25 / 12) >= 18, NA, 0))
  ## Age appropriate immunisation status - 18 months or more
  ageEPI6 <- ifelse(df$age / (365.25 / 12) >= 18 &
                      bcg == 1 & penta1 == 1 & penta2 == 1 & penta3 == 1 &
                      polio1 == 1 & polio2 == 1 & polio3 == 1 &
                      measles1 == 1 & measles2 == 1 & rubella == 1, 1,
                    ifelse(df$age / (365.25 / 12) < 18, NA, 0))
  ##
  ageImm <- rowSums(cbind(ageEPI1, ageEPI2, ageEPI3,
                          ageEPI4, ageEPI5, ageEPI6),
                    na.rm = TRUE)
  ## Concatenate age-appropriate EPI data.frame
  epi <- data.frame(bcg, hepb, penta1, penta2, penta3,
                    polio1, polio2, polio3, polioinj,
                    measles1, measles2, rubella, ageImm)
  ##
  names(epi) <- c("ageBCG", "ageHepB", "agePenta1", "agePenta2", "agePenta3",
                  "agePolio1", "agePolio2", "agePolio3", "agePolioInj",
                  "ageMeasles1", "ageMeasles2", "ageRubella", "ageImm")
  ## Return
  return(epi)
}


################################################################################
#
#' Function to recode birth weight indicators for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child health data
#'
#' @return A vector of recoded birth weight in grams
#'
#' @examples
#' chealth <- create_chealth(df = childHealth, x = hh, y = hhMembers)
#' recode_birthweight(df = chealth)
#'
#' @export
#'
#
################################################################################

recode_birthweight <- function(df) {
  ## kgs
  bwt1 <- as.integer(df$child_birthwt_kg)
  bwt1 <- bwt1 * 1000
  ## lbs
  bwt2 <- as.integer(df$child_birthwt_lb)
  bwt2 <- bwt2 * 453.597
  ## ounces
  bwt3 <- as.integer(df$child_birthwt_oz)
  bwt3 <- bwt3 * 28.3495231
  ##
  bwt <- rowSums(cbind(bwt2, bwt3), na.rm = TRUE)
  ##
  #bwt <- rowSums(cbind(bwt1, bwt), na.rm = TRUE)
  bwt <- ifelse(is.na(bwt1), bwt, bwt1)
  bwt <- ifelse(bwt == 0, NA, bwt)
  ##
  doc <- as.integer(df$child_birthwt_doc)
  ##
  bwtCard <- ifelse(doc == 1, bwt, NA)
  ##
  lbw1 <- bbw::recode(var = bwt, recodes = "lo:2499=1;NA=NA;else=0")
  lbw2 <- bbw::recode(var = bwtCard, recodes = "lo:2499=1;NA=NA;else=0")
  ## concatenate data.frame
  bwt <- data.frame(bwt, bwtCard, lbw1, lbw2)
  ##
  return(bwt)
}


################################################################################
#
#' Function to recode diarrhoea prevalence and treatment-seeking behaviour for
#' the Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child health data
#'
#' @return A vector of recoded diarrhoea prevalence and treatment-seeking
#'   behaviour
#'
#' @examples
#' chealth <- create_chealth(df = childHealth, x = hh, y = hhMembers)
#' recode_diarrhoea(df = chealth)
#'
#' @export
#'
#
################################################################################

recode_diarrhoea <- function(df) {
  ##
  ill <- as.integer(df$child_ill)
  ## Had diarrhoea?
  dia1 <- as.integer(df$child_ill.1)
  dia1 <- ifelse(is.na(ill), NA, dia1)
  ## Advice or treatment for diarrhoea?
  dia2 <- as.integer(df$child_diarrh_treat)
  dia2 <- ifelse(is.na(ill) | is.na(dia1), NA, dia2)
  ## Reasons for no treatment
  dia3 <- as.integer(df$child_diarrh_notreat)
  dia3 <- ifelse(dia2 == 1, NA, dia3)
  dia3 <- ifelse(dia3 == 888, NA, dia3)
  ## Reasons for no treatment: No health facility available
  dia3a <- ifelse(dia3 == 1, 1, 0)
  ## Reasons for no treatment: Health facility too far
  dia3b <- ifelse(dia3 == 2, 1, 0)
  ## Reasons for no treatment: Health facility not accessible
  dia3c <- ifelse(dia3 == 3, 1, 0)
  ## Reasons for no treatment: Treatment expensive
  dia3d <- ifelse(dia3 == 4, 1, 0)
  ## Reasons for no treatment: treatment was no necessary
  dia3e <- ifelse(dia3 == 5, 1, 0)
  ## Reasons for no treatment: advised not to seek treatment at health facility
  dia3f <- ifelse(dia3 == 6, 1, 0)
  ## Reasons for no treatment: alternative treatment
  dia3g <- ifelse(dia3 == 7, 1, 0)
  ## Reasons for no treatment: Don't know about seeking treatment
  dia3h <- ifelse(dia3 == 8, 1, 0)
  ##
  dia4 <- as.integer(df$child_diarrh_notice)
  ##
  dia4[as.integer(df$child_diarrh_notice_unit) == 1 & !is.na(dia4)] <- dia4[as.integer(df$child_diarrh_notice_unit) == 1 & !is.na(dia4)]
  dia4[as.integer(df$child_diarrh_notice_unit) == 2 & !is.na(dia4)] <- dia4[as.integer(df$child_diarrh_notice_unit) == 2 & !is.na(dia4)] * 7
  dia4[as.integer(df$child_diarrh_notice_unit) == 3 & !is.na(dia4)] <- dia4[as.integer(df$child_diarrh_notice_unit) == 3 & !is.na(dia4)] * (365/12)
  dia4[is.na(dia1)] <- NA
  dia4[dia1 == 0] <- NA
  ## Where advice sought?
  dia5 <- as.integer(df$child_diarrh_where)
  dia5 <- ifelse(dia2 == 0, NA, dia5)
  ## Where: Township hospital
  dia5a <- ifelse(dia5 == 1, 1, 0)
  ## Where: Station hospital
  dia5b <- ifelse(dia5 == 2, 1, 0)
  ## Where: RHC/Health assistant
  dia5c <- ifelse(dia5 == 3, 1, 0)
  ## Where: SRHC/Midwife
  dia5d <- ifelse(dia5 == 4, 1, 0)
  ## Where: Private clinic/doctor
  dia5e <- ifelse(dia5 == 5, 1, 0)
  ## Where: Community health worker
  dia5f <- ifelse(dia5 == 6, 1, 0)
  ## Where: Traditional healer
  dia5g <- ifelse(dia5 == 7, 1, 0)
  ## Where: Untrained health worker
  dia5h <- ifelse(dia5 == 8, 1, 0)
  ## Where: Drug from shop
  dia5i <- ifelse(dia5 == 9, 1, 0)
  ## Where: EHO clinics/volunteers
  dia5j <- ifelse(dia5 == 10, 1, 0)
  ## Where: Family members
  dia5k <- ifelse(dia5 == 11, 1, 0)
  ## Where: NGOs/clinics
  dia5l <- ifelse(dia5 == 12, 1, 0)
  ## Where: auxilliary midwife
  dia5m <- ifelse(dia5 == 13, 1, 0)
  ## Where else advice sought?
  dia6 <- as.integer(df$child_diarrh_else)
  dia6 <- ifelse(dia2 == 0, NA, dia6)
  ## Where: Township hospital
  dia6a <- ifelse(dia6 == 1, 1, 0)
  ## Where: Station hospital
  dia6b <- ifelse(dia6 == 2, 1, 0)
  ## Where: RHC/Health assistant
  dia6c <- ifelse(dia6 == 3, 1, 0)
  ## Where: SRHC/Midwife
  dia6d <- ifelse(dia6 == 4, 1, 0)
  ## Where: Private clinic/doctor
  dia6e <- ifelse(dia6 == 5, 1, 0)
  ## Where: Community health worker
  dia6f <- ifelse(dia6 == 6, 1, 0)
  ## Where: Traditional healer
  dia6g <- ifelse(dia6 == 7, 1, 0)
  ## Where: Untrained health worker
  dia6h <- ifelse(dia6 == 8, 1, 0)
  ## Where: Drug from shop
  dia6i <- ifelse(dia6 == 9, 1, 0)
  ## Where: EHO clinics/volunteers
  dia6j <- ifelse(dia6 == 10, 1, 0)
  ## Where: Family members
  dia6k <- ifelse(dia6 == 11, 1, 0)
  ## Where: NGOs/clinics
  dia6l <- ifelse(dia6 == 12, 1, 0)
  ## Where: auxilliary midwife
  dia6m <- ifelse(dia6 == 13, 1, 0)
  ## Pay for advice/treatment?
  dia7 <- as.integer(df$child_diarrh_amount)
  dia7 <- ifelse(dia2 == 0, NA, dia7)
  dia7 <- ifelse(as.integer(df$child_diarrh_pay) == 0, 0, dia7)
  dia7 <- bbw::recode(var = dia7, recodes = "444=NA;666=NA;777=NA;999=NA")
  ## What are the costs for?
  dia8 <- as.integer(df$child_diarrh_items)
  dia8 <- ifelse(dia7 == 0 | is.na(dia7), NA, dia8)
  ## What for: transportation
  dia8a <- as.integer(df$child_diarrh_items.1)
  dia8a <- ifelse(is.na(dia8), NA, dia8a)
  ## What for: registration fees
  dia8b <- as.integer(df$child_diarrh_items.2)
  dia8b <- ifelse(is.na(dia8), NA, dia8b)
  ## What for: medicines
  dia8c <- as.integer(df$child_diarrh_items.3)
  dia8c <- ifelse(is.na(dia8), NA, dia8c)
  ## What for: laboratory tests
  dia8d <- as.integer(df$child_diarrh_items.4)
  dia8d <- ifelse(is.na(dia8), NA, dia8d)
  ## What for: provider fees
  dia8e <- as.integer(df$child_diarrh_items.5)
  dia8e <- ifelse(is.na(dia8), NA, dia8e)
  ## What for: gifts
  dia8f <- as.integer(df$child_diarrh_items.6)
  dia8f <- ifelse(is.na(dia8), NA, dia8f)
  ## Take loan
  dia9 <- as.integer(df$child_diarrh_loan)
  dia9 <- ifelse(dia7 == 0 | is.na(dia7), NA, dia9)
  dia9 <- bbw::recode(var = dia9, recodes = "777=0;999=0;NA=NA")
  ## Child still has diarrhoea?
  dia10 <- as.integer(df$child_diarrh_still)
  ## How long child did child take to recover?
  dia11 <- as.integer(df$child_diarrh_recover)
  dia11[as.integer(df$child_diarrh_recoverunit) == 1 & !is.na(dia11)] <- dia11[as.integer(df$child_diarrh_recoverunit) == 1 & !is.na(dia11)]
  dia11[as.integer(df$child_diarrh_recoverunit) == 2 & !is.na(dia11)] <- dia11[as.integer(df$child_diarrh_recoverunit) == 2 & !is.na(dia11)] * 7
  dia11[as.integer(df$child_diarrh_recoverunit) == 3 & !is.na(dia11)] <- dia11[as.integer(df$child_diarrh_recoverunit) == 3 & !is.na(dia11)] * (365/12)
  ## Concatenate diarrhoea data.frame
  dia <- data.frame(dia1, dia2,
                    dia3, dia3a, dia3b, dia3c, dia3d, dia3f, dia3g, dia3h, dia4,
                    dia5, dia5a, dia5b, dia5c, dia5d, dia5e, dia5f, dia5g, dia5h,
                    dia5i, dia5j, dia5k, dia5l, dia5m,
                    dia6, dia6a, dia6b, dia6c, dia6d, dia6e, dia6f, dia6g, dia6h,
                    dia6i, dia6j, dia6k, dia6l, dia6m,
                    dia7, dia8, dia8a, dia8b, dia8c, dia8d, dia8e, dia8f,
                    dia9, dia10, dia11)
  ## Return
  return(dia)
}


################################################################################
#
#' Function to recode cough/ARI prevalence and treatment-seeking behaviour for
#' the Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child health data
#'
#' @return A vector of recoded cough/ARI prevalence and treatment-seeking
#'   behaviour
#'
#' @examples
#' chealth <- create_chealth(df = childHealth, x = hh, y = hhMembers)
#' recode_ari(df = chealth)
#'
#' @export
#'
#
################################################################################

recode_ari <- function(df) {
  ##
  ill <- as.integer(df$child_ill)
  ## Had cough/ARI?
  ari1 <- as.integer(df$child_ill.2)
  ari1 <- ifelse(is.na(ill), NA, ari1)
  ## Advice or treatment for cough/ARI?
  ari2 <- as.integer(df$child_cough_treat)
  ari2 <- ifelse(is.na(ill) | is.na(ari1), NA, ari2)
  ## Reasons for no treatment
  ari3 <- as.integer(df$child_cough_notreat)
  ari3 <- ifelse(ari2 == 1, NA, ari3)
  ari3 <- ifelse(ari3 == 888, NA, ari3)
  ## Reasons for no treatment: No health facility available
  ari3a <- ifelse(ari3 == 1, 1, 0)
  ## Reasons for no treatment: Health facility too far
  ari3b <- ifelse(ari3 == 2, 1, 0)
  ## Reasons for no treatment: Health facility not accessible
  ari3c <- ifelse(ari3 == 3, 1, 0)
  ## Reasons for no treatment: Treatment expensive
  ari3d <- ifelse(ari3 == 4, 1, 0)
  ## Reasons for no treatment: treatment was no necessary
  ari3e <- ifelse(ari3 == 5, 1, 0)
  ## Reasons for no treatment: advised not to seek treatment at health facility
  ari3f <- ifelse(ari3 == 6, 1, 0)
  ## Reasons for no treatment: alternative treatment
  ari3g <- ifelse(ari3 == 7, 1, 0)
  ## Reasons for no treatment: Don't know about seeking treatment
  ari3h <- ifelse(ari3 == 8, 1, 0)
  ##
  ari4 <- as.integer(df$child_cough_notice)
  ##
  ari4[as.integer(df$child_cough_notice_unit) == 1 & !is.na(ari4)] <- ari4[as.integer(df$child_cough_notice_unit) == 1 & !is.na(ari4)]
  ari4[as.integer(df$child_cough_notice_unit) == 2 & !is.na(ari4)] <- ari4[as.integer(df$child_cough_notice_unit) == 2 & !is.na(ari4)] * 7
  ari4[as.integer(df$child_cough_notice_unit) == 3 & !is.na(ari4)] <- ari4[as.integer(df$child_cough_notice_unit) == 3 & !is.na(ari4)] * (365/12)
  ari4[is.na(ari1)] <- NA
  ari4[ari1 == 0] <- NA
  ## Where advice sought?
  ari5 <- as.integer(df$child_cough_where)
  ari5 <- ifelse(ari2 == 0, NA, ari5)
  ## Where: Township hospital
  ari5a <- ifelse(ari5 == 1, 1, 0)
  ## Where: Station hospital
  ari5b <- ifelse(ari5 == 2, 1, 0)
  ## Where: RHC/Health assistant
  ari5c <- ifelse(ari5 == 3, 1, 0)
  ## Where: SRHC/Midwife
  ari5d <- ifelse(ari5 == 4, 1, 0)
  ## Where: Private clinic/doctor
  ari5e <- ifelse(ari5 == 5, 1, 0)
  ## Where: Community health worker
  ari5f <- ifelse(ari5 == 6, 1, 0)
  ## Where: Traditional healer
  ari5g <- ifelse(ari5 == 7, 1, 0)
  ## Where: Untrained health worker
  ari5h <- ifelse(ari5 == 8, 1, 0)
  ## Where: Drug from shop
  ari5i <- ifelse(ari5 == 9, 1, 0)
  ## Where: EHO clinics/volunteers
  ari5j <- ifelse(ari5 == 10, 1, 0)
  ## Where: Family members
  ari5k <- ifelse(ari5 == 11, 1, 0)
  ## Where: NGOs/clinics
  ari5l <- ifelse(ari5 == 12, 1, 0)
  ## Where: auxilliary midwife
  ari5m <- ifelse(ari5 == 13, 1, 0)
  ## Where else advice sought?
  ari6 <- as.integer(df$child_cough_else)
  ari6 <- ifelse(ari2 == 0, NA, ari6)
  ## Where: Township hospital
  ari6a <- ifelse(ari6 == 1, 1, 0)
  ## Where: Station hospital
  ari6b <- ifelse(ari6 == 2, 1, 0)
  ## Where: RHC/Health assistant
  ari6c <- ifelse(ari6 == 3, 1, 0)
  ## Where: SRHC/Midwife
  ari6d <- ifelse(ari6 == 4, 1, 0)
  ## Where: Private clinic/doctor
  ari6e <- ifelse(ari6 == 5, 1, 0)
  ## Where: Community health worker
  ari6f <- ifelse(ari6 == 6, 1, 0)
  ## Where: Traditional healer
  ari6g <- ifelse(ari6 == 7, 1, 0)
  ## Where: Untrained health worker
  ari6h <- ifelse(ari6 == 8, 1, 0)
  ## Where: Drug from shop
  ari6i <- ifelse(ari6 == 9, 1, 0)
  ## Where: EHO clinics/volunteers
  ari6j <- ifelse(ari6 == 10, 1, 0)
  ## Where: Family members
  ari6k <- ifelse(ari6 == 11, 1, 0)
  ## Where: NGOs/clinics
  ari6l <- ifelse(ari6 == 12, 1, 0)
  ## Where: auxilliary midwife
  ari6m <- ifelse(ari6 == 13, 1, 0)
  ## Pay for advice/treatment?
  ari7 <- as.integer(df$child_cough_amount)
  ari7 <- ifelse(ari2 == 0, NA, ari7)
  ari7 <- ifelse(as.integer(df$child_cough_pay) == 0, 0, ari7)
  ari7 <- bbw::recode(var = ari7, recodes = "444=NA;666=NA;777=NA;999=NA")
  ## What are the costs for?
  ari8 <- as.integer(df$child_cough_items)
  ari8 <- ifelse(ari7 == 0 | is.na(ari7), NA, ari8)
  ## What for: transportation
  ari8a <- as.integer(df$child_cough_items.1)
  ari8a <- ifelse(is.na(ari8), NA, ari8a)
  ## What for: registration fees
  ari8b <- as.integer(df$child_cough_items.2)
  ari8b <- ifelse(is.na(ari8), NA, ari8b)
  ## What for: medicines
  ari8c <- as.integer(df$child_cough_items.3)
  ari8c <- ifelse(is.na(ari8), NA, ari8c)
  ## What for: laboratory tests
  ari8d <- as.integer(df$child_cough_items.4)
  ari8d <- ifelse(is.na(ari8), NA, ari8d)
  ## What for: provider fees
  ari8e <- as.integer(df$child_cough_items.5)
  ari8e <- ifelse(is.na(ari8), NA, ari8e)
  ## What for: gifts
  ari8f <- as.integer(df$child_cough_items.6)
  ari8f <- ifelse(is.na(ari8), NA, ari8f)
  ## Take loan
  ari9 <- as.integer(df$child_cough_loan)
  ari9 <- ifelse(ari7 == 0 | is.na(ari7), NA, ari9)
  ari9 <- bbw::recode(var = ari9, recodes = "777=0;999=0;NA=NA")
  ## Child still has cough/ARI?
  ari10 <- as.integer(df$child_cough_still)
  ## How long child did child take to recover?
  ari11 <- as.integer(df$child_cough_recover)
  ari11[as.integer(df$child_cough_recoverunit) == 1 & !is.na(ari11)] <- ari11[as.integer(df$child_cough_recoverunit) == 1 & !is.na(ari11)]
  ari11[as.integer(df$child_cough_recoverunit) == 2 & !is.na(ari11)] <- ari11[as.integer(df$child_cough_recoverunit) == 2 & !is.na(ari11)] * 7
  ari11[as.integer(df$child_cough_recoverunit) == 3 & !is.na(ari11)] <- ari11[as.integer(df$child_cough_recoverunit) == 3 & !is.na(ari11)] * (365/12)
  ## Concatenate cough/ARI data.frame
  ari <- data.frame(ari1, ari2,
                    ari3, ari3a, ari3b, ari3c, ari3d, ari3f, ari3g, ari3h, ari4,
                    ari5, ari5a, ari5b, ari5c, ari5d, ari5e, ari5f, ari5g, ari5h,
                    ari5i, ari5j, ari5k, ari5l, ari5m,
                    ari6, ari6a, ari6b, ari6c, ari6d, ari6e, ari6f, ari6g, ari6h,
                    ari6i, ari6j, ari6k, ari6l, ari6m,
                    ari7, ari8, ari8a, ari8b, ari8c, ari8d, ari8e, ari8f,
                    ari9, ari10, ari11)
  ## Return
  return(ari)
}


################################################################################
#
#' Function to recode fever prevalence and treatment-seeking behaviour for
#' the Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child health data
#'
#' @return A vector of recoded fever prevalence and treatment-seeking
#'   behaviour
#'
#' @examples
#' chealth <- create_chealth(df = childHealth, x = hh, y = hhMembers)
#' recode_fever(df = chealth)
#'
#' @export
#'
#
################################################################################

recode_fever <- function(df) {
  ##
  ill <- as.integer(df$child_ill)
  ## Had fever?
  fev1 <- as.integer(df$child_ill.3)
  fev1 <- ifelse(is.na(ill), NA, fev1)
  ## Advice or treatment for fever?
  fev2 <- as.integer(df$child_fever_treat)
  fev2 <- ifelse(is.na(ill) | is.na(fev1), NA, fev2)
  ## Reasons for no treatment
  fev3 <- as.integer(df$child_fever_notreat)
  fev3 <- ifelse(fev2 == 1, NA, fev3)
  fev3 <- ifelse(fev3 == 888, NA, fev3)
  ## Reasons for no treatment: No health facility available
  fev3a <- ifelse(fev3 == 1, 1, 0)
  ## Reasons for no treatment: Health facility too far
  fev3b <- ifelse(fev3 == 2, 1, 0)
  ## Reasons for no treatment: Health facility not accessible
  fev3c <- ifelse(fev3 == 3, 1, 0)
  ## Reasons for no treatment: Treatment expensive
  fev3d <- ifelse(fev3 == 4, 1, 0)
  ## Reasons for no treatment: treatment was no necessary
  fev3e <- ifelse(fev3 == 5, 1, 0)
  ## Reasons for no treatment: advised not to seek treatment at health facility
  fev3f <- ifelse(fev3 == 6, 1, 0)
  ## Reasons for no treatment: alternative treatment
  fev3g <- ifelse(fev3 == 7, 1, 0)
  ## Reasons for no treatment: Don't know about seeking treatment
  fev3h <- ifelse(fev3 == 8, 1, 0)
  ##
  fev4 <- as.integer(df$child_fever_notice)
  ##
  fev4[as.integer(df$child_fever_notice_unit) == 1 & !is.na(fev4)] <- fev4[as.integer(df$child_fever_notice_unit) == 1 & !is.na(fev4)]
  fev4[as.integer(df$child_fever_notice_unit) == 2 & !is.na(fev4)] <- fev4[as.integer(df$child_fever_notice_unit) == 2 & !is.na(fev4)] * 7
  fev4[as.integer(df$child_fever_notice_unit) == 3 & !is.na(fev4)] <- fev4[as.integer(df$child_fever_notice_unit) == 3 & !is.na(fev4)] * (365/12)
  fev4[is.na(fev1)] <- NA
  fev4[fev1 == 0] <- NA
  ## Where advice sought?
  fev5 <- as.integer(df$child_fever_where)
  fev5 <- ifelse(fev2 == 0, NA, fev5)
  ## Where: Township hospital
  fev5a <- ifelse(fev5 == 1, 1, 0)
  ## Where: Station hospital
  fev5b <- ifelse(fev5 == 2, 1, 0)
  ## Where: RHC/Health assistant
  fev5c <- ifelse(fev5 == 3, 1, 0)
  ## Where: SRHC/Midwife
  fev5d <- ifelse(fev5 == 4, 1, 0)
  ## Where: Private clinic/doctor
  fev5e <- ifelse(fev5 == 5, 1, 0)
  ## Where: Community health worker
  fev5f <- ifelse(fev5 == 6, 1, 0)
  ## Where: Traditional healer
  fev5g <- ifelse(fev5 == 7, 1, 0)
  ## Where: Untrained health worker
  fev5h <- ifelse(fev5 == 8, 1, 0)
  ## Where: Drug from shop
  fev5i <- ifelse(fev5 == 9, 1, 0)
  ## Where: EHO clinics/volunteers
  fev5j <- ifelse(fev5 == 10, 1, 0)
  ## Where: Family members
  fev5k <- ifelse(fev5 == 11, 1, 0)
  ## Where: NGOs/clinics
  fev5l <- ifelse(fev5 == 12, 1, 0)
  ## Where: auxilliary midwife
  fev5m <- ifelse(fev5 == 13, 1, 0)
  ## Where else advice sought?
  fev6 <- as.integer(df$child_fever_else)
  fev6 <- ifelse(fev2 == 0, NA, fev6)
  ## Where: Township hospital
  fev6a <- ifelse(fev6 == 1, 1, 0)
  ## Where: Station hospital
  fev6b <- ifelse(fev6 == 2, 1, 0)
  ## Where: RHC/Health assistant
  fev6c <- ifelse(fev6 == 3, 1, 0)
  ## Where: SRHC/Midwife
  fev6d <- ifelse(fev6 == 4, 1, 0)
  ## Where: Private clinic/doctor
  fev6e <- ifelse(fev6 == 5, 1, 0)
  ## Where: Community health worker
  fev6f <- ifelse(fev6 == 6, 1, 0)
  ## Where: Traditional healer
  fev6g <- ifelse(fev6 == 7, 1, 0)
  ## Where: Untrained health worker
  fev6h <- ifelse(fev6 == 8, 1, 0)
  ## Where: Drug from shop
  fev6i <- ifelse(fev6 == 9, 1, 0)
  ## Where: EHO clinics/volunteers
  fev6j <- ifelse(fev6 == 10, 1, 0)
  ## Where: Family members
  fev6k <- ifelse(fev6 == 11, 1, 0)
  ## Where: NGOs/clinics
  fev6l <- ifelse(fev6 == 12, 1, 0)
  ## Where: auxilliary midwife
  fev6m <- ifelse(fev6 == 13, 1, 0)
  ## Pay for advice/treatment?
  fev7 <- as.integer(df$child_fever_amount)
  fev7 <- ifelse(fev2 == 0, NA, fev7)
  fev7 <- ifelse(as.integer(df$child_fever_pay) == 0, 0, fev7)
  fev7 <- bbw::recode(var = fev7, recodes = "444=NA;666=NA;777=NA;999=NA")
  ## What are the costs for?
  fev8 <- as.integer(df$child_fever_items)
  fev8 <- ifelse(fev7 == 0 | is.na(fev7), NA, fev8)
  ## What for: transportation
  fev8a <- as.integer(df$child_fever_items.1)
  fev8a <- ifelse(is.na(fev8), NA, fev8a)
  ## What for: registration fees
  fev8b <- as.integer(df$child_fever_items.2)
  fev8b <- ifelse(is.na(fev8), NA, fev8b)
  ## What for: medicines
  fev8c <- as.integer(df$child_fever_items.3)
  fev8c <- ifelse(is.na(fev8), NA, fev8c)
  ## What for: laboratory tests
  fev8d <- as.integer(df$child_fever_items.4)
  fev8d <- ifelse(is.na(fev8), NA, fev8d)
  ## What for: provider fees
  fev8e <- as.integer(df$child_fever_items.5)
  fev8e <- ifelse(is.na(fev8), NA, fev8e)
  ## What for: gifts
  fev8f <- as.integer(df$child_fever_items.6)
  fev8f <- ifelse(is.na(fev8), NA, fev8f)
  ## Take loan
  fev9 <- as.integer(df$child_fever_loan)
  fev9 <- ifelse(fev7 == 0 | is.na(fev7), NA, fev9)
  fev9 <- bbw::recode(var = fev9, recodes = "777=0;999=0;NA=NA")
  ## Child still has fever?
  fev10 <- as.integer(df$child_fever_still)
  ## How long child did child take to recover?
  fev11 <- as.integer(df$child_fever_recover)
  fev11[as.integer(df$child_fever_recoverunit) == 1 & !is.na(fev11)] <- fev11[as.integer(df$child_fever_recoverunit) == 1 & !is.na(fev11)]
  fev11[as.integer(df$child_fever_recoverunit) == 2 & !is.na(fev11)] <- fev11[as.integer(df$child_fever_recoverunit) == 2 & !is.na(fev11)] * 7
  fev11[as.integer(df$child_fever_recoverunit) == 3 & !is.na(fev11)] <- fev11[as.integer(df$child_fever_recoverunit) == 3 & !is.na(fev11)] * (365/12)
  ## Malaria testing
  fev12 <- as.integer(df$child_fever_malaria)
  fev12 <- bbw::recode(var = fev12, recodes = "999=0;NA=NA")
  ## Concatenate fever data.frame
  fev <- data.frame(fev1, fev2,
                    fev3, fev3a, fev3b, fev3c, fev3d, fev3f, fev3g, fev3h, fev4,
                    fev5, fev5a, fev5b, fev5c, fev5d, fev5e, fev5f, fev5g, fev5h,
                    fev5i, fev5j, fev5k, fev5l, fev5m,
                    fev6, fev6a, fev6b, fev6c, fev6d, fev6e, fev6f, fev6g, fev6h,
                    fev6i, fev6j, fev6k, fev6l, fev6m,
                    fev7, fev8, fev8a, fev8b, fev8c, fev8d, fev8e, fev8f,
                    fev9, fev10, fev11, fev12)
  ## Return
  return(fev)
}


################################################################################
#
#' Function to recode child health indicators for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child health data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded child health indicators
#'
#' @examples
#' chealth <- create_chealth(df = childHealth, x = hh, y = hhMembers)
#' recode_chealth(df = chealth)
#'
#' @export
#'
#
################################################################################

recode_chealth <- function(df,
                           core.columns = c("KEY",
                                            "geo_state",
                                            "geo_rural",
                                            "geo_villward",
                                            "sample_component")) {
  ## Recode immunisation coverage indicators
  epi <- suppressWarnings(recode_epi(df = df))
  ## Recode age-appropriate immunisation coverage indicators
  ageEPI <- recode_age_epi(df = df)
  ## Recode birthweight indicators
  bwt <- recode_birthweight(df = df)
  ## Recode diarrhoea
  dia <- suppressWarnings(recode_diarrhoea(df = df))
  ## Recode cough/ARI
  ari <- suppressWarnings(recode_ari(df = df))
  ## Recode fever
  fev <- suppressWarnings(recode_fever(df = df))
  ## Concatenate indicators into a data.frame
  ch <- data.frame(df[ , core.columns], epi, ageEPI, bwt, dia, ari, fev)
  ## Return data.frame
  return(ch)
}


