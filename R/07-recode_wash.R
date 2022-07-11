################################################################################
#
#' Function to recode water indicators for WASH for the Myanmar MCCT Programme
#' Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing water, sanitation and hygiene data
#'
#' @return A vector of recoded water indicators
#'
#' @examples
#' recode_water(df = hh)
#'
#' @export
#'
#
################################################################################

recode_water <- function(df) {
  ## Water access during summertime
  summer1 <- as.integer(df$water_sum)
  summer1 <- bbw::recode(var = summer1,
                         recodes = "1:7=1;8=0;9=1;10=0;11:12=1;13=0;888=0")
  ## Treat water?
  summer2 <- as.integer(df$water_sum_treat)
  ## Treatment method
  summer3 <- as.integer(df$water_sum_treatmethod)
  ## Treatment method: Boil
  summer3a <- as.integer(df$water_rain_treatmethod.1)
  summer3a <- ifelse(is.na(summer3), NA, summer3a)
  ## Treatment method: bleach/chlorine
  summer3b <- as.integer(df$water_rain_treatmethod.2)
  summer3b <- ifelse(is.na(summer3), NA, summer3b)
  ## Treatment method: iodine
  summer3c <- as.integer(df$water_rain_treatmethod.3)
  summer3c <- ifelse(is.na(summer3), NA, summer3c)
  ## Treatment method: strain through cloth
  summer3d <- as.integer(df$water_rain_treatmethod.4)
  summer3d <- ifelse(is.na(summer3), NA, summer3d)
  ## Treatment method: water filter
  summer3e <- as.integer(df$water_rain_treatmethod.5)
  summer3e <- ifelse(is.na(summer3), NA, summer3e)
  ## Treatment method: composite filter
  summer3f <- as.integer(df$water_rain_treatmethod.6)
  summer3f <- ifelse(is.na(summer3), NA, summer3f)
  ## Treatment method: solar disinfection
  summer3g <- as.integer(df$water_rain_treatmethod.7)
  summer3g <- ifelse(is.na(summer3), NA, summer3g)
  ## Treatment method: stand and settle
  summer3h <- as.integer(df$water_rain_treatmethod.8)
  summer3h <- ifelse(is.na(summer3), NA, summer3h)
  ## Treatment method: Other
  summer3i <- as.integer(df$water_rain_treatmethod.888)
  summer3i <- ifelse(is.na(summer3), NA, summer3i)
  ## Probable safe drinking water
  summer4 <- summer1
  summer4[summer4 == 1] <- 1
  summer4[summer4 == 0 & (summer3a == 1 | summer3b == 1 | summer3c == 1)] <- 1
  summer4[summer4 == 0 & summer3a == 0 & summer3b == 0 & summer3c == 0] <- 0
  ## Surface water
  summer5 <- df$water_sum
  summer5 <- bbw::recode(var = summer5, recodes = "13=1;NA=NA;else=0")
  ## Unimproved
  summer6 <- df$water_sum
  summer6 <- bbw::recode(var = summer6, recodes = "8=1;10=1;888=1;NA=NA;else=0")
  ## Water access during rainy season
  rain1 <- as.integer(df$water_rain)
  rain1 <- bbw::recode(var = rain1,
                       recodes = "1:7=1;8=0;9=1;10=0;11:12=1;13=0;888=0")
  ## Treat water?
  rain2 <- as.integer(df$water_rain_treat)
  ## Treatment method
  rain3 <- as.integer(df$water_rain_treatmethod)
  ## Treatment method: Boil
  rain3a <- as.integer(df$water_rain_treatmethod.1)
  rain3a <- ifelse(is.na(rain3), NA, rain3a)
  ## Treatment method: bleach/chlorine
  rain3b <- as.integer(df$water_rain_treatmethod.2)
  rain3b <- ifelse(is.na(rain3), NA, rain3b)
  ## Treatment method: iodine
  rain3c <- as.integer(df$water_rain_treatmethod.3)
  rain3c <- ifelse(is.na(rain3), NA, rain3c)
  ## Treatment method: strain through cloth
  rain3d <- as.integer(df$water_rain_treatmethod.4)
  rain3d <- ifelse(is.na(rain3), NA, rain3d)
  ## Treatment method: water filter
  rain3e <- as.integer(df$water_rain_treatmethod.5)
  rain3e <- ifelse(is.na(rain3), NA, rain3e)
  ## Treatment method: composite filter
  rain3f <- as.integer(df$water_rain_treatmethod.6)
  rain3f <- ifelse(is.na(rain3), NA, rain3f)
  ## Treatment method: solar disinfection
  rain3g <- as.integer(df$water_rain_treatmethod.7)
  rain3g <- ifelse(is.na(rain3), NA, rain3g)
  ## Treatment method: stand and settle
  rain3h <- as.integer(df$water_rain_treatmethod.8)
  rain3h <- ifelse(is.na(rain3), NA, rain3h)
  ## Treatment method: Other
  rain3i <- as.integer(df$water_rain_treatmethod.888)
  rain3i <- ifelse(is.na(rain3), NA, rain3i)
  ## Probable safe drinking water
  rain4 <- rain1
  rain4[rain4 == 1] <- 1
  rain4[rain4 == 0 & (rain3a == 1 | rain3b == 1 | rain3c == 1)] <- 1
  rain4[rain4 == 0 & rain3a == 0 & rain3b == 0 & rain3c == 0] <- 0
  ## Surface water
  rain5 <- df$water_rain
  rain5 <- bbw::recode(var = rain5, recodes = "13=1;NA=NA;else=0")
  ## Unimproved
  rain6 <- df$water_rain
  rain6 <- bbw::recode(var = rain6, recodes = "8=1;10=1;888=1;NA=NA;else=0")
  ## Water access during winter season
  winter1 <- as.integer(df$water_winter)
  winter1 <- bbw::recode(var = winter1,
                         recodes = "1:7=1;8=0;9=1;10=0;11:12=1;13=0;888=0")
  ## Treat water?
  winter2 <- as.integer(df$water_winter_treat)
  ## Treatment method
  winter3 <- as.integer(df$water_winter_treatmethod)
  ## Treatment method: Boil
  winter3a <- as.integer(df$water_winter_treatmethod.1)
  winter3a <- ifelse(is.na(winter3), NA, winter3a)
  ## Treatment method: bleach/chlorine
  winter3b <- as.integer(df$water_winter_treatmethod.2)
  winter3b <- ifelse(is.na(winter3), NA, winter3b)
  ## Treatment method: iodine
  winter3c <- as.integer(df$water_winter_treatmethod.3)
  winter3c <- ifelse(is.na(winter3), NA, winter3c)
  ## Treatment method: stwinter through cloth
  winter3d <- as.integer(df$water_winter_treatmethod.4)
  winter3d <- ifelse(is.na(winter3), NA, winter3d)
  ## Treatment method: water filter
  winter3e <- as.integer(df$water_winter_treatmethod.5)
  winter3e <- ifelse(is.na(winter3), NA, winter3e)
  ## Treatment method: composite filter
  winter3f <- as.integer(df$water_winter_treatmethod.6)
  winter3f <- ifelse(is.na(winter3), NA, winter3f)
  ## Treatment method: solar disinfection
  winter3g <- as.integer(df$water_winter_treatmethod.7)
  winter3g <- ifelse(is.na(winter3), NA, winter3g)
  ## Treatment method: stand and settle
  winter3h <- as.integer(df$water_winter_treatmethod.8)
  winter3h <- ifelse(is.na(winter3), NA, winter3h)
  ## Treatment method: Other
  winter3i <- as.integer(df$water_winter_treatmethod.888)
  winter3i <- ifelse(is.na(winter3), NA, winter3i)
  ## Probable safe drinking water
  winter4 <- winter1
  winter4[winter4 == 1] <- 1
  winter4[winter4 == 0 & (winter3a == 1 | winter3b == 1 | winter3c == 1)] <- 1
  winter4[winter4 == 0 & winter3a == 0 & winter3b == 0 & winter3c == 0] <- 0
  ## Surface water
  winter5 <- df$water_winter
  winter5 <- bbw::recode(var = winter5, recodes = "13=1;NA=NA;else=0")
  ## Unimproved
  winter6 <- df$water_winter
  winter6 <- bbw::recode(var = winter6, recodes = "8=1;10=1;888=1;NA=NA;else=0")
  ## Water pot
  waterpot1 <- as.integer(df$waterpot_yn)
  ## Water pot capacity
  waterpot2 <- as.integer(df$waterpot_capacity)
  ## Water pot capacity: < 100 litres
  waterpot2a <- bbw::recode(var = waterpot2, recodes = "1=1;else=0")
  ## Water pot capacity: 100-200 litres
  waterpot2b <- bbw::recode(var = waterpot2, recodes = "2=1;else=0")
  ## Water pot capacity: >200 litres
  waterpot2c <- bbw::recode(var = waterpot2, recodes = "3=1;else=0")
  ## Waterpot condition
  waterpot3 <- as.integer(df$waterpot_condition)
  ## Waterpot condition: clean
  waterpot3a <- as.integer(df$waterpot_condition.1)
  waterpot3a <- ifelse(is.na(waterpot3), NA, waterpot3a)
  ## Waterpot condition: covered
  waterpot3b <- as.integer(df$waterpot_condition.2)
  waterpot3b <- ifelse(is.na(waterpot3), NA, waterpot3b)
  ## Waterpot condition: with cup and handle
  waterpot3c <- as.integer(df$waterpot_condition.3)
  waterpot3c <- ifelse(is.na(waterpot3), NA, waterpot3c)
  ## Waterpot condition: meet all of above
  waterpot3d <- as.integer(df$waterpot_condition.4)
  waterpot3d <- ifelse(is.na(waterpot3), NA, waterpot3d)
  waterpot3d[waterpot3d == 1] <- 0
  waterpot3d[waterpot3a == 1 & waterpot3b == 1 & waterpot3c == 1] <- 1
  waterpot3d[waterpot3a == 0 | waterpot3b == 0 | waterpot3c == 1] <- 0
  ## Concatenate water indicators
  water <- data.frame(summer1, summer2,
                      summer3, summer3a, summer3b, summer3c, summer3d, summer3e,
                      summer3f, summer3g, summer3h, summer3i, summer4,
                      summer5, summer6,
                      rain1, rain2,
                      rain3, rain3a, rain3b, rain3c, rain3d, rain3e,
                      rain3f, rain3g, rain3h, rain3i, rain4, rain5, rain6,
                      winter1, winter2,
                      winter3, winter3a, winter3b, winter3c, winter3d, winter3e,
                      winter3f, winter3g, winter3h, winter3i, winter4,
                      winter5, winter6,
                      waterpot1, waterpot2, waterpot2a, waterpot2b, waterpot2c,
                      waterpot3, waterpot3a, waterpot3b, waterpot3c, waterpot3d)
  ## Return
  return(water)
}


################################################################################
#
#' Function to recode sanitation indicators for WASH for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing water, sanitation and hygiene data
#'
#' @return A vector of recoded sanitation indicators
#'
#' @examples
#' recode_sanitation(df = hh)
#'
#' @export
#'
#
################################################################################

recode_sanitation <- function(df) {
  ## Type of latrine
  san1 <- as.integer(df$latrine_type)
  ## Type: water flush with septic tank
  san1a <- bbw::recode(var = san1, recodes = "1=1;NA=NA;else=0")
  ## Type: water flush without septic tank but with water seal
  san1b <- bbw::recode(var = san1, recodes = "2=1;NA=NA;else=0")
  ## Type: pit latrine fly proof (with slab)
  san1c <- bbw::recode(var = san1, recodes = "3=1;NA=NA;else=0")
  ## Type: pit latrine not fly proof (without slab)
  san1d <- bbw::recode(var = san1, recodes = "4=1;NA=NA;else=0")
  ## Type: hanging latrine
  san1e <- bbw::recode(var = san1, recodes = "5=1;NA=NA;else=0")
  ## Type: open defecation
  san1f <- bbw::recode(var = san1, recodes = "6=1;NA=NA;else=0")
  ## Is toilet shared?
  san2 <- as.integer(df$latrine_share)
  ## Is toilet improved and not shared? - basic
  san3 <- san1
  san3 <- bbw::recode(var = san3, recodes = "1:3=1;NA=NA;else=0")
  san3 <- ifelse(san3 == 1 & san2 == 1, 1, 0)
  ## Is toilet improved and not shared (basic)? - observed
  observed <- as.integer(df$latrine_observe)
  san3a <- ifelse(observed == 0, NA, san3)
  ## Is toilet improved but shared? - limited
  san4 <- san1
  san4 <- bbw::recode(var = san4, recodes = "1:3=1;NA=NA;else=0")
  san4 <- ifelse(san4 == 1 & san2 == 0, 1, 0)
  ## Is toilet improved but shared (limited)? - observed
  san4a <- ifelse(observed == 0, NA, san4)
  ## Is toilet unimproved?
  san5 <- san1
  san5 <- bbw::recode(var = san5, recodes = "4:5=1;NA=NA;else=0")
  ## Is toilet unimproved? - observed
  san5a <- ifelse(observed == 0, NA, san5)
  ## Open defecation
  san6 <- san1f
  ## Concatenate sanitation indicators
  san <- data.frame(san1, san1a, san1b, san1c, san1d, san1e,
                    san1f, san2, san3, san3a, san4, san4a, san5, san5a, san6)
  ## Return data.frame
  return(san)
}


################################################################################
#
#' Function to recode handwashing indicators for WASH for the Myanmar MCCT
#' Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing water, sanitation and hygiene data
#'
#' @return A vector of recoded handwashing indicators
#'
#' @examples
#' recode_handwashing(df = hh)
#'
#' @export
#'
#
################################################################################

recode_handwashing <- function(df) {
  ## Use handsoap
  hw1 <- as.integer(df$soap_yn)
  ## Why not use soap
  hw2 <- as.integer(df$soap_why)
  ## Why: financial difficulty to buy soap
  hw2a <- bbw::recode(var = hw2, recodes = "1=1;NA=NA;666=NA;999=NA;else=0")
  ## Why: not available
  hw2b <- bbw::recode(var = hw2, recodes = "2=1;NA=NA;666=NA;999=NA;else=0")
  ## Why: not important
  hw2c <- bbw::recode(var = hw2, recodes = "3=1;NA=NA;666=NA;999=NA;else=0")
  ## Why: other
  hw2d <- bbw::recode(var = hw2, recodes = "888=1;NA=NA;666=NA;999=NA;else=0")
  ## Wash hands with soap after using toilet
  hw3 <- as.integer(df$soap_tiolet)
  ## Wash hands with soap after using toilet: never
  hw3a <- bbw::recode(var = hw3, recodes = "1=1;NA=NA;else=0")
  ## Wash hands with soap after using toilet: sometimes
  hw3b <- bbw::recode(var = hw3, recodes = "2=1;NA=NA;else=0")
  ## Wash hands with soap after using toilet: often
  hw3c <- bbw::recode(var = hw3, recodes = "3=1;NA=NA;else=0")
  ## Wash hands with soap after using toilet: always
  hw3d <- bbw::recode(var = hw3, recodes = "4=1;NA=NA;else=0")
  ## Wash hands with soap before eating
  hw4 <- as.integer(df$soap_before_eat)
  ## Wash hands with soap before eating: never
  hw4a <- bbw::recode(var = hw4, recodes = "1=1;NA=NA;else=0")
  ## Wash hands with soap before eating: sometimes
  hw4b <- bbw::recode(var = hw4, recodes = "2=1;NA=NA;else=0")
  ## Wash hands with soap before eating: often
  hw4c <- bbw::recode(var = hw4, recodes = "3=1;NA=NA;else=0")
  ## Wash hands with soap before eating: always
  hw4d <- bbw::recode(var = hw4, recodes = "4=1;NA=NA;else=0")
  ## Wash hands with soap after eating
  hw5 <- as.integer(df$soap_after_eat)
  ## Wash hands with soap after eating: never
  hw5a <- bbw::recode(var = hw5, recodes = "1=1;NA=NA;else=0")
  ## Wash hands with soap after eating: sometimes
  hw5b <- bbw::recode(var = hw5, recodes = "2=1;NA=NA;else=0")
  ## Wash hands with soap after eating: often
  hw5c <- bbw::recode(var = hw5, recodes = "3=1;NA=NA;else=0")
  ## Wash hands with soap after eating: always
  hw5d <- bbw::recode(var = hw5, recodes = "4=1;NA=NA;else=0")
  ## Wash hands with soap before/after handling children
  hw6 <- as.integer(df$soap_handle_child)
  ## Wash hands with soap before/after handling children: never
  hw6a <- bbw::recode(var = hw6, recodes = "1=1;NA=NA;else=0")
  ## Wash hands with soap before/after handling children: sometimes
  hw6b <- bbw::recode(var = hw6, recodes = "2=1;NA=NA;else=0")
  ## Wash hands with soap before/after handling children: often
  hw6c <- bbw::recode(var = hw6, recodes = "3=1;NA=NA;else=0")
  ## Wash hands with soap before/after handling children: always
  hw6d <- bbw::recode(var = hw6, recodes = "4=1;NA=NA;else=0")
  ## Wash hands with soap before cooking/preparing food
  hw7 <- as.integer(df$soap_before_cook)
  ## Wash hands with soap before cooking/preparing food: never
  hw7a <- bbw::recode(var = hw7, recodes = "1=1;NA=NA;else=0")
  ## Wash hands with soap before cooking/preparing food: sometimes
  hw7b <- bbw::recode(var = hw7, recodes = "2=1;NA=NA;else=0")
  ## Wash hands with soap before cooking/preparing food: often
  hw7c <- bbw::recode(var = hw7, recodes = "3=1;NA=NA;else=0")
  ## Wash hands with soap before cooking/preparing food: always
  hw7d <- bbw::recode(var = hw7, recodes = "4=1;NA=NA;else=0")
  ## Wash hands with soap before feeding children
  hw8 <- as.integer(df$soap_feed_child)
  ## Wash hands with soap before feeding children: never
  hw8a <- bbw::recode(var = hw8, recodes = "1=1;NA=NA;else=0")
  ## Wash hands with soap before feeding children: sometimes
  hw8b <- bbw::recode(var = hw8, recodes = "2=1;NA=NA;else=0")
  ## Wash hands with soap before feeding children: often
  hw8c <- bbw::recode(var = hw8, recodes = "3=1;NA=NA;else=0")
  ## Wash hands with soap before feeding children: always
  hw8d <- bbw::recode(var = hw8, recodes = "4=1;NA=NA;else=0")
  ## Wash hands with soap after cleaning baby
  hw9 <- as.integer(df$soap_clean_baby)
  ## Wash hands with soap after cleaning baby: never
  hw9a <- bbw::recode(var = hw9, recodes = "1=1;NA=NA;else=0")
  ## Wash hands with soap after cleaning baby: sometimes
  hw9b <- bbw::recode(var = hw9, recodes = "2=1;NA=NA;else=0")
  ## Wash hands with soap after cleaning baby: often
  hw9c <- bbw::recode(var = hw9, recodes = "3=1;NA=NA;else=0")
  ## Wash hands with soap after cleaning baby: always
  hw9d <- bbw::recode(var = hw9, recodes = "4=1;NA=NA;else=0")
  ## Wash hands with soap after disposing child's faeces
  hw10 <- as.integer(df$soap_child_faeces)
  ## Wash hands with soap after disposing child's faeces: never
  hw10a <- bbw::recode(var = hw10, recodes = "1=1;NA=NA;else=0")
  ## Wash hands with soap after disposing child's faeces: sometimes
  hw10b <- bbw::recode(var = hw10, recodes = "2=1;NA=NA;else=0")
  ## Wash hands with soap after disposing child's faeces: often
  hw10c <- bbw::recode(var = hw10, recodes = "3=1;NA=NA;else=0")
  ## Wash hands with soap after disposing child's faeces: always
  hw10d <- bbw::recode(var = hw10, recodes = "4=1;NA=NA;else=0")
  ## Observe handwashing facility
  hw11 <- as.integer(df$observ_washplace)
  ## Not observed
  hw11a <- as.integer(df$observ_washplace.0)
  hw11a <- ifelse(is.na(hw11), NA, hw11a)
  ## Observed in dwelling
  hw11b <- as.integer(df$observ_washplace.1)
  hw11b <- ifelse(is.na(hw11), NA, hw11b)
  ## Observed in yard/plot
  hw11c <- as.integer(df$observ_washplace.2)
  hw11c <- ifelse(is.na(hw11), NA, hw11c)
  ## Observed mobile facility
  hw11d <- as.integer(df$observ_washplace.3)
  hw11d <- ifelse(is.na(hw11), NA, hw11d)
  ## No handwashing facility
  hw11e <- as.integer(df$observ_washplace.4)
  hw11e <- ifelse(is.na(hw11), NA, hw11e)
  ## Water in handwashing facility?
  hw12 <- as.integer(df$observ_water)
  hw12 <- bbw::recode(var = hw12, recodes = "1=1;NA=NA;else=0")
  ## Soap present in handwashing facility
  hw13 <- as.integer(df$soap_present)
  hw13 <- bbw::recode(var = hw13, recodes = "1=1;NA=NA;else=0")
  ## Handwashing facility with soap and water
  hw14 <- as.numeric(stringr::str_detect(string = df$observ_washplace,
                                         pattern = "1|2|3"))
  hw14[stringr::str_detect(string = df$observ_washplace, pattern = "4")] <- 0
  hw14 <- ifelse(hw14 == 1 & hw12 == 1 & hw13 == 1, 1, 0)
  ## handwashing facility without soap and water
  hw15 <- as.numeric(stringr::str_detect(string = df$observ_washplace,
                                         pattern = "1|2|3"))
  hw15[stringr::str_detect(string = df$observ_washplace, pattern = "4")] <- 0
  hw15 <- ifelse(hw15 == 1 & hw12 == 0 & hw13 == 0, 1, 0)
  ## No handwashing facility
  hw16 <- as.numeric(stringr::str_detect(string = df$observ_washplace,
                                         pattern = "4|888"))
  hw16[hw14 == 1] <- 0
  ## Appropriate handwashing behaviour
  hw17 <- ifelse(hw3d == 1 & hw4d == 1 & hw5d == 1 & hw6d == 1 & hw7d == 1 &
                   hw8d == 1 & hw9d == 1 & hw10d == 1 & hw16 == 1, 1, 0)
  ## Concatenate handwashing indicators
  handwash <- data.frame(hw1,
                         hw2, hw2a, hw2b, hw2c, hw2d,
                         hw3, hw3a, hw3b, hw3c, hw3d,
                         hw4, hw4a, hw4b, hw4c, hw4d,
                         hw5, hw5a, hw5b, hw5c, hw5d,
                         hw6, hw6a, hw6b, hw6c, hw6d,
                         hw7, hw7a, hw7b, hw7c, hw7d,
                         hw8, hw8a, hw8b, hw8c, hw8d,
                         hw9, hw9a, hw9b, hw9c, hw9d,
                         hw10, hw10a, hw10b, hw10c, hw10d,
                         hw11, hw11a, hw11b, hw11c, hw11d, hw11e,
                         hw12, hw13, hw14, hw15, hw16, hw17)
  ## Return data.frame
  return(handwash)
}



################################################################################
#
#' Function to recode water, sanitation and hygiene (WASH) indicators for the
#' Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing water, sanitation and hygiene data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A vector of recoded water, sanitation and hygiene indicators
#'
#' @examples
#' recode_wash(df = hh)
#'
#' @export
#'
#
################################################################################

recode_wash <- function(df,
                        core.columns = c("KEY",
                                         "geo_state",
                                         "geo_rural",
                                         "geo_villward",
                                         "sample_component")) {
  ## Water indicators
  water <- suppressWarnings(recode_water(df = df))
  ## Sanitation indicators
  sanitation <- suppressWarnings(recode_sanitation(df = df))
  ## Handwashing indicators
  handwash <- suppressWarnings(recode_handwashing(df = df))
  ## Concatenate all WASH indicators
  wash <- data.frame(df[ , core.columns], water, sanitation, handwash)
  ## Return WASH data.frame
  return(wash)
}
