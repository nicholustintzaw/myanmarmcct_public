################################################################################
#
#' Function to create poverty probability index dataset for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing poverty-related household data
#'
#' @return A data.frame of poverty probability index dataset
#'
#' @examples
#' ## Create poverty probability index dataset
#' create_ppi(df = hh)
#'
#' @export
#'
#'
#
################################################################################

create_ppi <- function(df) {
  ##
  p1 <- df[ , c("KEY", "geo_rural", "geo_state", "sample_component",
                "house_electric", "water_rain", "house_roof", "house_wall",
                "house_cooking", "hhbeef_freq")]
  p1 <- p1[!duplicated(p1$KEY), ]
  ## Get ages
  z <- merge(df[ , c("KEY", "intrv_date")],
             hhMembers[ , c("PARENT_KEY", "hh_mem_dob", "hh_mem_age")],
             by.x = "KEY",
             by.y = "PARENT_KEY",
             all.x = TRUE)
  age <- floor(as.numeric(lubridate::mdy(z$intrv_date) - lubridate::mdy(z$hh_mem_dob)) / 365)
  age <- ifelse(is.na(age), as.integer(z$hh_mem_age), age)
  nU5 <- ifelse(age <= 4, 1, 0)
  n5to9 <- ifelse(age %in% 5:9, 1, 0)
  x <- data.frame(hhid = z$KEY, age, nU5, n5to9)
  y <- aggregate(cbind(nU5, n5to9) ~ hhid, data = x, FUN = sum)
  y <- y[!duplicated(y$hhid), ]
  y <- merge(df[ , c("KEY", "geo_state")],
             y,
             by.x = "KEY",
             by.y = "hhid", all.x = TRUE)
  y <- y[ , c("KEY", "nU5", "n5to9")]
  ## Get education of of head of household
  x <- hhMembers[hhMembers$hh_mem_relation == "1", ]
  x <- x[!duplicated(x$PARENT_KEY), ]
  x <- merge(df[ , c("KEY", "geo_state")],
             x,
             by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
  x <- x[ , c("KEY", "hh_mem_highedu")]
  x <- x[!duplicated(x$KEY), ]
  ## concatenate dataset
  ppiDF <- merge(p1, y, by = "KEY", all.x = TRUE)
  ppiDF <- merge(ppiDF, x, by = "KEY", all.x = TRUE)
  ## return
  return(ppiDF)
}


################################################################################
#
#' Function to recode poverty probability index indicators for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing poverty-related household data
#' @param core.columns A vector of variable names to include in resulting
#'   data.frame
#'
#' @return A data.frame of recoded poverty probability index indicators based on
#'   the national poverty line
#'
#' @examples
#' ## Recode poverty probability index indicators
#' recode_ppi(df = hh)
#'
#'
#' @export
#'
#
################################################################################

recode_ppi <- function(df,
                       core.columns = c("geo_rural",
                                        "geo_state",
                                        "sample_component")) {
  ##
  ppiDF <- create_ppi(df = df)
  ## ppi1: In what agricultural zone does HH live? Hills and mountains
  ppi1 <- ifelse(ppiDF$geo_state == "MMR002" | ppiDF$geo_state == "MMR003", 0, NA)
  ## ppi2: number of members between 0-4 years
  ppi2 <- bbw::recode(var = ppiDF$nU5,
                      recodes = "2:hi=0;1=8;0=14")
  ## ppi3: number of members between 5 to 9 years
  ppi3 <- bbw::recode(var = ppiDF$n5to9,
                      recodes = "2:hi=0;1=8;0=12")
  ## ppi4: electricity in the past 12 months
  ppi4 <- bbw::recode(var = as.integer(ppiDF$house_electric),
                      recodes = "1=9;else=0")
  ## ppi5: source of water in wet season
  ppi5 <- as.integer(stringr::str_detect(string = ppiDF$water_rain,
                                         pattern = "1|5|12"))
  ppi5 <- bbw::recode(var = ppi5, recodes = "1=11;else=0")
  ## ppi6: roof material
  ppi6 <- as.integer(stringr::str_detect(string = ppiDF$house_roof,
                                         pattern = "3"))
  ppi6 <- bbw::recode(var = ppi6, recodes = "1=0;NA=0;else=7")
  ## ppi7: wall material
  ppi7 <- as.integer(stringr::str_detect(string = ppiDF$house_wall,
                                         pattern = "3"))
  ppi7 <- bbw::recode(var = ppi7, recodes = "1=0;NA=0;else=9")
  ## ppi8: electric stove
  ppi8 <- as.integer(stringr::str_detect(string = ppiDF$house_cooking,
                                         pattern = "4"))
  ppi8 <- bbw::recode(var = ppi8, recodes = "1=8;else=0")
  ## ppi9: education household head
  ppi9 <- as.integer(ppiDF$hh_mem_highedu)
  ppi9 <- bbw::recode(var = ppi9, recodes = "3=7;4=12;6=12;else=0")
  ## ppi10: consume meat
  ppi10 <- as.integer(ppiDF$hhbeef_freq)
  ppi10 <- bbw::recode(var = ppi10, recodes = "1:hi=11;else=0")
  ## Sum PPI
  ppiScore <- ppi1 + ppi2 + ppi3 + ppi4 + ppi5 +
    ppi6 + ppi7 + ppi8 + ppi9 + ppi10
  ## get probabilities
  ppi <- ppitables::ppiMMR2019[ppiScore + 1, "nl100"]
  ## concatenate ppi indicators
  ppi <- data.frame(KEY = ppiDF[ , "KEY"], df[ , core.columns],
                    ppiScore, ppi)
  ## return data.frame
  return(ppi)
}
