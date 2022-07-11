################################################################################
#
#' Function to create raw child antropometry dataset
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child anthropometry data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing identifying information that will be linked to the
#'   child anthropometry data
#'
#' @return A data.frame containing raw child anthropometry data for the Myanmar
#'   MCCT Programme Evaluation Study
#'
#' @examples
#' create_canthro(df = childAnthro, x = anthroDF)
#'
#' @export
#'
#
################################################################################

create_canthro <- function(df, x) {
  merge(x = df, y = x, by.x = "PARENT_KEY", by.y = "KEY", all.x = TRUE)
}


################################################################################
#
#' Function to create raw mother antropometry dataset
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing mother anthropometry data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing identifying information that will be linked to the
#'   mother anthropometry data
#' @param y A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child anthropometry data to get associated information
#'   that will be linked to the mother anthropometry data
#'
#' @return A data.frame containing raw mother anthropometry data for the Myanmar
#'   MCCT Programme Evaluation Study
#'
#' @examples
#' create_manthro(df = motherAnthro, x = anthroDF,
#'                y = create_canthro(df = childAnthro, x = anthroDF))
#'
#' @export
#'
#
################################################################################

create_manthro <- function(df, x, y) {
  x <- merge(x = df, y = x, by.x = "PARENT_KEY", by.y = "KEY", all.x = TRUE)
  x <- x[ , c("PARENT_KEY", "calc_mom_post", "calc_momanthro", "calc_momage",
              "note_momanthro", "mom_anthro_confirm", "mom_anthro_note", "mom_muac")]
  x <- merge(x = x, y = y, by = "PARENT_KEY", all.x = TRUE)
  return(x)
}


################################################################################
#
#' Function to recode age variable and convert to age in days
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data
#'
#' @return A vector of recoded ages based on any re-testing done
#'
#' @examples
#'
#' recode_age(df = childAnthro)
#'
#' @export
#'
#
################################################################################

recode_age <- function(df) {
  dob <- lubridate::ymd_hms(df$cDOB)
  ##
  dob[is.na(df$cDOB2) & is.na(df$cDOB3)] <- lubridate::ymd_hms(df$cDOB[is.na(df$cDOB2) & is.na(df$cDOB3)])
  ##
  dob[!is.na(df$cDOB2)] <- lubridate::mdy(df$cDOB2[!is.na(df$cDOB2)])
  ##
  dob[df$cDOB3 != "" & !is.na(df$cDOB3)] <- lubridate::mdy(df$cDOB3[df$cDOB3 != "" & !is.na(df$cDOB3)])
  ##
  age <- vector(mode = "character", length = nrow(df))
  ## Keep NAs
  age[is.na(df$cAge)] <- NA
  ##
  age[is.na(df$cAge2) & is.na(df$cAge3)] <- df$cAge[is.na(df$cAge2) & is.na(df$cAge3)]
  ##
  age[!is.na(df$cAge2)] <- df$cAge2[!is.na(df$cAge2)]
  ##
  age[!is.na(df$cAge3)] <- df$cAge3[!is.na(df$cAge3)]
  ##
  age <- as.numeric(age)
  ## Calculated age in days
  age_in_days <- as.numeric(lubridate::date(lubridate::ymd_hms(df$enumDate)) - lubridate::date(dob))
  ##
  age_in_days[is.na(age_in_days)] <- age[is.na(age_in_days)] * 365.25 / 12
  ##
  dob[is.na(dob)] <- lubridate::ymd_hms(df$enumDate)[is.na(dob)] - age_in_days[is.na(dob)]
  ##
  ageDF <- data.frame(dob, age_in_days)
  ##
  names(ageDF) <- c("dob", "age")
  ##
  return(ageDF)
}


################################################################################
#
#' Function to recode sex variable
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data
#'
#' @return A vector of recoded sex based on any re-testing done
#'
#' @examples
#'
#' recode_sex(df = childAnthro)
#'
#' @export
#'
#
################################################################################

recode_sex <- function(df) {
  sex <- vector(mode = "numeric", length = nrow(df))
  ## Keep NAs
  sex[is.na(df$cSex)] <- NA
  ##
  sex[is.na(df$cSex2) & is.na(df$cSex3)] <- df$cSex[is.na(df$cSex2) & is.na(df$cSex3)]
  ##
  sex[!is.na(df$cSex2)] <- df$cSex2[!is.na(df$cSex2)]
  ##
  sex[!is.na(df$cSex3)] <- df$cSex3[!is.na(df$cSex3)]
  ## Recode sex to 1 = Male; 2 = Female
  sex <- bbw::recode(sex, "0=2")
  return(sex)
}


################################################################################
#
#' Function to recode weight variable
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data
#'
#' @return A vector of recoded weights based on any re-testing done
#'
#' @examples
#'
#' recode_weight(df = childAnthro)
#'
#' @export
#'
#
################################################################################

recode_weight <- function(df) {
  weight <- vector(mode = "numeric", length = nrow(df))
  ## Keep NAs
  weight[is.na(df$cWeight)] <- NA
  ##
  weight[is.na(df$cWeight2) & is.na(df$cWeight3)] <- df$cWeight[is.na(df$cWeight2) & is.na(df$cWeight3)]
  ##
  weight[!is.na(df$cWeight2)] <- df$cWeight2[!is.na(df$cWeight2)]
  ##
  weight[!is.na(df$cWeight3)] <- df$cWeight3[!is.na(df$cWeight3)]
  ##
  weight <- as.numeric(weight)
  ##
  return(weight)
}


################################################################################
#
#' Function to recode height variable
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data
#'
#' @return A vector of recoded heights based on any re-testing done
#'
#' @examples
#'
#' recode_height(df = childAnthro)
#'
#' @export
#'
#
################################################################################

recode_height <- function(df) {
  height <- vector(mode = "numeric", length = nrow(df))
  ## Keep NAs
  height[is.na(df$cHeight)] <- NA
  ##
  height[is.na(df$cHeight2) & is.na(df$cHeight3)] <- df$cHeight[is.na(df$cHeight2) & is.na(df$cHeight3)]
  ##
  height[!is.na(df$cHeight2)] <- df$cHeight2[!is.na(df$cHeight2)]
  ##
  height[!is.na(df$cHeight3)] <- df$cHeight3[!is.na(df$cHeight3)]
  ##
  height <- as.numeric(height)
  ##
  return(height)
}


################################################################################
#
#' Function to recode MUAC variable
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data
#'
#' @return A vector of recoded MUACs based on any re-testing done
#'
#' @examples
#'
#' recode_muac(df = childAnthro)
#'
#' @export
#'
#
################################################################################

recode_muac <- function(df) {
  muac <- vector(mode = "numeric", length = nrow(df))
  ## Keep NAs
  muac[is.na(df$cMuac)] <- NA
  ##
  muac[is.na(df$cMuac2) & is.na(df$cMuac3)] <- df$cMuac[is.na(df$cMuac2) & is.na(df$cMuac3)]
  ##
  muac[!is.na(df$cMuac2)] <- df$cMuac2[!is.na(df$cMuac2)]
  ##
  muac[!is.na(df$cMuac3)] <- df$cMuac3[!is.na(df$cMuac3)]
  ##
  muac <- as.numeric(muac)
  ##
  return(muac)
}


################################################################################
#
#' Function to recode oedema variable
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data
#'
#' @return A vector of recoded oedema results based on any re-testing done
#'
#' @examples
#'
#' recode_oedema(df = childAnthro)
#'
#' @export
#'
#
################################################################################

recode_oedema <- function(df) {
  oedema <- vector(mode = "numeric", length = nrow(df))
  ## Keep NAs
  oedema[is.na(df$oedema)] <- NA
  ##
  oedema[is.na(df$oedema2) & is.na(df$oedema3)] <- df$oedema[is.na(df$oedema2) & is.na(df$oedema3)]
  ##
  oedema[!is.na(df$oedema2)] <- df$oedema2[!is.na(df$oedema2)]
  ##
  oedema[!is.na(df$oedema3)] <- df$oedema3[!is.na(df$oedema3)]
  ##
  oedema <- bbw::recode(var = oedema, recodes = "99=NA")
  ##
  oedema <- as.numeric(oedema)
  ##
  return(oedema)
}


################################################################################
#
#' Function to recode position of height measurements variable
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data
#'
#' @return A vector of recoded position of height measurements based on any
#' re-testing done
#'
#' @examples
#'
#' recode_position(df = childAnthro)
#'
#' @export
#'
#
################################################################################

recode_position <- function(df) {
  position <- vector(mode = "numeric", length = nrow(df))
  ## Keep NAs
  position[is.na(df$lenHeightInit)] <- NA
  ##
  position[is.na(df$lenHeightInit2) & is.na(df$lenHeightInit3)] <- df$lenHeightInit[is.na(df$lenHeightInit2) & is.na(df$lenHeightInit3)]
  ##
  position[!is.na(df$lenHeightInit2)] <- df$lenHeightInit2[!is.na(df$lenHeightInit2)]
  ##
  position[!is.na(df$lenHeightInit3)] <- df$lenHeightInit3[!is.na(df$lenHeightInit3)]
  ##
  position <- bbw::recode(var = position, recodes = "0=2")
  ##
  return(position)
}


################################################################################
#
#' Function to recode child anthropometric indicators and calculate
#' anthropometric z-scores
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data
#' @param core.columns A vector of variable names to include in resulting
#' @param flag Logical. Should WHO flagging criteria be applied to the z-scores?
#'   Default is TRUE.
#' @param cases Logical. Should cases of child undernutrition be assessed?
#'   Default is TRUE.
#'
#' @return A data.frame of recoded anthropometric measurements and z-scores
#'
#' @examples
#' recode_anthro(df = create_canthro(df = childAnthro, x = anthroDF))
#'
#' @export
#'
#
################################################################################

recode_anthro <- function(df,
                          core.columns = c("enumDate",
                                           "geo_rural",
                                           "geo_state",
                                           "geo_villward"),
                          flag = TRUE, cases = TRUE) {
  ## Recode anthro measurements
  ageDF <- recode_age(df = df)
  sex <- recode_sex(df = df)
  muac <- recode_muac(df = df)
  height <- recode_height(df = df)
  weight <- recode_weight(df = df)
  oedema <- recode_oedema(df = df)
  position <- recode_position(df = df)

  sample_component <- ifelse(lubridate::ymd_hms(ageDF$dob) %within%
                               lubridate::interval(start = lubridate::ymd("2018/09/01"),
                                                   end = lubridate::ymd("2018/10/31")), 2, 1)

  respid <- paste(df$geo_town, df$geo_rural, df$geo_villward, df$hh_num, sep = "/")

  anthroDF <- data.frame(df[ , c(core.columns, "will_participate")], respid, sample_component,
                         ageDF, sex, muac, height, weight, oedema, position)

  ## Get z-scores weight-for-age
  anthroDF <- zscorer::addWGSR(data = anthroDF, sex = "sex",
                               firstPart = "weight", secondPart = "age",
                               index = "wfa", output = "waz")

  ## Get z-scores height-for-age
  anthroDF <- zscorer::addWGSR(data = anthroDF, sex = "sex",
                               firstPart = "height", secondPart = "age",
                               index = "hfa", standing = "position",
                               output = "haz")

  ## Get z-scores weight-for-height
  anthroDF <- zscorer::addWGSR(data = anthroDF, sex = "sex",
                               firstPart = "weight", secondPart = "height",
                               index = "wfh", standing = "position",
                               output = "whz")
  ## Convert age back to months
  anthroDF$age <- anthroDF$age / (365.25 / 12)
  ## Flag z-scores using WHO flagging criteria
  if(flag) {
    anthroDF <- nutricheckr::flag_who(df = anthroDF, hlaz = "haz", waz = "waz", whlz = "whz")
  }
  ## Find undernutrition cases
  if(cases) {
    anthroDF <- nutricheckr::find_child_stunting(df = anthroDF, flag = "flag")
    anthroDF <- nutricheckr::find_child_underweight(df = anthroDF, flag = "flag")
    anthroDF <- nutricheckr::find_child_wasting(df = anthroDF, flag = "flag")
  }
  ##
  return(anthroDF)
}


################################################################################
#
#' Function to recode maternal MUAC indicators
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing maternal mid-upper arm circumference data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing identifying information that will be linked to the
#'   mother anthropometry data
#' @param y A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing child anthropometry data to get associated information
#'   that will be linked to the mother anthropometry data
#' @param core.columns A vector of variable names to include in resulting
#'
#' @return A data.frame of recoded MUAC measurements
#'
#' @examples
#'
#' recode_maternal_anthro(df = motherAnthro, x = anthroDF, y = childAnthro)
#'
#' @export
#'
#
################################################################################

recode_maternal_anthro <- function(df, x, y,
                                   core.columns = c("dob",
                                                    "geo_state",
                                                    "geo_rural",
                                                    "geo_villward")) {
  canthro <- create_canthro(df = y, x = x)
  canthro <- data.frame(canthro, recode_age(df = canthro))
  manthro <- create_manthro(df = df, x = x, y = canthro)

  sample_component <- ifelse(lubridate::ymd_hms(manthro$dob) %within%
                               lubridate::interval(start = lubridate::ymd("2018/09/01"),
                                                   end = lubridate::ymd("2018/10/31")), 2, 1)

  respid <- paste(manthro$geo_town,
                  manthro$geo_rural,
                  manthro$geo_villward,
                  manthro$hh_num,
                  sep = "/")

  ## MUAC in cms
  muac <- as.integer(manthro$mom_muac)
  ## GAM (MUAC < 21.0 cms)
  gam <- bbw::recode(var = muac * 10, recodes = "lo:209=1;NA=NA;else=0")
  ## MAM (MUAC >=18.5 & < 21)
  mam <- bbw::recode(var = muac * 10, recodes = "185:209=1;NA=NA;else=0")
  ## SAM (MUAC < 18.5)
  sam <- bbw::recode(var = muac * 10, recodes = "lo:184=1;NA=NA;else=0")
  ## Concatenate
  mAnthro <- data.frame(manthro[ , c(core.columns, "will_participate")],
                        respid, sample_component, muac, gam, mam, sam)
  ## Remove those not willing to participate
  mAnthro <- mAnthro[manthro$will_participate == "1", ]
  ## Return data.frame
  return(mAnthro)
}
