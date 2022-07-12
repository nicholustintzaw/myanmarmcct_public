################################################################################
#
#' Function to create raw ANC dataset
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing ANC data
#' @param x A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household information that will be linked to the
#'   ANC data
#' @param y A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household roster information that will be linked to the
#'   ANC data
#' @param status A character value indicating whether to create data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame containing raw ANC data for the Myanmar MCCT Programme
#'   Evaluation Study
#'
#' @examples
#' create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#'
#' @export
#'
#
################################################################################

create_anc <- function(df, x, y, status) {
  ## Create data.frame from hh dataset to get needed information
  x <- x[ , c("KEY", "intrv_date", "geo_state", "geo_rural", "geo_villward",
              "sample_component", "will_participate")]

  ## Create data.frame from hhMembers dataset to get needed information
  y <- y[ , c("PARENT_KEY", "KEY", "hh_mem_sex", "hh_mem_age",
              "hh_mem_age_month", "hh_mem_dob", "hh_mem_pregnow",
              "hh_mem_u5num", "hh_mem_highedu", "hh_mem_occup")]

  if(status == "current") {
    ## change KEY to match anc1 KEY
    y$KEY <- stringr::str_replace_all(string = y$KEY,
                                      pattern = "grp_hh",
                                      replacement = "ancnow_rep")
  }

  if(status == "past") {
    ## change KEY to match anc2 KEY
    y$KEY <- stringr::str_replace_all(string = y$KEY,
                                      pattern = "grp_hh",
                                      replacement = "ancpast_rep")
  }

  ## Merge hhMembers data with childHealth to get hhMember information
  xy <- merge(y, df, by.x = "KEY", by.y = "KEY", all.y = TRUE)

  ## Merge hh data with merged childHealth data to get hh information
  z <- merge(x, xy, by.x = "KEY", by.y = "PARENT_KEY.y", all.y = TRUE)

  ## Calculate age in days
  age <- floor(as.numeric(lubridate::mdy(z$intrv_date) -
                            lubridate::mdy(z$hh_mem_dob)) / 365.25)
  age <- ifelse(is.na(age),
                floor(sum(as.integer(z$hh_mem_age),
                          (as.integer(z$hh_mem_age_month) / 12),
                          na.rm = TRUE)),
                age)

  ## Add age to data.frame
  z$age <- age

  if(status == "current") {
    z <- z[as.integer(z$hh_mem_pregnow) == 1 & !is.na(z$hh_mem_pregnow), ]
  }

  if(status == "past") {
    z <- z[as.integer(z$hh_mem_pregnow) == 0 &
             !is.na(z$hh_mem_pregnow) &
             as.integer(z$hh_mem_u5num) > 0 & !is.na(z$hh_mem_u5num), ]
  }

  ## remove unnecessary variables
  # z <- z[ , names(z)[!names(z) %in% c("KEY.y",
  #                                     "PARENT_KEY.x",
  #                                     "SET.OF.ancnow_rep",
  #                                     "SET.OF.ancpast_rep")]]

  ## return data.frame
  return(z)
}


################################################################################
#
#' Function to recode the specialist ANC provider data for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded specialist ANC provider information
#'
#' @examples
#' ## Recode specialist provider for current pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_specialist(df = x, status = "current")
#'
#' ## Recode specialist provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_specialist(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_specialist <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend specialist
    specialist <- as.integer(df$ancnow_who.1)
    ## Where specialist
    specialist.where <- as.integer(df$ancnow_pdoc_where)
    specialist.home <- bbw::recode(var = specialist.where,
                                   recodes = "1=1;NA=NA;else=0")
    specialist.gov <- bbw::recode(var = specialist.where,
                                  recodes = "2=1;NA=NA;else=0")
    specialist.private <- bbw::recode(var = specialist.where,
                                      recodes = "3=1;NA=NA;else=0")
    specialist.srhc <- bbw::recode(var = specialist.where,
                                   recodes = "4=1;NA=NA;else=0")
    specialist.village <- bbw::recode(var = specialist.where,
                                      recodes = "5=1;NA=NA;else=0")
    specialist.eho <- bbw::recode(var = specialist.where,
                                  recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    specialist.dry <- as.integer(df$ancnow_pdoc_dist_dry)
    specialist.dry.lt15 <- bbw::recode(var = specialist.dry,
                                       recodes = "1=1;NA=NA;else=0")
    specialist.dry.gteq15lt60 <- bbw::recode(var = specialist.dry,
                                             recodes = "2=1;NA=NA;else=0")
    specialist.dry.gteq60lt120 <- bbw::recode(var = specialist.dry,
                                              recodes = "3=1;NA=NA;else=0")
    specialist.dry.gteq120lt360 <- bbw::recode(var = specialist.dry,
                                               recodes = "4=1;NA=NA;else=0")
    specialist.dry.gteq360lt720 <- bbw::recode(var = specialist.dry,
                                               recodes = "5=1;NA=NA;else=0")
    specialist.dry.gteq720lt1440 <- bbw::recode(var = specialist.dry,
                                                recodes = "6=1;NA=NA;else=0")
    specialist.dry.gteq1440 <- bbw::recode(var = specialist.dry,
                                           recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    specialist.wet <- as.integer(df$ancnow_pdoc_dist_wet)
    specialist.wet.lt15 <- bbw::recode(var = specialist.wet,
                                       recodes = "1=1;NA=NA;else=0")
    specialist.wet.gteq15lt60 <- bbw::recode(var = specialist.wet,
                                             recodes = "2=1;NA=NA;else=0")
    specialist.wet.gteq60lt120 <- bbw::recode(var = specialist.wet,
                                              recodes = "3=1;NA=NA;else=0")
    specialist.wet.gteq120lt360 <- bbw::recode(var = specialist.wet,
                                               recodes = "4=1;NA=NA;else=0")
    specialist.wet.gteq360lt720 <- bbw::recode(var = specialist.wet,
                                               recodes = "5=1;NA=NA;else=0")
    specialist.wet.gteq720lt1440 <- bbw::recode(var = specialist.wet,
                                                recodes = "6=1;NA=NA;else=0")
    specialist.wet.gteq1440 <- bbw::recode(var = specialist.wet,
                                           recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    specialist.visit <- as.integer(df$ancnow_pdoc_visit)
    ## Number of visits per trimester
    specialist.visit.first <- as.integer(df$ancnow_pdoc_1tri_times)
    specialist.visit.second <- as.integer(df$ancnow_pdoc_2tri_times)
    specialist.visit.third <- as.integer(df$ancnow_pdoc_3tri_times)
    ## Concatenate specialist provider data.frame
    specialist <- data.frame(specialist, specialist.where, specialist.home,
                             specialist.gov, specialist.private, specialist.srhc,
                             specialist.village, specialist.eho, specialist.dry,
                             specialist.dry.lt15, specialist.dry.gteq15lt60,
                             specialist.dry.gteq60lt120,
                             specialist.dry.gteq120lt360,
                             specialist.dry.gteq360lt720,
                             specialist.dry.gteq720lt1440,
                             specialist.dry.gteq1440, specialist.wet,
                             specialist.wet.lt15, specialist.wet.gteq15lt60,
                             specialist.wet.gteq60lt120,
                             specialist.wet.gteq120lt360,
                             specialist.wet.gteq360lt720,
                             specialist.wet.gteq720lt1440,
                             specialist.wet.gteq1440, specialist.visit,
                             specialist.visit.first, specialist.visit.second,
                             specialist.visit.third)
  } else {
    ## Attend specialist
    specialist <- as.integer(df$ancpast_who.1)
    ## Where specialist
    specialist.where <- as.integer(df$ancpast_pdoc_where)
    specialist.home <- bbw::recode(var = specialist.where,
                                   recodes = "1=1;NA=NA;else=0")
    specialist.gov <- bbw::recode(var = specialist.where,
                                  recodes = "2=1;NA=NA;else=0")
    specialist.private <- bbw::recode(var = specialist.where,
                                      recodes = "3=1;NA=NA;else=0")
    specialist.srhc <- bbw::recode(var = specialist.where,
                                   recodes = "4=1;NA=NA;else=0")
    specialist.village <- bbw::recode(var = specialist.where,
                                      recodes = "5=1;NA=NA;else=0")
    specialist.eho <- bbw::recode(var = specialist.where,
                                  recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    specialist.dry <- as.integer(df$ancpast_pdoc_dist_dry)
    specialist.dry.lt15 <- bbw::recode(var = specialist.dry,
                                       recodes = "1=1;NA=NA;else=0")
    specialist.dry.gteq15lt60 <- bbw::recode(var = specialist.dry,
                                             recodes = "2=1;NA=NA;else=0")
    specialist.dry.gteq60lt120 <- bbw::recode(var = specialist.dry,
                                              recodes = "3=1;NA=NA;else=0")
    specialist.dry.gteq120lt360 <- bbw::recode(var = specialist.dry,
                                               recodes = "4=1;NA=NA;else=0")
    specialist.dry.gteq360lt720 <- bbw::recode(var = specialist.dry,
                                               recodes = "5=1;NA=NA;else=0")
    specialist.dry.gteq720lt1440 <- bbw::recode(var = specialist.dry,
                                                recodes = "6=1;NA=NA;else=0")
    specialist.dry.gteq1440 <- bbw::recode(var = specialist.dry,
                                           recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    specialist.wet <- as.integer(df$ancpast_pdoc_dist_wet)
    specialist.wet.lt15 <- bbw::recode(var = specialist.wet,
                                       recodes = "1=1;NA=NA;else=0")
    specialist.wet.gteq15lt60 <- bbw::recode(var = specialist.wet,
                                             recodes = "2=1;NA=NA;else=0")
    specialist.wet.gteq60lt120 <- bbw::recode(var = specialist.wet,
                                              recodes = "3=1;NA=NA;else=0")
    specialist.wet.gteq120lt360 <- bbw::recode(var = specialist.wet,
                                               recodes = "4=1;NA=NA;else=0")
    specialist.wet.gteq360lt720 <- bbw::recode(var = specialist.wet,
                                               recodes = "5=1;NA=NA;else=0")
    specialist.wet.gteq720lt1440 <- bbw::recode(var = specialist.wet,
                                                recodes = "6=1;NA=NA;else=0")
    specialist.wet.gteq1440 <- bbw::recode(var = specialist.wet,
                                           recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    specialist.visit <- as.integer(df$ancpast_pdoc_visit)
    ## Number of visits per trimester
    specialist.visit.first <- as.integer(df$ancpast_pdoc_1tri_times)
    specialist.visit.second <- as.integer(df$ancpast_pdoc_2tri_times)
    specialist.visit.third <- as.integer(df$ancpast_pdoc_3tri_times)
    ## Concatenate specialist provider data.frame
    specialist <- data.frame(specialist, specialist.where, specialist.home,
                             specialist.gov,
                             specialist.private, specialist.srhc,
                             specialist.village,
                             specialist.eho, specialist.dry,
                             specialist.dry.lt15, specialist.dry.gteq15lt60,
                             specialist.dry.gteq60lt120,
                             specialist.dry.gteq120lt360,
                             specialist.dry.gteq360lt720,
                             specialist.dry.gteq720lt1440,
                             specialist.dry.gteq1440, specialist.wet,
                             specialist.wet.lt15, specialist.wet.gteq15lt60,
                             specialist.wet.gteq60lt120,
                             specialist.wet.gteq120lt360,
                             specialist.wet.gteq360lt720,
                             specialist.wet.gteq720lt1440,
                             specialist.wet.gteq1440, specialist.visit,
                             specialist.visit.first, specialist.visit.second,
                             specialist.visit.third)
  }
  ## Return data.frame
  return(specialist)
}


################################################################################
#
#' Function to recode the doctor ANC provider data for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded doctor ANC provider information
#'
#' @examples
#' ## Recode provider doctor for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_doctor(df = x, status = "current")
#'
#' ## Recode provider doctor for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_doctor(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_doctor <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend doctor
    doctor <- as.integer(df$ancnow_who.2)
    ## Where doctor
    doctor.where <- as.integer(df$ancnow_doc_where)
    doctor.home <- bbw::recode(var = doctor.where,
                               recodes = "1=1;NA=NA;else=0")
    doctor.gov <- bbw::recode(var = doctor.where,
                              recodes = "2=1;NA=NA;else=0")
    doctor.private <- bbw::recode(var = doctor.where,
                                  recodes = "3=1;NA=NA;else=0")
    doctor.srhc <- bbw::recode(var = doctor.where,
                               recodes = "4=1;NA=NA;else=0")
    doctor.village <- bbw::recode(var = doctor.where,
                                  recodes = "5=1;NA=NA;else=0")
    doctor.eho <- bbw::recode(var = doctor.where,
                              recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    doctor.dry <- as.integer(df$ancnow_doc_dist_dry)
    doctor.dry.lt15 <- bbw::recode(var = doctor.dry,
                                   recodes = "1=1;NA=NA;else=0")
    doctor.dry.gteq15lt60 <- bbw::recode(var = doctor.dry,
                                         recodes = "2=1;NA=NA;else=0")
    doctor.dry.gteq60lt120 <- bbw::recode(var = doctor.dry,
                                          recodes = "3=1;NA=NA;else=0")
    doctor.dry.gteq120lt360 <- bbw::recode(var = doctor.dry,
                                           recodes = "4=1;NA=NA;else=0")
    doctor.dry.gteq360lt720 <- bbw::recode(var = doctor.dry,
                                           recodes = "5=1;NA=NA;else=0")
    doctor.dry.gteq720lt1440 <- bbw::recode(var = doctor.dry,
                                            recodes = "6=1;NA=NA;else=0")
    doctor.dry.gteq1440 <- bbw::recode(var = doctor.dry,
                                       recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    doctor.wet <- as.integer(df$ancnow_doc_dist_wet)
    doctor.wet.lt15 <- bbw::recode(var = doctor.wet,
                                   recodes = "1=1;NA=NA;else=0")
    doctor.wet.gteq15lt60 <- bbw::recode(var = doctor.wet,
                                         recodes = "2=1;NA=NA;else=0")
    doctor.wet.gteq60lt120 <- bbw::recode(var = doctor.wet,
                                          recodes = "3=1;NA=NA;else=0")
    doctor.wet.gteq120lt360 <- bbw::recode(var = doctor.wet,
                                           recodes = "4=1;NA=NA;else=0")
    doctor.wet.gteq360lt720 <- bbw::recode(var = doctor.wet,
                                           recodes = "5=1;NA=NA;else=0")
    doctor.wet.gteq720lt1440 <- bbw::recode(var = doctor.wet,
                                            recodes = "6=1;NA=NA;else=0")
    doctor.wet.gteq1440 <- bbw::recode(var = doctor.wet,
                                       recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    doctor.visit <- as.integer(df$ancnow_doc_visit)
    ## Number of visits per trimester
    doctor.visit.first <- as.integer(df$ancnow_doc_1tri_times)
    doctor.visit.second <- as.integer(df$ancnow_doc_2tri_times)
    doctor.visit.third <- as.integer(df$ancnow_doc_3tri_times)
    ## Concatenate doctor provider data.frame
    doctor <- data.frame(doctor, doctor.where, doctor.home,
                         doctor.gov, doctor.private, doctor.srhc,
                         doctor.village, doctor.eho, doctor.dry,
                         doctor.dry.lt15, doctor.dry.gteq15lt60,
                         doctor.dry.gteq60lt120, doctor.dry.gteq120lt360,
                         doctor.dry.gteq360lt720, doctor.dry.gteq720lt1440,
                         doctor.dry.gteq1440, doctor.wet,
                         doctor.wet.lt15, doctor.wet.gteq15lt60,
                         doctor.wet.gteq60lt120, doctor.wet.gteq120lt360,
                         doctor.wet.gteq360lt720, doctor.wet.gteq720lt1440,
                         doctor.wet.gteq1440, doctor.visit,
                         doctor.visit.first, doctor.visit.second,
                         doctor.visit.third)
  } else {
    ## Attend doctor
    doctor <- as.integer(df$ancpast_who.2)
    ## Where doctor
    doctor.where <- as.integer(df$ancpast_doc_where)
    doctor.home <- bbw::recode(var = doctor.where,
                               recodes = "1=1;NA=NA;else=0")
    doctor.gov <- bbw::recode(var = doctor.where,
                              recodes = "2=1;NA=NA;else=0")
    doctor.private <- bbw::recode(var = doctor.where,
                                  recodes = "3=1;NA=NA;else=0")
    doctor.srhc <- bbw::recode(var = doctor.where,
                               recodes = "4=1;NA=NA;else=0")
    doctor.village <- bbw::recode(var = doctor.where,
                                  recodes = "5=1;NA=NA;else=0")
    doctor.eho <- bbw::recode(var = doctor.where,
                              recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    doctor.dry <- as.integer(df$ancpast_doc_dist_dry)
    doctor.dry.lt15 <- bbw::recode(var = doctor.dry,
                                   recodes = "1=1;NA=NA;else=0")
    doctor.dry.gteq15lt60 <- bbw::recode(var = doctor.dry,
                                         recodes = "2=1;NA=NA;else=0")
    doctor.dry.gteq60lt120 <- bbw::recode(var = doctor.dry,
                                          recodes = "3=1;NA=NA;else=0")
    doctor.dry.gteq120lt360 <- bbw::recode(var = doctor.dry,
                                           recodes = "4=1;NA=NA;else=0")
    doctor.dry.gteq360lt720 <- bbw::recode(var = doctor.dry,
                                           recodes = "5=1;NA=NA;else=0")
    doctor.dry.gteq720lt1440 <- bbw::recode(var = doctor.dry,
                                            recodes = "6=1;NA=NA;else=0")
    doctor.dry.gteq1440 <- bbw::recode(var = doctor.dry,
                                       recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    doctor.wet <- as.integer(df$ancpast_doc_dist_wet)
    doctor.wet.lt15 <- bbw::recode(var = doctor.wet,
                                   recodes = "1=1;NA=NA;else=0")
    doctor.wet.gteq15lt60 <- bbw::recode(var = doctor.wet,
                                         recodes = "2=1;NA=NA;else=0")
    doctor.wet.gteq60lt120 <- bbw::recode(var = doctor.wet,
                                          recodes = "3=1;NA=NA;else=0")
    doctor.wet.gteq120lt360 <- bbw::recode(var = doctor.wet,
                                           recodes = "4=1;NA=NA;else=0")
    doctor.wet.gteq360lt720 <- bbw::recode(var = doctor.wet,
                                           recodes = "5=1;NA=NA;else=0")
    doctor.wet.gteq720lt1440 <- bbw::recode(var = doctor.wet,
                                            recodes = "6=1;NA=NA;else=0")
    doctor.wet.gteq1440 <- bbw::recode(var = doctor.wet,
                                       recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    doctor.visit <- as.integer(df$ancpast_doc_visit)
    ## Number of visits per trimester
    doctor.visit.first <- as.integer(df$ancpast_doc_1tri_times)
    doctor.visit.second <- as.integer(df$ancpast_doc_2tri_times)
    doctor.visit.third <- as.integer(df$ancpast_doc_3tri_times)
    ## Concatenate doctor provider data.frame
    doctor <- data.frame(doctor, doctor.where, doctor.home,
                         doctor.gov, doctor.private, doctor.srhc,
                         doctor.village, doctor.eho, doctor.dry,
                         doctor.dry.lt15, doctor.dry.gteq15lt60,
                         doctor.dry.gteq60lt120, doctor.dry.gteq120lt360,
                         doctor.dry.gteq360lt720, doctor.dry.gteq720lt1440,
                         doctor.dry.gteq1440, doctor.wet,
                         doctor.wet.lt15, doctor.wet.gteq15lt60,
                         doctor.wet.gteq60lt120, doctor.wet.gteq120lt360,
                         doctor.wet.gteq360lt720, doctor.wet.gteq720lt1440,
                         doctor.wet.gteq1440, doctor.visit,
                         doctor.visit.first, doctor.visit.second,
                         doctor.visit.third)
  }
  ## Return data.frame
  return(doctor)
}


################################################################################
#
#' Function to recode the nurse ANC provider data for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded nurse ANC provider information
#'
#' @examples
#' ## Recode nurse provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_nurse(df = x, status = "current")
#'
#' ## Recode nurse provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_nurse(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_nurse <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend nurse
    nurse <- as.integer(df$ancnow_who.3)
    ## Where nurse
    nurse.where <- as.integer(df$ancnow_nurs_where)
    nurse.home <- bbw::recode(var = nurse.where,
                              recodes = "1=1;NA=NA;else=0")
    nurse.gov <- bbw::recode(var = nurse.where,
                             recodes = "2=1;NA=NA;else=0")
    nurse.private <- bbw::recode(var = nurse.where,
                                 recodes = "3=1;NA=NA;else=0")
    nurse.srhc <- bbw::recode(var = nurse.where,
                              recodes = "4=1;NA=NA;else=0")
    nurse.village <- bbw::recode(var = nurse.where,
                                 recodes = "5=1;NA=NA;else=0")
    nurse.eho <- bbw::recode(var = nurse.where,
                             recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    nurse.dry <- as.integer(df$ancnow_nurs_dist_dry)
    nurse.dry.lt15 <- bbw::recode(var = nurse.dry,
                                  recodes = "1=1;NA=NA;else=0")
    nurse.dry.gteq15lt60 <- bbw::recode(var = nurse.dry,
                                        recodes = "2=1;NA=NA;else=0")
    nurse.dry.gteq60lt120 <- bbw::recode(var = nurse.dry,
                                         recodes = "3=1;NA=NA;else=0")
    nurse.dry.gteq120lt360 <- bbw::recode(var = nurse.dry,
                                          recodes = "4=1;NA=NA;else=0")
    nurse.dry.gteq360lt720 <- bbw::recode(var = nurse.dry,
                                          recodes = "5=1;NA=NA;else=0")
    nurse.dry.gteq720lt1440 <- bbw::recode(var = nurse.dry,
                                           recodes = "6=1;NA=NA;else=0")
    nurse.dry.gteq1440 <- bbw::recode(var = nurse.dry,
                                      recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    nurse.wet <- as.integer(df$ancnow_nurs_dist_wet)
    nurse.wet.lt15 <- bbw::recode(var = nurse.wet,
                                  recodes = "1=1;NA=NA;else=0")
    nurse.wet.gteq15lt60 <- bbw::recode(var = nurse.wet,
                                        recodes = "2=1;NA=NA;else=0")
    nurse.wet.gteq60lt120 <- bbw::recode(var = nurse.wet,
                                         recodes = "3=1;NA=NA;else=0")
    nurse.wet.gteq120lt360 <- bbw::recode(var = nurse.wet,
                                          recodes = "4=1;NA=NA;else=0")
    nurse.wet.gteq360lt720 <- bbw::recode(var = nurse.wet,
                                          recodes = "5=1;NA=NA;else=0")
    nurse.wet.gteq720lt1440 <- bbw::recode(var = nurse.wet,
                                           recodes = "6=1;NA=NA;else=0")
    nurse.wet.gteq1440 <- bbw::recode(var = nurse.wet,
                                      recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    nurse.visit <- as.integer(df$ancnow_nurs_visit)
    ## Number of visits per trimester
    nurse.visit.first <- as.integer(df$ancnow_nurs_1tri_times)
    nurse.visit.second <- as.integer(df$ancnow_nurs_2tri_times)
    nurse.visit.third <- as.integer(df$ancnow_nurs_3tri_times)
    ## Concatenate nurse provider data.frame
    nurse <- data.frame(nurse, nurse.where, nurse.home,
                        nurse.gov, nurse.private, nurse.srhc,
                        nurse.village, nurse.eho, nurse.dry,
                        nurse.dry.lt15, nurse.dry.gteq15lt60,
                        nurse.dry.gteq60lt120, nurse.dry.gteq120lt360,
                        nurse.dry.gteq360lt720, nurse.dry.gteq720lt1440,
                        nurse.dry.gteq1440, nurse.wet,
                        nurse.wet.lt15, nurse.wet.gteq15lt60,
                        nurse.wet.gteq60lt120, nurse.wet.gteq120lt360,
                        nurse.wet.gteq360lt720, nurse.wet.gteq720lt1440,
                        nurse.wet.gteq1440, nurse.visit,
                        nurse.visit.first, nurse.visit.second,
                        nurse.visit.third)
  } else {
    ## Attend nurse
    nurse <- as.integer(df$ancpast_who.3)
    ## Where nurse
    nurse.where <- as.integer(df$ancpast_nurs_where)
    nurse.home <- bbw::recode(var = nurse.where,
                              recodes = "1=1;NA=NA;else=0")
    nurse.gov <- bbw::recode(var = nurse.where,
                             recodes = "2=1;NA=NA;else=0")
    nurse.private <- bbw::recode(var = nurse.where,
                                 recodes = "3=1;NA=NA;else=0")
    nurse.srhc <- bbw::recode(var = nurse.where,
                              recodes = "4=1;NA=NA;else=0")
    nurse.village <- bbw::recode(var = nurse.where,
                                 recodes = "5=1;NA=NA;else=0")
    nurse.eho <- bbw::recode(var = nurse.where,
                             recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    nurse.dry <- as.integer(df$ancpast_nurs_dist_dry)
    nurse.dry.lt15 <- bbw::recode(var = nurse.dry,
                                  recodes = "1=1;NA=NA;else=0")
    nurse.dry.gteq15lt60 <- bbw::recode(var = nurse.dry,
                                        recodes = "2=1;NA=NA;else=0")
    nurse.dry.gteq60lt120 <- bbw::recode(var = nurse.dry,
                                         recodes = "3=1;NA=NA;else=0")
    nurse.dry.gteq120lt360 <- bbw::recode(var = nurse.dry,
                                          recodes = "4=1;NA=NA;else=0")
    nurse.dry.gteq360lt720 <- bbw::recode(var = nurse.dry,
                                          recodes = "5=1;NA=NA;else=0")
    nurse.dry.gteq720lt1440 <- bbw::recode(var = nurse.dry,
                                           recodes = "6=1;NA=NA;else=0")
    nurse.dry.gteq1440 <- bbw::recode(var = nurse.dry,
                                      recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    nurse.wet <- as.integer(df$ancpast_nurs_dist_wet)
    nurse.wet.lt15 <- bbw::recode(var = nurse.wet,
                                  recodes = "1=1;NA=NA;else=0")
    nurse.wet.gteq15lt60 <- bbw::recode(var = nurse.wet,
                                        recodes = "2=1;NA=NA;else=0")
    nurse.wet.gteq60lt120 <- bbw::recode(var = nurse.wet,
                                         recodes = "3=1;NA=NA;else=0")
    nurse.wet.gteq120lt360 <- bbw::recode(var = nurse.wet,
                                          recodes = "4=1;NA=NA;else=0")
    nurse.wet.gteq360lt720 <- bbw::recode(var = nurse.wet,
                                          recodes = "5=1;NA=NA;else=0")
    nurse.wet.gteq720lt1440 <- bbw::recode(var = nurse.wet,
                                           recodes = "6=1;NA=NA;else=0")
    nurse.wet.gteq1440 <- bbw::recode(var = nurse.wet,
                                      recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    nurse.visit <- as.integer(df$ancpast_nurs_visit)
    ## Number of visits per trimester
    nurse.visit.first <- as.integer(df$ancpast_nurs_1tri_times)
    nurse.visit.second <- as.integer(df$ancpast_nurs_2tri_times)
    nurse.visit.third <- as.integer(df$ancpast_nurs_3tri_times)
    ## Concatenate nurse provider data.frame
    nurse <- data.frame(nurse, nurse.where, nurse.home,
                        nurse.gov, nurse.private, nurse.srhc,
                        nurse.village, nurse.eho, nurse.dry,
                        nurse.dry.lt15, nurse.dry.gteq15lt60,
                        nurse.dry.gteq60lt120, nurse.dry.gteq120lt360,
                        nurse.dry.gteq360lt720, nurse.dry.gteq720lt1440,
                        nurse.dry.gteq1440, nurse.wet,
                        nurse.wet.lt15, nurse.wet.gteq15lt60,
                        nurse.wet.gteq60lt120, nurse.wet.gteq120lt360,
                        nurse.wet.gteq360lt720, nurse.wet.gteq720lt1440,
                        nurse.wet.gteq1440, nurse.visit,
                        nurse.visit.first, nurse.visit.second,
                        nurse.visit.third)
  }
  ## Return data.frame
  return(nurse)
}


################################################################################
#
#' Function to recode the health assistant ANC provider data for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded health assistant ANC provider information
#'
#' @examples
#' ## Recode health assistant provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_ha(df = x, status = "current")
#'
#' ## Recode health assistant provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_ha(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_ha <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend ha
    ha <- as.integer(df$ancnow_who.4)
    ## Where ha
    ha.where <- as.integer(df$ancnow_ha_where)
    ha.home <- bbw::recode(var = ha.where,
                           recodes = "1=1;NA=NA;else=0")
    ha.gov <- bbw::recode(var = ha.where,
                          recodes = "2=1;NA=NA;else=0")
    ha.private <- bbw::recode(var = ha.where,
                              recodes = "3=1;NA=NA;else=0")
    ha.srhc <- bbw::recode(var = ha.where,
                           recodes = "4=1;NA=NA;else=0")
    ha.village <- bbw::recode(var = ha.where,
                              recodes = "5=1;NA=NA;else=0")
    ha.eho <- bbw::recode(var = ha.where,
                          recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    ha.dry <- as.integer(df$ancnow_ha_dist_dry)
    ha.dry.lt15 <- bbw::recode(var = ha.dry,
                               recodes = "1=1;NA=NA;else=0")
    ha.dry.gteq15lt60 <- bbw::recode(var = ha.dry,
                                     recodes = "2=1;NA=NA;else=0")
    ha.dry.gteq60lt120 <- bbw::recode(var = ha.dry,
                                      recodes = "3=1;NA=NA;else=0")
    ha.dry.gteq120lt360 <- bbw::recode(var = ha.dry,
                                       recodes = "4=1;NA=NA;else=0")
    ha.dry.gteq360lt720 <- bbw::recode(var = ha.dry,
                                       recodes = "5=1;NA=NA;else=0")
    ha.dry.gteq720lt1440 <- bbw::recode(var = ha.dry,
                                        recodes = "6=1;NA=NA;else=0")
    ha.dry.gteq1440 <- bbw::recode(var = ha.dry,
                                   recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    ha.wet <- as.integer(df$ancnow_ha_dist_wet)
    ha.wet.lt15 <- bbw::recode(var = ha.wet,
                               recodes = "1=1;NA=NA;else=0")
    ha.wet.gteq15lt60 <- bbw::recode(var = ha.wet,
                                     recodes = "2=1;NA=NA;else=0")
    ha.wet.gteq60lt120 <- bbw::recode(var = ha.wet,
                                      recodes = "3=1;NA=NA;else=0")
    ha.wet.gteq120lt360 <- bbw::recode(var = ha.wet,
                                       recodes = "4=1;NA=NA;else=0")
    ha.wet.gteq360lt720 <- bbw::recode(var = ha.wet,
                                       recodes = "5=1;NA=NA;else=0")
    ha.wet.gteq720lt1440 <- bbw::recode(var = ha.wet,
                                        recodes = "6=1;NA=NA;else=0")
    ha.wet.gteq1440 <- bbw::recode(var = ha.wet,
                                   recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    ha.visit <- as.integer(df$ancnow_ha_visit)
    ## Number of visits per trimester
    ha.visit.first <- as.integer(df$ancnow_ha_1tri_times)
    ha.visit.second <- as.integer(df$ancnow_ha_2tri_times)
    ha.visit.third <- as.integer(df$ancnow_ha_3tri_times)
    ## Concatenate ha provider data.frame
    ha <- data.frame(ha, ha.where, ha.home,
                     ha.gov, ha.private, ha.srhc,
                     ha.village, ha.eho, ha.dry,
                     ha.dry.lt15, ha.dry.gteq15lt60,
                     ha.dry.gteq60lt120, ha.dry.gteq120lt360,
                     ha.dry.gteq360lt720, ha.dry.gteq720lt1440,
                     ha.dry.gteq1440, ha.wet,
                     ha.wet.lt15, ha.wet.gteq15lt60,
                     ha.wet.gteq60lt120, ha.wet.gteq120lt360,
                     ha.wet.gteq360lt720, ha.wet.gteq720lt1440,
                     ha.wet.gteq1440, ha.visit,
                     ha.visit.first, ha.visit.second,
                     ha.visit.third)
  } else {
    ## Attend ha
    ha <- as.integer(df$ancpast_who.4)
    ## Where ha
    ha.where <- as.integer(df$ancpast_ha_where)
    ha.home <- bbw::recode(var = ha.where,
                           recodes = "1=1;NA=NA;else=0")
    ha.gov <- bbw::recode(var = ha.where,
                          recodes = "2=1;NA=NA;else=0")
    ha.private <- bbw::recode(var = ha.where,
                              recodes = "3=1;NA=NA;else=0")
    ha.srhc <- bbw::recode(var = ha.where,
                           recodes = "4=1;NA=NA;else=0")
    ha.village <- bbw::recode(var = ha.where,
                              recodes = "5=1;NA=NA;else=0")
    ha.eho <- bbw::recode(var = ha.where,
                          recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    ha.dry <- as.integer(df$ancpast_ha_dist_dry)
    ha.dry.lt15 <- bbw::recode(var = ha.dry,
                               recodes = "1=1;NA=NA;else=0")
    ha.dry.gteq15lt60 <- bbw::recode(var = ha.dry,
                                     recodes = "2=1;NA=NA;else=0")
    ha.dry.gteq60lt120 <- bbw::recode(var = ha.dry,
                                      recodes = "3=1;NA=NA;else=0")
    ha.dry.gteq120lt360 <- bbw::recode(var = ha.dry,
                                       recodes = "4=1;NA=NA;else=0")
    ha.dry.gteq360lt720 <- bbw::recode(var = ha.dry,
                                       recodes = "5=1;NA=NA;else=0")
    ha.dry.gteq720lt1440 <- bbw::recode(var = ha.dry,
                                        recodes = "6=1;NA=NA;else=0")
    ha.dry.gteq1440 <- bbw::recode(var = ha.dry,
                                   recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    ha.wet <- as.integer(df$ancpast_ha_dist_wet)
    ha.wet.lt15 <- bbw::recode(var = ha.wet,
                               recodes = "1=1;NA=NA;else=0")
    ha.wet.gteq15lt60 <- bbw::recode(var = ha.wet,
                                     recodes = "2=1;NA=NA;else=0")
    ha.wet.gteq60lt120 <- bbw::recode(var = ha.wet,
                                      recodes = "3=1;NA=NA;else=0")
    ha.wet.gteq120lt360 <- bbw::recode(var = ha.wet,
                                       recodes = "4=1;NA=NA;else=0")
    ha.wet.gteq360lt720 <- bbw::recode(var = ha.wet,
                                       recodes = "5=1;NA=NA;else=0")
    ha.wet.gteq720lt1440 <- bbw::recode(var = ha.wet,
                                        recodes = "6=1;NA=NA;else=0")
    ha.wet.gteq1440 <- bbw::recode(var = ha.wet,
                                   recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    ha.visit <- as.integer(df$ancpast_ha_visit)
    ## Number of visits per trimester
    ha.visit.first <- as.integer(df$ancpast_ha_1tri_times)
    ha.visit.second <- as.integer(df$ancpast_ha_2tri_times)
    ha.visit.third <- as.integer(df$ancpast_ha_3tri_times)
    ## Concatenate ha provider data.frame
    ha <- data.frame(ha, ha.where, ha.home,
                     ha.gov, ha.private, ha.srhc,
                     ha.village, ha.eho, ha.dry,
                     ha.dry.lt15, ha.dry.gteq15lt60,
                     ha.dry.gteq60lt120, ha.dry.gteq120lt360,
                     ha.dry.gteq360lt720, ha.dry.gteq720lt1440,
                     ha.dry.gteq1440, ha.wet,
                     ha.wet.lt15, ha.wet.gteq15lt60,
                     ha.wet.gteq60lt120, ha.wet.gteq120lt360,
                     ha.wet.gteq360lt720, ha.wet.gteq720lt1440,
                     ha.wet.gteq1440, ha.visit,
                     ha.visit.first, ha.visit.second,
                     ha.visit.third)
  }
  ## Return data.frame
  return(ha)
}


################################################################################
#
#' Function to recode the private doctor ANC provider data for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded private doctor ANC provider information
#'
#' @examples
#' ## Recode private doctor provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_pdoc(df = x, status = "current")
#'
#' ## Recode private doctor provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_pdoc(df = x, status = "past")

#' @export
#'
#'
#
################################################################################

recode_provider_pdoc <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend pdoc
    pdoc <- as.integer(df$ancnow_who.5)
    ## Where pdoc
    pdoc.where <- as.integer(df$ancnow_pdoc_where)
    pdoc.home <- bbw::recode(var = pdoc.where,
                             recodes = "1=1;NA=NA;else=0")
    pdoc.gov <- bbw::recode(var = pdoc.where,
                            recodes = "2=1;NA=NA;else=0")
    pdoc.private <- bbw::recode(var = pdoc.where,
                                recodes = "3=1;NA=NA;else=0")
    pdoc.srhc <- bbw::recode(var = pdoc.where,
                             recodes = "4=1;NA=NA;else=0")
    pdoc.village <- bbw::recode(var = pdoc.where,
                                recodes = "5=1;NA=NA;else=0")
    pdoc.eho <- bbw::recode(var = pdoc.where,
                            recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    pdoc.dry <- as.integer(df$ancnow_pdoc_dist_dry)
    pdoc.dry.lt15 <- bbw::recode(var = pdoc.dry,
                                 recodes = "1=1;NA=NA;else=0")
    pdoc.dry.gteq15lt60 <- bbw::recode(var = pdoc.dry,
                                       recodes = "2=1;NA=NA;else=0")
    pdoc.dry.gteq60lt120 <- bbw::recode(var = pdoc.dry,
                                        recodes = "3=1;NA=NA;else=0")
    pdoc.dry.gteq120lt360 <- bbw::recode(var = pdoc.dry,
                                         recodes = "4=1;NA=NA;else=0")
    pdoc.dry.gteq360lt720 <- bbw::recode(var = pdoc.dry,
                                         recodes = "5=1;NA=NA;else=0")
    pdoc.dry.gteq720lt1440 <- bbw::recode(var = pdoc.dry,
                                          recodes = "6=1;NA=NA;else=0")
    pdoc.dry.gteq1440 <- bbw::recode(var = pdoc.dry,
                                     recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    pdoc.wet <- as.integer(df$ancnow_pdoc_dist_wet)
    pdoc.wet.lt15 <- bbw::recode(var = pdoc.wet,
                                 recodes = "1=1;NA=NA;else=0")
    pdoc.wet.gteq15lt60 <- bbw::recode(var = pdoc.wet,
                                       recodes = "2=1;NA=NA;else=0")
    pdoc.wet.gteq60lt120 <- bbw::recode(var = pdoc.wet,
                                        recodes = "3=1;NA=NA;else=0")
    pdoc.wet.gteq120lt360 <- bbw::recode(var = pdoc.wet,
                                         recodes = "4=1;NA=NA;else=0")
    pdoc.wet.gteq360lt720 <- bbw::recode(var = pdoc.wet,
                                         recodes = "5=1;NA=NA;else=0")
    pdoc.wet.gteq720lt1440 <- bbw::recode(var = pdoc.wet,
                                          recodes = "6=1;NA=NA;else=0")
    pdoc.wet.gteq1440 <- bbw::recode(var = pdoc.wet,
                                     recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    pdoc.visit <- as.integer(df$ancnow_pdoc_visit)
    ## Number of visits per trimester
    pdoc.visit.first <- as.integer(df$ancnow_pdoc_1tri_times)
    pdoc.visit.second <- as.integer(df$ancnow_pdoc_2tri_times)
    pdoc.visit.third <- as.integer(df$ancnow_pdoc_3tri_times)
    ## Concatenate pdoc provider data.frame
    pdoc <- data.frame(pdoc, pdoc.where, pdoc.home,
                       pdoc.gov, pdoc.private, pdoc.srhc,
                       pdoc.village, pdoc.eho, pdoc.dry,
                       pdoc.dry.lt15, pdoc.dry.gteq15lt60,
                       pdoc.dry.gteq60lt120, pdoc.dry.gteq120lt360,
                       pdoc.dry.gteq360lt720, pdoc.dry.gteq720lt1440,
                       pdoc.dry.gteq1440, pdoc.wet,
                       pdoc.wet.lt15, pdoc.wet.gteq15lt60,
                       pdoc.wet.gteq60lt120, pdoc.wet.gteq120lt360,
                       pdoc.wet.gteq360lt720, pdoc.wet.gteq720lt1440,
                       pdoc.wet.gteq1440, pdoc.visit,
                       pdoc.visit.first, pdoc.visit.second,
                       pdoc.visit.third)
  } else {
    ## Attend pdoc
    pdoc <- as.integer(df$ancpast_who.5)
    ## Where pdoc
    pdoc.where <- as.integer(df$ancpast_pdoc_where)
    pdoc.home <- bbw::recode(var = pdoc.where,
                             recodes = "1=1;NA=NA;else=0")
    pdoc.gov <- bbw::recode(var = pdoc.where,
                            recodes = "2=1;NA=NA;else=0")
    pdoc.private <- bbw::recode(var = pdoc.where,
                                recodes = "3=1;NA=NA;else=0")
    pdoc.srhc <- bbw::recode(var = pdoc.where,
                             recodes = "4=1;NA=NA;else=0")
    pdoc.village <- bbw::recode(var = pdoc.where,
                                recodes = "5=1;NA=NA;else=0")
    pdoc.eho <- bbw::recode(var = pdoc.where,
                            recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    pdoc.dry <- as.integer(df$ancpast_pdoc_dist_dry)
    pdoc.dry.lt15 <- bbw::recode(var = pdoc.dry,
                                 recodes = "1=1;NA=NA;else=0")
    pdoc.dry.gteq15lt60 <- bbw::recode(var = pdoc.dry,
                                       recodes = "2=1;NA=NA;else=0")
    pdoc.dry.gteq60lt120 <- bbw::recode(var = pdoc.dry,
                                        recodes = "3=1;NA=NA;else=0")
    pdoc.dry.gteq120lt360 <- bbw::recode(var = pdoc.dry,
                                         recodes = "4=1;NA=NA;else=0")
    pdoc.dry.gteq360lt720 <- bbw::recode(var = pdoc.dry,
                                         recodes = "5=1;NA=NA;else=0")
    pdoc.dry.gteq720lt1440 <- bbw::recode(var = pdoc.dry,
                                          recodes = "6=1;NA=NA;else=0")
    pdoc.dry.gteq1440 <- bbw::recode(var = pdoc.dry,
                                     recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    pdoc.wet <- as.integer(df$ancpast_pdoc_dist_wet)
    pdoc.wet.lt15 <- bbw::recode(var = pdoc.wet,
                                 recodes = "1=1;NA=NA;else=0")
    pdoc.wet.gteq15lt60 <- bbw::recode(var = pdoc.wet,
                                       recodes = "2=1;NA=NA;else=0")
    pdoc.wet.gteq60lt120 <- bbw::recode(var = pdoc.wet,
                                        recodes = "3=1;NA=NA;else=0")
    pdoc.wet.gteq120lt360 <- bbw::recode(var = pdoc.wet,
                                         recodes = "4=1;NA=NA;else=0")
    pdoc.wet.gteq360lt720 <- bbw::recode(var = pdoc.wet,
                                         recodes = "5=1;NA=NA;else=0")
    pdoc.wet.gteq720lt1440 <- bbw::recode(var = pdoc.wet,
                                          recodes = "6=1;NA=NA;else=0")
    pdoc.wet.gteq1440 <- bbw::recode(var = pdoc.wet,
                                     recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    pdoc.visit <- as.integer(df$ancpast_pdoc_visit)
    ## Number of visits per trimester
    pdoc.visit.first <- as.integer(df$ancpast_pdoc_1tri_times)
    pdoc.visit.second <- as.integer(df$ancpast_pdoc_2tri_times)
    pdoc.visit.third <- as.integer(df$ancpast_pdoc_3tri_times)
    ## Concatenate pdoc provider data.frame
    pdoc <- data.frame(pdoc, pdoc.where, pdoc.home,
                       pdoc.gov, pdoc.private, pdoc.srhc,
                       pdoc.village, pdoc.eho, pdoc.dry,
                       pdoc.dry.lt15, pdoc.dry.gteq15lt60,
                       pdoc.dry.gteq60lt120, pdoc.dry.gteq120lt360,
                       pdoc.dry.gteq360lt720, pdoc.dry.gteq720lt1440,
                       pdoc.dry.gteq1440, pdoc.wet,
                       pdoc.wet.lt15, pdoc.wet.gteq15lt60,
                       pdoc.wet.gteq60lt120, pdoc.wet.gteq120lt360,
                       pdoc.wet.gteq360lt720, pdoc.wet.gteq720lt1440,
                       pdoc.wet.gteq1440, pdoc.visit,
                       pdoc.visit.first, pdoc.visit.second,
                       pdoc.visit.third)
  }
  ## Return data.frame
  return(pdoc)
}


################################################################################
#
#' Function to recode the lady health visitor ANC provider data for the Myanmar
#' MCCT Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded lady health visitor ANC provider information
#'
#' @examples
#' ## Recode lady health visitor provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_lhv(df = x, status = "current")
#'
#' ## Recode lady health visitor provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_lhv(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_lhv <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend lhv
    lhv <- as.integer(df$ancnow_who.6)
    ## Where lhv
    lhv.where <- as.integer(df$ancnow_lhv_where)
    lhv.home <- bbw::recode(var = lhv.where,
                            recodes = "1=1;NA=NA;else=0")
    lhv.gov <- bbw::recode(var = lhv.where,
                           recodes = "2=1;NA=NA;else=0")
    lhv.private <- bbw::recode(var = lhv.where,
                               recodes = "3=1;NA=NA;else=0")
    lhv.srhc <- bbw::recode(var = lhv.where,
                            recodes = "4=1;NA=NA;else=0")
    lhv.village <- bbw::recode(var = lhv.where,
                               recodes = "5=1;NA=NA;else=0")
    lhv.eho <- bbw::recode(var = lhv.where,
                           recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    lhv.dry <- as.integer(df$ancnow_lhv_dist_dry)
    lhv.dry.lt15 <- bbw::recode(var = lhv.dry,
                                recodes = "1=1;NA=NA;else=0")
    lhv.dry.gteq15lt60 <- bbw::recode(var = lhv.dry,
                                      recodes = "2=1;NA=NA;else=0")
    lhv.dry.gteq60lt120 <- bbw::recode(var = lhv.dry,
                                       recodes = "3=1;NA=NA;else=0")
    lhv.dry.gteq120lt360 <- bbw::recode(var = lhv.dry,
                                        recodes = "4=1;NA=NA;else=0")
    lhv.dry.gteq360lt720 <- bbw::recode(var = lhv.dry,
                                        recodes = "5=1;NA=NA;else=0")
    lhv.dry.gteq720lt1440 <- bbw::recode(var = lhv.dry,
                                         recodes = "6=1;NA=NA;else=0")
    lhv.dry.gteq1440 <- bbw::recode(var = lhv.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    lhv.wet <- as.integer(df$ancnow_lhv_dist_wet)
    lhv.wet.lt15 <- bbw::recode(var = lhv.wet,
                                recodes = "1=1;NA=NA;else=0")
    lhv.wet.gteq15lt60 <- bbw::recode(var = lhv.wet,
                                      recodes = "2=1;NA=NA;else=0")
    lhv.wet.gteq60lt120 <- bbw::recode(var = lhv.wet,
                                       recodes = "3=1;NA=NA;else=0")
    lhv.wet.gteq120lt360 <- bbw::recode(var = lhv.wet,
                                        recodes = "4=1;NA=NA;else=0")
    lhv.wet.gteq360lt720 <- bbw::recode(var = lhv.wet,
                                        recodes = "5=1;NA=NA;else=0")
    lhv.wet.gteq720lt1440 <- bbw::recode(var = lhv.wet,
                                         recodes = "6=1;NA=NA;else=0")
    lhv.wet.gteq1440 <- bbw::recode(var = lhv.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    lhv.visit <- as.integer(df$ancnow_lhv_visit)
    ## Number of visits per trimester
    lhv.visit.first <- as.integer(df$ancnow_lhv_1tri_times)
    lhv.visit.second <- as.integer(df$ancnow_lhv_2tri_times)
    lhv.visit.third <- as.integer(df$ancnow_lhv_3tri_times)
    ## Concatenate lhv provider data.frame
    lhv <- data.frame(lhv, lhv.where, lhv.home,
                      lhv.gov, lhv.private, lhv.srhc,
                      lhv.village, lhv.eho, lhv.dry,
                      lhv.dry.lt15, lhv.dry.gteq15lt60,
                      lhv.dry.gteq60lt120, lhv.dry.gteq120lt360,
                      lhv.dry.gteq360lt720, lhv.dry.gteq720lt1440,
                      lhv.dry.gteq1440, lhv.wet,
                      lhv.wet.lt15, lhv.wet.gteq15lt60,
                      lhv.wet.gteq60lt120, lhv.wet.gteq120lt360,
                      lhv.wet.gteq360lt720, lhv.wet.gteq720lt1440,
                      lhv.wet.gteq1440, lhv.visit,
                      lhv.visit.first, lhv.visit.second,
                      lhv.visit.third)
  } else {
    ## Attend lhv
    lhv <- as.integer(df$ancpast_who.6)
    ## Where lhv
    lhv.where <- as.integer(df$ancpast_lhv_where)
    lhv.home <- bbw::recode(var = lhv.where,
                            recodes = "1=1;NA=NA;else=0")
    lhv.gov <- bbw::recode(var = lhv.where,
                           recodes = "2=1;NA=NA;else=0")
    lhv.private <- bbw::recode(var = lhv.where,
                               recodes = "3=1;NA=NA;else=0")
    lhv.srhc <- bbw::recode(var = lhv.where,
                            recodes = "4=1;NA=NA;else=0")
    lhv.village <- bbw::recode(var = lhv.where,
                               recodes = "5=1;NA=NA;else=0")
    lhv.eho <- bbw::recode(var = lhv.where,
                           recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    lhv.dry <- as.integer(df$ancpast_lhv_dist_dry)
    lhv.dry.lt15 <- bbw::recode(var = lhv.dry,
                                recodes = "1=1;NA=NA;else=0")
    lhv.dry.gteq15lt60 <- bbw::recode(var = lhv.dry,
                                      recodes = "2=1;NA=NA;else=0")
    lhv.dry.gteq60lt120 <- bbw::recode(var = lhv.dry,
                                       recodes = "3=1;NA=NA;else=0")
    lhv.dry.gteq120lt360 <- bbw::recode(var = lhv.dry,
                                        recodes = "4=1;NA=NA;else=0")
    lhv.dry.gteq360lt720 <- bbw::recode(var = lhv.dry,
                                        recodes = "5=1;NA=NA;else=0")
    lhv.dry.gteq720lt1440 <- bbw::recode(var = lhv.dry,
                                         recodes = "6=1;NA=NA;else=0")
    lhv.dry.gteq1440 <- bbw::recode(var = lhv.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    lhv.wet <- as.integer(df$ancpast_lhv_dist_wet)
    lhv.wet.lt15 <- bbw::recode(var = lhv.wet,
                                recodes = "1=1;NA=NA;else=0")
    lhv.wet.gteq15lt60 <- bbw::recode(var = lhv.wet,
                                      recodes = "2=1;NA=NA;else=0")
    lhv.wet.gteq60lt120 <- bbw::recode(var = lhv.wet,
                                       recodes = "3=1;NA=NA;else=0")
    lhv.wet.gteq120lt360 <- bbw::recode(var = lhv.wet,
                                        recodes = "4=1;NA=NA;else=0")
    lhv.wet.gteq360lt720 <- bbw::recode(var = lhv.wet,
                                        recodes = "5=1;NA=NA;else=0")
    lhv.wet.gteq720lt1440 <- bbw::recode(var = lhv.wet,
                                         recodes = "6=1;NA=NA;else=0")
    lhv.wet.gteq1440 <- bbw::recode(var = lhv.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    lhv.visit <- as.integer(df$ancpast_lhv_visit)
    ## Number of visits per trimester
    lhv.visit.first <- as.integer(df$ancpast_lhv_1tri_times)
    lhv.visit.second <- as.integer(df$ancpast_lhv_2tri_times)
    lhv.visit.third <- as.integer(df$ancpast_lhv_3tri_times)
    ## Concatenate lhv provider data.frame
    lhv <- data.frame(lhv, lhv.where, lhv.home,
                      lhv.gov, lhv.private, lhv.srhc,
                      lhv.village, lhv.eho, lhv.dry,
                      lhv.dry.lt15, lhv.dry.gteq15lt60,
                      lhv.dry.gteq60lt120, lhv.dry.gteq120lt360,
                      lhv.dry.gteq360lt720, lhv.dry.gteq720lt1440,
                      lhv.dry.gteq1440, lhv.wet,
                      lhv.wet.lt15, lhv.wet.gteq15lt60,
                      lhv.wet.gteq60lt120, lhv.wet.gteq120lt360,
                      lhv.wet.gteq360lt720, lhv.wet.gteq720lt1440,
                      lhv.wet.gteq1440, lhv.visit,
                      lhv.visit.first, lhv.visit.second,
                      lhv.visit.third)
  }
  ## Return data.frame
  return(lhv)
}


################################################################################
#
#' Function to recode the midwife ANC provider data for the Myanmar MCCT
#' Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded midwife ANC provider information
#'
#' @examples
#' ## Recode midwife provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_midwife(df = x, status = "current")
#'
#' ## Recode midwife provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_midwife(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_midwife <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend midwife
    midwife <- as.integer(df$ancnow_who.7)
    ## Where midwife
    midwife.where <- as.integer(df$ancnow_mw_where)
    midwife.home <- bbw::recode(var = midwife.where,
                                recodes = "1=1;NA=NA;else=0")
    midwife.gov <- bbw::recode(var = midwife.where,
                               recodes = "2=1;NA=NA;else=0")
    midwife.private <- bbw::recode(var = midwife.where,
                                   recodes = "3=1;NA=NA;else=0")
    midwife.srhc <- bbw::recode(var = midwife.where,
                                recodes = "4=1;NA=NA;else=0")
    midwife.village <- bbw::recode(var = midwife.where,
                                   recodes = "5=1;NA=NA;else=0")
    midwife.eho <- bbw::recode(var = midwife.where,
                               recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    midwife.dry <- as.integer(df$ancnow_mw_dist_dry)
    midwife.dry.lt15 <- bbw::recode(var = midwife.dry,
                                    recodes = "1=1;NA=NA;else=0")
    midwife.dry.gteq15lt60 <- bbw::recode(var = midwife.dry,
                                          recodes = "2=1;NA=NA;else=0")
    midwife.dry.gteq60lt120 <- bbw::recode(var = midwife.dry,
                                           recodes = "3=1;NA=NA;else=0")
    midwife.dry.gteq120lt360 <- bbw::recode(var = midwife.dry,
                                            recodes = "4=1;NA=NA;else=0")
    midwife.dry.gteq360lt720 <- bbw::recode(var = midwife.dry,
                                            recodes = "5=1;NA=NA;else=0")
    midwife.dry.gteq720lt1440 <- bbw::recode(var = midwife.dry,
                                             recodes = "6=1;NA=NA;else=0")
    midwife.dry.gteq1440 <- bbw::recode(var = midwife.dry,
                                        recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    midwife.wet <- as.integer(df$ancnow_mw_dist_wet)
    midwife.wet.lt15 <- bbw::recode(var = midwife.wet,
                                    recodes = "1=1;NA=NA;else=0")
    midwife.wet.gteq15lt60 <- bbw::recode(var = midwife.wet,
                                          recodes = "2=1;NA=NA;else=0")
    midwife.wet.gteq60lt120 <- bbw::recode(var = midwife.wet,
                                           recodes = "3=1;NA=NA;else=0")
    midwife.wet.gteq120lt360 <- bbw::recode(var = midwife.wet,
                                            recodes = "4=1;NA=NA;else=0")
    midwife.wet.gteq360lt720 <- bbw::recode(var = midwife.wet,
                                            recodes = "5=1;NA=NA;else=0")
    midwife.wet.gteq720lt1440 <- bbw::recode(var = midwife.wet,
                                             recodes = "6=1;NA=NA;else=0")
    midwife.wet.gteq1440 <- bbw::recode(var = midwife.wet,
                                        recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    midwife.visit <- as.integer(df$ancnow_mw_visit)
    ## Number of visits per trimester
    midwife.visit.first <- as.integer(df$ancnow_mw_1tri_times)
    midwife.visit.second <- as.integer(df$ancnow_mw_2tri_times)
    midwife.visit.third <- as.integer(df$ancnow_mw_3tri_times)
    ## Concatenate midwife provider data.frame
    midwife <- data.frame(midwife, midwife.where, midwife.home,
                          midwife.gov, midwife.private, midwife.srhc,
                          midwife.village, midwife.eho, midwife.dry,
                          midwife.dry.lt15, midwife.dry.gteq15lt60,
                          midwife.dry.gteq60lt120, midwife.dry.gteq120lt360,
                          midwife.dry.gteq360lt720, midwife.dry.gteq720lt1440,
                          midwife.dry.gteq1440, midwife.wet,
                          midwife.wet.lt15, midwife.wet.gteq15lt60,
                          midwife.wet.gteq60lt120, midwife.wet.gteq120lt360,
                          midwife.wet.gteq360lt720, midwife.wet.gteq720lt1440,
                          midwife.wet.gteq1440, midwife.visit,
                          midwife.visit.first, midwife.visit.second,
                          midwife.visit.third)
  } else {
    ## Attend midwife
    midwife <- as.integer(df$ancpast_who.7)
    ## Where midwife
    midwife.where <- as.integer(df$ancpast_mw_where)
    midwife.home <- bbw::recode(var = midwife.where,
                                recodes = "1=1;NA=NA;else=0")
    midwife.gov <- bbw::recode(var = midwife.where,
                               recodes = "2=1;NA=NA;else=0")
    midwife.private <- bbw::recode(var = midwife.where,
                                   recodes = "3=1;NA=NA;else=0")
    midwife.srhc <- bbw::recode(var = midwife.where,
                                recodes = "4=1;NA=NA;else=0")
    midwife.village <- bbw::recode(var = midwife.where,
                                   recodes = "5=1;NA=NA;else=0")
    midwife.eho <- bbw::recode(var = midwife.where,
                               recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    midwife.dry <- as.integer(df$ancpast_mw_dist_dry)
    midwife.dry.lt15 <- bbw::recode(var = midwife.dry,
                                    recodes = "1=1;NA=NA;else=0")
    midwife.dry.gteq15lt60 <- bbw::recode(var = midwife.dry,
                                          recodes = "2=1;NA=NA;else=0")
    midwife.dry.gteq60lt120 <- bbw::recode(var = midwife.dry,
                                           recodes = "3=1;NA=NA;else=0")
    midwife.dry.gteq120lt360 <- bbw::recode(var = midwife.dry,
                                            recodes = "4=1;NA=NA;else=0")
    midwife.dry.gteq360lt720 <- bbw::recode(var = midwife.dry,
                                            recodes = "5=1;NA=NA;else=0")
    midwife.dry.gteq720lt1440 <- bbw::recode(var = midwife.dry,
                                             recodes = "6=1;NA=NA;else=0")
    midwife.dry.gteq1440 <- bbw::recode(var = midwife.dry,
                                        recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    midwife.wet <- as.integer(df$ancpast_mw_dist_wet)
    midwife.wet.lt15 <- bbw::recode(var = midwife.wet,
                                    recodes = "1=1;NA=NA;else=0")
    midwife.wet.gteq15lt60 <- bbw::recode(var = midwife.wet,
                                          recodes = "2=1;NA=NA;else=0")
    midwife.wet.gteq60lt120 <- bbw::recode(var = midwife.wet,
                                           recodes = "3=1;NA=NA;else=0")
    midwife.wet.gteq120lt360 <- bbw::recode(var = midwife.wet,
                                            recodes = "4=1;NA=NA;else=0")
    midwife.wet.gteq360lt720 <- bbw::recode(var = midwife.wet,
                                            recodes = "5=1;NA=NA;else=0")
    midwife.wet.gteq720lt1440 <- bbw::recode(var = midwife.wet,
                                             recodes = "6=1;NA=NA;else=0")
    midwife.wet.gteq1440 <- bbw::recode(var = midwife.wet,
                                        recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    midwife.visit <- as.integer(df$ancpast_mw_visit)
    ## Number of visits per trimester
    midwife.visit.first <- as.integer(df$ancpast_mw_1tri_times)
    midwife.visit.second <- as.integer(df$ancpast_mw_2tri_times)
    midwife.visit.third <- as.integer(df$ancpast_mw_3tri_times)
    ## Concatenate midwife provider data.frame
    midwife <- data.frame(midwife, midwife.where, midwife.home,
                          midwife.gov, midwife.private, midwife.srhc,
                          midwife.village, midwife.eho, midwife.dry,
                          midwife.dry.lt15, midwife.dry.gteq15lt60,
                          midwife.dry.gteq60lt120, midwife.dry.gteq120lt360,
                          midwife.dry.gteq360lt720, midwife.dry.gteq720lt1440,
                          midwife.dry.gteq1440, midwife.wet,
                          midwife.wet.lt15, midwife.wet.gteq15lt60,
                          midwife.wet.gteq60lt120, midwife.wet.gteq120lt360,
                          midwife.wet.gteq360lt720, midwife.wet.gteq720lt1440,
                          midwife.wet.gteq1440, midwife.visit,
                          midwife.visit.first, midwife.visit.second,
                          midwife.visit.third)
  }
  ## Return data.frame
  return(midwife)
}


################################################################################
#
#' Function to recode the auxilliary midwife ANC provider data for the Myanmar
#' MCCT Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded auxilliary midwife ANC provider information
#'
#' @examples
#' ## Recode auxilliary midwife for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_amw(df = x, status = "current")
#'
#' ## Recode auxilliary midwife for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_amw(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_amw <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend amw
    amw <- as.integer(df$ancnow_who.8)
    ## Where amw
    amw.where <- as.integer(df$ancnow_amw_where)
    amw.home <- bbw::recode(var = amw.where, recodes = "1=1;NA=NA;else=0")
    amw.gov <- bbw::recode(var = amw.where, recodes = "2=1;NA=NA;else=0")
    amw.private <- bbw::recode(var = amw.where, recodes = "3=1;NA=NA;else=0")
    amw.srhc <- bbw::recode(var = amw.where, recodes = "4=1;NA=NA;else=0")
    amw.village <- bbw::recode(var = amw.where, recodes = "5=1;NA=NA;else=0")
    amw.eho <- bbw::recode(var = amw.where, recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    amw.dry <- as.integer(df$ancnow_amw_dist_dry)
    amw.dry.lt15 <- bbw::recode(var = amw.dry,
                                recodes = "1=1;NA=NA;else=0")
    amw.dry.gteq15lt60 <- bbw::recode(var = amw.dry,
                                      recodes = "2=1;NA=NA;else=0")
    amw.dry.gteq60lt120 <- bbw::recode(var = amw.dry,
                                       recodes = "3=1;NA=NA;else=0")
    amw.dry.gteq120lt360 <- bbw::recode(var = amw.dry,
                                        recodes = "4=1;NA=NA;else=0")
    amw.dry.gteq360lt720 <- bbw::recode(var = amw.dry,
                                        recodes = "5=1;NA=NA;else=0")
    amw.dry.gteq720lt1440 <- bbw::recode(var = amw.dry,
                                         recodes = "6=1;NA=NA;else=0")
    amw.dry.gteq1440 <- bbw::recode(var = amw.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    amw.wet <- as.integer(df$ancnow_amw_dist_wet)
    amw.wet.lt15 <- bbw::recode(var = amw.wet,
                                recodes = "1=1;NA=NA;else=0")
    amw.wet.gteq15lt60 <- bbw::recode(var = amw.wet,
                                      recodes = "2=1;NA=NA;else=0")
    amw.wet.gteq60lt120 <- bbw::recode(var = amw.wet,
                                       recodes = "3=1;NA=NA;else=0")
    amw.wet.gteq120lt360 <- bbw::recode(var = amw.wet,
                                        recodes = "4=1;NA=NA;else=0")
    amw.wet.gteq360lt720 <- bbw::recode(var = amw.wet,
                                        recodes = "5=1;NA=NA;else=0")
    amw.wet.gteq720lt1440 <- bbw::recode(var = amw.wet,
                                         recodes = "6=1;NA=NA;else=0")
    amw.wet.gteq1440 <- bbw::recode(var = amw.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    amw.visit <- as.integer(df$ancnow_amw_visit)
    ## Number of visits per trimester
    amw.visit.first <- as.integer(df$ancnow_amw_1tri_times)
    amw.visit.second <- as.integer(df$ancnow_amw_2tri_times)
    amw.visit.third <- as.integer(df$ancnow_amw_3tri_times)
    ## Concatenate amw provider data.frame
    amw <- data.frame(amw, amw.where, amw.home,
                      amw.gov, amw.private, amw.srhc,
                      amw.village, amw.eho, amw.dry,
                      amw.dry.lt15, amw.dry.gteq15lt60,
                      amw.dry.gteq60lt120, amw.dry.gteq120lt360,
                      amw.dry.gteq360lt720, amw.dry.gteq720lt1440,
                      amw.dry.gteq1440, amw.wet,
                      amw.wet.lt15, amw.wet.gteq15lt60,
                      amw.wet.gteq60lt120, amw.wet.gteq120lt360,
                      amw.wet.gteq360lt720, amw.wet.gteq720lt1440,
                      amw.wet.gteq1440, amw.visit,
                      amw.visit.first, amw.visit.second,
                      amw.visit.third)
  } else {
    ## Attend amw
    amw <- as.integer(df$ancpast_who.8)
    ## Where amw
    amw.where <- as.integer(df$ancpast_amw_where)
    amw.home <- bbw::recode(var = amw.where, recodes = "1=1;NA=NA;else=0")
    amw.gov <- bbw::recode(var = amw.where, recodes = "2=1;NA=NA;else=0")
    amw.private <- bbw::recode(var = amw.where, recodes = "3=1;NA=NA;else=0")
    amw.srhc <- bbw::recode(var = amw.where, recodes = "4=1;NA=NA;else=0")
    amw.village <- bbw::recode(var = amw.where, recodes = "5=1;NA=NA;else=0")
    amw.eho <- bbw::recode(var = amw.where, recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    amw.dry <- as.integer(df$ancpast_amw_dist_dry)
    amw.dry.lt15 <- bbw::recode(var = amw.dry,
                                recodes = "1=1;NA=NA;else=0")
    amw.dry.gteq15lt60 <- bbw::recode(var = amw.dry,
                                      recodes = "2=1;NA=NA;else=0")
    amw.dry.gteq60lt120 <- bbw::recode(var = amw.dry,
                                       recodes = "3=1;NA=NA;else=0")
    amw.dry.gteq120lt360 <- bbw::recode(var = amw.dry,
                                        recodes = "4=1;NA=NA;else=0")
    amw.dry.gteq360lt720 <- bbw::recode(var = amw.dry,
                                        recodes = "5=1;NA=NA;else=0")
    amw.dry.gteq720lt1440 <- bbw::recode(var = amw.dry,
                                         recodes = "6=1;NA=NA;else=0")
    amw.dry.gteq1440 <- bbw::recode(var = amw.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    amw.wet <- as.integer(df$ancpast_amw_dist_wet)
    amw.wet.lt15 <- bbw::recode(var = amw.wet,
                                recodes = "1=1;NA=NA;else=0")
    amw.wet.gteq15lt60 <- bbw::recode(var = amw.wet,
                                      recodes = "2=1;NA=NA;else=0")
    amw.wet.gteq60lt120 <- bbw::recode(var = amw.wet,
                                       recodes = "3=1;NA=NA;else=0")
    amw.wet.gteq120lt360 <- bbw::recode(var = amw.wet,
                                        recodes = "4=1;NA=NA;else=0")
    amw.wet.gteq360lt720 <- bbw::recode(var = amw.wet,
                                        recodes = "5=1;NA=NA;else=0")
    amw.wet.gteq720lt1440 <- bbw::recode(var = amw.wet,
                                         recodes = "6=1;NA=NA;else=0")
    amw.wet.gteq1440 <- bbw::recode(var = amw.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    amw.visit <- as.integer(df$ancpast_amw_visit)
    ## Number of visits per trimester
    amw.visit.first <- as.integer(df$ancpast_amw_1tri_times)
    amw.visit.second <- as.integer(df$ancpast_amw_2tri_times)
    amw.visit.third <- as.integer(df$ancpast_amw_3tri_times)
    ## Concatenate amw provider data.frame
    amw <- data.frame(amw, amw.where, amw.home,
                      amw.gov, amw.private, amw.srhc,
                      amw.village, amw.eho, amw.dry,
                      amw.dry.lt15, amw.dry.gteq15lt60,
                      amw.dry.gteq60lt120, amw.dry.gteq120lt360,
                      amw.dry.gteq360lt720, amw.dry.gteq720lt1440,
                      amw.dry.gteq1440, amw.wet,
                      amw.wet.lt15, amw.wet.gteq15lt60,
                      amw.wet.gteq60lt120, amw.wet.gteq120lt360,
                      amw.wet.gteq360lt720, amw.wet.gteq720lt1440,
                      amw.wet.gteq1440, amw.visit,
                      amw.visit.first, amw.visit.second,
                      amw.visit.third)
  }
  ## Return data.frame
  return(amw)
}


################################################################################
#
#' Function to recode the traditional birth attendant ANC provider data for the
#' Myanmar MCCT Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded traditional birth attendant ANC provider
#'   information
#'
#' @examples
#' ## Recode tba provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_tba(df = x, status = "current")
#'
#' ## Recode tba provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_tba(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_tba <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend tba
    tba <- as.integer(df$ancnow_who.9)
    ## Where tba
    tba.where <- as.integer(df$ancnow_tba_where)
    tba.home <- bbw::recode(var = tba.where, recodes = "1=1;NA=NA;else=0")
    tba.gov <- bbw::recode(var = tba.where, recodes = "2=1;NA=NA;else=0")
    tba.private <- bbw::recode(var = tba.where, recodes = "3=1;NA=NA;else=0")
    tba.srhc <- bbw::recode(var = tba.where, recodes = "4=1;NA=NA;else=0")
    tba.village <- bbw::recode(var = tba.where, recodes = "5=1;NA=NA;else=0")
    tba.eho <- bbw::recode(var = tba.where, recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    tba.dry <- as.integer(df$ancnow_tba_dist_dry)
    tba.dry.lt15 <- bbw::recode(var = tba.dry,
                                recodes = "1=1;NA=NA;else=0")
    tba.dry.gteq15lt60 <- bbw::recode(var = tba.dry,
                                      recodes = "2=1;NA=NA;else=0")
    tba.dry.gteq60lt120 <- bbw::recode(var = tba.dry,
                                       recodes = "3=1;NA=NA;else=0")
    tba.dry.gteq120lt360 <- bbw::recode(var = tba.dry,
                                        recodes = "4=1;NA=NA;else=0")
    tba.dry.gteq360lt720 <- bbw::recode(var = tba.dry,
                                        recodes = "5=1;NA=NA;else=0")
    tba.dry.gteq720lt1440 <- bbw::recode(var = tba.dry,
                                         recodes = "6=1;NA=NA;else=0")
    tba.dry.gteq1440 <- bbw::recode(var = tba.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    tba.wet <- as.integer(df$ancnow_tba_dist_wet)
    tba.wet.lt15 <- bbw::recode(var = tba.wet,
                                recodes = "1=1;NA=NA;else=0")
    tba.wet.gteq15lt60 <- bbw::recode(var = tba.wet,
                                      recodes = "2=1;NA=NA;else=0")
    tba.wet.gteq60lt120 <- bbw::recode(var = tba.wet,
                                       recodes = "3=1;NA=NA;else=0")
    tba.wet.gteq120lt360 <- bbw::recode(var = tba.wet,
                                        recodes = "4=1;NA=NA;else=0")
    tba.wet.gteq360lt720 <- bbw::recode(var = tba.wet,
                                        recodes = "5=1;NA=NA;else=0")
    tba.wet.gteq720lt1440 <- bbw::recode(var = tba.wet,
                                         recodes = "6=1;NA=NA;else=0")
    tba.wet.gteq1440 <- bbw::recode(var = tba.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    tba.visit <- as.integer(df$ancnow_tba_visit)
    ## Number of visits per trimester
    tba.visit.first <- as.integer(df$ancnow_tba_1tri_times)
    tba.visit.second <- as.integer(df$ancnow_tba_2tri_times)
    tba.visit.third <- as.integer(df$ancnow_tba_3tri_times)
    ## Concatenate tba provider data.frame
    tba <- data.frame(tba, tba.where, tba.home,
                      tba.gov, tba.private, tba.srhc,
                      tba.village, tba.eho, tba.dry,
                      tba.dry.lt15, tba.dry.gteq15lt60,
                      tba.dry.gteq60lt120, tba.dry.gteq120lt360,
                      tba.dry.gteq360lt720, tba.dry.gteq720lt1440,
                      tba.dry.gteq1440, tba.wet,
                      tba.wet.lt15, tba.wet.gteq15lt60,
                      tba.wet.gteq60lt120, tba.wet.gteq120lt360,
                      tba.wet.gteq360lt720, tba.wet.gteq720lt1440,
                      tba.wet.gteq1440, tba.visit,
                      tba.visit.first, tba.visit.second,
                      tba.visit.third)
  } else {
    ## Attend tba
    tba <- as.integer(df$ancpast_who.9)
    ## Where tba
    tba.where <- as.integer(df$ancpast_tba_where)
    tba.home <- bbw::recode(var = tba.where, recodes = "1=1;NA=NA;else=0")
    tba.gov <- bbw::recode(var = tba.where, recodes = "2=1;NA=NA;else=0")
    tba.private <- bbw::recode(var = tba.where, recodes = "3=1;NA=NA;else=0")
    tba.srhc <- bbw::recode(var = tba.where, recodes = "4=1;NA=NA;else=0")
    tba.village <- bbw::recode(var = tba.where, recodes = "5=1;NA=NA;else=0")
    tba.eho <- bbw::recode(var = tba.where, recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    tba.dry <- as.integer(df$ancpast_tba_dist_dry)
    tba.dry.lt15 <- bbw::recode(var = tba.dry,
                                recodes = "1=1;NA=NA;else=0")
    tba.dry.gteq15lt60 <- bbw::recode(var = tba.dry,
                                      recodes = "2=1;NA=NA;else=0")
    tba.dry.gteq60lt120 <- bbw::recode(var = tba.dry,
                                       recodes = "3=1;NA=NA;else=0")
    tba.dry.gteq120lt360 <- bbw::recode(var = tba.dry,
                                        recodes = "4=1;NA=NA;else=0")
    tba.dry.gteq360lt720 <- bbw::recode(var = tba.dry,
                                        recodes = "5=1;NA=NA;else=0")
    tba.dry.gteq720lt1440 <- bbw::recode(var = tba.dry,
                                         recodes = "6=1;NA=NA;else=0")
    tba.dry.gteq1440 <- bbw::recode(var = tba.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    tba.wet <- as.integer(df$ancpast_tba_dist_wet)
    tba.wet.lt15 <- bbw::recode(var = tba.wet,
                                recodes = "1=1;NA=NA;else=0")
    tba.wet.gteq15lt60 <- bbw::recode(var = tba.wet,
                                      recodes = "2=1;NA=NA;else=0")
    tba.wet.gteq60lt120 <- bbw::recode(var = tba.wet,
                                       recodes = "3=1;NA=NA;else=0")
    tba.wet.gteq120lt360 <- bbw::recode(var = tba.wet,
                                        recodes = "4=1;NA=NA;else=0")
    tba.wet.gteq360lt720 <- bbw::recode(var = tba.wet,
                                        recodes = "5=1;NA=NA;else=0")
    tba.wet.gteq720lt1440 <- bbw::recode(var = tba.wet,
                                         recodes = "6=1;NA=NA;else=0")
    tba.wet.gteq1440 <- bbw::recode(var = tba.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    tba.visit <- as.integer(df$ancpast_tba_visit)
    ## Number of visits per trimester
    tba.visit.first <- as.integer(df$ancpast_tba_1tri_times)
    tba.visit.second <- as.integer(df$ancpast_tba_2tri_times)
    tba.visit.third <- as.integer(df$ancpast_tba_3tri_times)
    ## Concatenate tba provider data.frame
    tba <- data.frame(tba, tba.where, tba.home,
                      tba.gov, tba.private, tba.srhc,
                      tba.village, tba.eho, tba.dry,
                      tba.dry.lt15, tba.dry.gteq15lt60,
                      tba.dry.gteq60lt120, tba.dry.gteq120lt360,
                      tba.dry.gteq360lt720, tba.dry.gteq720lt1440,
                      tba.dry.gteq1440, tba.wet,
                      tba.wet.lt15, tba.wet.gteq15lt60,
                      tba.wet.gteq60lt120, tba.wet.gteq120lt360,
                      tba.wet.gteq360lt720, tba.wet.gteq720lt1440,
                      tba.wet.gteq1440, tba.visit,
                      tba.visit.first, tba.visit.second,
                      tba.visit.third)
  }
  ## Return data.frame
  return(tba)
}


################################################################################
#
#' Function to recode the community health worker ANC provider data for the
#' Myanmar MCCT Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded community health worker ANC provider
#'   information
#'
#' @examples
#' ## Recode chw provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_chw(df = x, status = "current")
#'
#' ## Recode chw provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_chw(df = x, status = "past")
#' @export
#'
#'
#
################################################################################

recode_provider_chw <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend chw
    chw <- as.integer(df$ancnow_who.10)
    ## Where chw
    chw.where <- as.integer(df$ancnow_chw_where)
    chw.home <- bbw::recode(var = chw.where, recodes = "1=1;NA=NA;else=0")
    chw.gov <- bbw::recode(var = chw.where, recodes = "2=1;NA=NA;else=0")
    chw.private <- bbw::recode(var = chw.where, recodes = "3=1;NA=NA;else=0")
    chw.srhc <- bbw::recode(var = chw.where, recodes = "4=1;NA=NA;else=0")
    chw.village <- bbw::recode(var = chw.where, recodes = "5=1;NA=NA;else=0")
    chw.eho <- bbw::recode(var = chw.where, recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    chw.dry <- as.integer(df$ancnow_chw_dist_dry)
    chw.dry.lt15 <- bbw::recode(var = chw.dry,
                                recodes = "1=1;NA=NA;else=0")
    chw.dry.gteq15lt60 <- bbw::recode(var = chw.dry,
                                      recodes = "2=1;NA=NA;else=0")
    chw.dry.gteq60lt120 <- bbw::recode(var = chw.dry,
                                       recodes = "3=1;NA=NA;else=0")
    chw.dry.gteq120lt360 <- bbw::recode(var = chw.dry,
                                        recodes = "4=1;NA=NA;else=0")
    chw.dry.gteq360lt720 <- bbw::recode(var = chw.dry,
                                        recodes = "5=1;NA=NA;else=0")
    chw.dry.gteq720lt1440 <- bbw::recode(var = chw.dry,
                                         recodes = "6=1;NA=NA;else=0")
    chw.dry.gteq1440 <- bbw::recode(var = chw.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    chw.wet <- as.integer(df$ancnow_chw_dist_wet)
    chw.wet.lt15 <- bbw::recode(var = chw.wet,
                                recodes = "1=1;NA=NA;else=0")
    chw.wet.gteq15lt60 <- bbw::recode(var = chw.wet,
                                      recodes = "2=1;NA=NA;else=0")
    chw.wet.gteq60lt120 <- bbw::recode(var = chw.wet,
                                       recodes = "3=1;NA=NA;else=0")
    chw.wet.gteq120lt360 <- bbw::recode(var = chw.wet,
                                        recodes = "4=1;NA=NA;else=0")
    chw.wet.gteq360lt720 <- bbw::recode(var = chw.wet,
                                        recodes = "5=1;NA=NA;else=0")
    chw.wet.gteq720lt1440 <- bbw::recode(var = chw.wet,
                                         recodes = "6=1;NA=NA;else=0")
    chw.wet.gteq1440 <- bbw::recode(var = chw.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    chw.visit <- as.integer(df$ancnow_chw_visit)
    ## Number of visits per trimester
    chw.visit.first <- as.integer(df$ancnow_chw_1tri_times)
    chw.visit.second <- as.integer(df$ancnow_chw_2tri_times)
    chw.visit.third <- as.integer(df$ancnow_chw_3tri_times)
    ## Concatenate chw provider data.frame
    chw <- data.frame(chw, chw.where, chw.home,
                      chw.gov, chw.private, chw.srhc,
                      chw.village, chw.eho, chw.dry,
                      chw.dry.lt15, chw.dry.gteq15lt60,
                      chw.dry.gteq60lt120, chw.dry.gteq120lt360,
                      chw.dry.gteq360lt720, chw.dry.gteq720lt1440,
                      chw.dry.gteq1440, chw.wet,
                      chw.wet.lt15, chw.wet.gteq15lt60,
                      chw.wet.gteq60lt120, chw.wet.gteq120lt360,
                      chw.wet.gteq360lt720, chw.wet.gteq720lt1440,
                      chw.wet.gteq1440, chw.visit,
                      chw.visit.first, chw.visit.second,
                      chw.visit.third)
  } else {
    ## Attend chw
    chw <- as.integer(df$ancpast_who.10)
    ## Where chw
    chw.where <- as.integer(df$ancpast_chw_where)
    chw.home <- bbw::recode(var = chw.where, recodes = "1=1;NA=NA;else=0")
    chw.gov <- bbw::recode(var = chw.where, recodes = "2=1;NA=NA;else=0")
    chw.private <- bbw::recode(var = chw.where, recodes = "3=1;NA=NA;else=0")
    chw.srhc <- bbw::recode(var = chw.where, recodes = "4=1;NA=NA;else=0")
    chw.village <- bbw::recode(var = chw.where, recodes = "5=1;NA=NA;else=0")
    chw.eho <- bbw::recode(var = chw.where, recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    chw.dry <- as.integer(df$ancpast_chw_dist_dry)
    chw.dry.lt15 <- bbw::recode(var = chw.dry,
                                recodes = "1=1;NA=NA;else=0")
    chw.dry.gteq15lt60 <- bbw::recode(var = chw.dry,
                                      recodes = "2=1;NA=NA;else=0")
    chw.dry.gteq60lt120 <- bbw::recode(var = chw.dry,
                                       recodes = "3=1;NA=NA;else=0")
    chw.dry.gteq120lt360 <- bbw::recode(var = chw.dry,
                                        recodes = "4=1;NA=NA;else=0")
    chw.dry.gteq360lt720 <- bbw::recode(var = chw.dry,
                                        recodes = "5=1;NA=NA;else=0")
    chw.dry.gteq720lt1440 <- bbw::recode(var = chw.dry,
                                         recodes = "6=1;NA=NA;else=0")
    chw.dry.gteq1440 <- bbw::recode(var = chw.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    chw.wet <- as.integer(df$ancpast_chw_dist_wet)
    chw.wet.lt15 <- bbw::recode(var = chw.wet,
                                recodes = "1=1;NA=NA;else=0")
    chw.wet.gteq15lt60 <- bbw::recode(var = chw.wet,
                                      recodes = "2=1;NA=NA;else=0")
    chw.wet.gteq60lt120 <- bbw::recode(var = chw.wet,
                                       recodes = "3=1;NA=NA;else=0")
    chw.wet.gteq120lt360 <- bbw::recode(var = chw.wet,
                                        recodes = "4=1;NA=NA;else=0")
    chw.wet.gteq360lt720 <- bbw::recode(var = chw.wet,
                                        recodes = "5=1;NA=NA;else=0")
    chw.wet.gteq720lt1440 <- bbw::recode(var = chw.wet,
                                         recodes = "6=1;NA=NA;else=0")
    chw.wet.gteq1440 <- bbw::recode(var = chw.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    chw.visit <- as.integer(df$ancpast_chw_visit)
    ## Number of visits per trimester
    chw.visit.first <- as.integer(df$ancpast_chw_1tri_times)
    chw.visit.second <- as.integer(df$ancpast_chw_2tri_times)
    chw.visit.third <- as.integer(df$ancpast_chw_3tri_times)
    ## Concatenate chw provider data.frame
    chw <- data.frame(chw, chw.where, chw.home,
                      chw.gov, chw.private, chw.srhc,
                      chw.village, chw.eho, chw.dry,
                      chw.dry.lt15, chw.dry.gteq15lt60,
                      chw.dry.gteq60lt120, chw.dry.gteq120lt360,
                      chw.dry.gteq360lt720, chw.dry.gteq720lt1440,
                      chw.dry.gteq1440, chw.wet,
                      chw.wet.lt15, chw.wet.gteq15lt60,
                      chw.wet.gteq60lt120, chw.wet.gteq120lt360,
                      chw.wet.gteq360lt720, chw.wet.gteq720lt1440,
                      chw.wet.gteq1440, chw.visit,
                      chw.visit.first, chw.visit.second,
                      chw.visit.third)
  }
  ## Return data.frame
  return(chw)
}


################################################################################
#
#' Function to recode the ethnic health worker ANC provider data for the Myanmar
#' MCCT Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded ethnic health worker ANC provider information
#'
#' @examples
#' ## Recode ehw provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_provider_ehw(df = x, status = "current")
#'
#' ## Recode ehw provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_provider_ehw(df = x, status = "past")
#'
#' @export
#'
#'
#
################################################################################

recode_provider_ehw <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Attend ehw
    ehw <- as.integer(df$ancnow_who.11)
    ## Where ehw
    ehw.where <- as.integer(df$ancnow_ehw_where)
    ehw.home <- bbw::recode(var = ehw.where, recodes = "1=1;NA=NA;else=0")
    ehw.gov <- bbw::recode(var = ehw.where, recodes = "2=1;NA=NA;else=0")
    ehw.private <- bbw::recode(var = ehw.where, recodes = "3=1;NA=NA;else=0")
    ehw.srhc <- bbw::recode(var = ehw.where, recodes = "4=1;NA=NA;else=0")
    ehw.village <- bbw::recode(var = ehw.where, recodes = "5=1;NA=NA;else=0")
    ehw.eho <- bbw::recode(var = ehw.where, recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    ehw.dry <- as.integer(df$ancnow_ehw_dist_dry)
    ehw.dry.lt15 <- bbw::recode(var = ehw.dry,
                                recodes = "1=1;NA=NA;else=0")
    ehw.dry.gteq15lt60 <- bbw::recode(var = ehw.dry,
                                      recodes = "2=1;NA=NA;else=0")
    ehw.dry.gteq60lt120 <- bbw::recode(var = ehw.dry,
                                       recodes = "3=1;NA=NA;else=0")
    ehw.dry.gteq120lt360 <- bbw::recode(var = ehw.dry,
                                        recodes = "4=1;NA=NA;else=0")
    ehw.dry.gteq360lt720 <- bbw::recode(var = ehw.dry,
                                        recodes = "5=1;NA=NA;else=0")
    ehw.dry.gteq720lt1440 <- bbw::recode(var = ehw.dry,
                                         recodes = "6=1;NA=NA;else=0")
    ehw.dry.gteq1440 <- bbw::recode(var = ehw.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    ehw.wet <- as.integer(df$ancnow_ehw_dist_wet)
    ehw.wet.lt15 <- bbw::recode(var = ehw.wet,
                                recodes = "1=1;NA=NA;else=0")
    ehw.wet.gteq15lt60 <- bbw::recode(var = ehw.wet,
                                      recodes = "2=1;NA=NA;else=0")
    ehw.wet.gteq60lt120 <- bbw::recode(var = ehw.wet,
                                       recodes = "3=1;NA=NA;else=0")
    ehw.wet.gteq120lt360 <- bbw::recode(var = ehw.wet,
                                        recodes = "4=1;NA=NA;else=0")
    ehw.wet.gteq360lt720 <- bbw::recode(var = ehw.wet,
                                        recodes = "5=1;NA=NA;else=0")
    ehw.wet.gteq720lt1440 <- bbw::recode(var = ehw.wet,
                                         recodes = "6=1;NA=NA;else=0")
    ehw.wet.gteq1440 <- bbw::recode(var = ehw.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    ehw.visit <- as.integer(df$ancnow_ehw_visit)
    ## Number of visits per trimester
    ehw.visit.first <- as.integer(df$ancnow_ehw_1tri_times)
    ehw.visit.second <- as.integer(df$ancnow_ehw_2tri_times)
    ehw.visit.third <- as.integer(df$ancnow_ehw_3tri_times)
    ## Concatenate ehw provider data.frame
    ehw <- data.frame(ehw, ehw.where, ehw.home,
                      ehw.gov, ehw.private, ehw.srhc,
                      ehw.village, ehw.eho, ehw.dry,
                      ehw.dry.lt15, ehw.dry.gteq15lt60,
                      ehw.dry.gteq60lt120, ehw.dry.gteq120lt360,
                      ehw.dry.gteq360lt720, ehw.dry.gteq720lt1440,
                      ehw.dry.gteq1440, ehw.wet,
                      ehw.wet.lt15, ehw.wet.gteq15lt60,
                      ehw.wet.gteq60lt120, ehw.wet.gteq120lt360,
                      ehw.wet.gteq360lt720, ehw.wet.gteq720lt1440,
                      ehw.wet.gteq1440, ehw.visit,
                      ehw.visit.first, ehw.visit.second,
                      ehw.visit.third)
  } else {
    ## Attend ehw
    ehw <- as.integer(df$ancpast_who.11)
    ## Where ehw
    ehw.where <- as.integer(df$ancpast_ehw_where)
    ehw.home <- bbw::recode(var = ehw.where, recodes = "1=1;NA=NA;else=0")
    ehw.gov <- bbw::recode(var = ehw.where, recodes = "2=1;NA=NA;else=0")
    ehw.private <- bbw::recode(var = ehw.where, recodes = "3=1;NA=NA;else=0")
    ehw.srhc <- bbw::recode(var = ehw.where, recodes = "4=1;NA=NA;else=0")
    ehw.village <- bbw::recode(var = ehw.where, recodes = "5=1;NA=NA;else=0")
    ehw.eho <- bbw::recode(var = ehw.where, recodes = "6=1;NA=NA;else=0")
    ## Recode others
    ## Distance dry season
    ehw.dry <- as.integer(df$ancpast_ehw_dist_dry)
    ehw.dry.lt15 <- bbw::recode(var = ehw.dry,
                                recodes = "1=1;NA=NA;else=0")
    ehw.dry.gteq15lt60 <- bbw::recode(var = ehw.dry,
                                      recodes = "2=1;NA=NA;else=0")
    ehw.dry.gteq60lt120 <- bbw::recode(var = ehw.dry,
                                       recodes = "3=1;NA=NA;else=0")
    ehw.dry.gteq120lt360 <- bbw::recode(var = ehw.dry,
                                        recodes = "4=1;NA=NA;else=0")
    ehw.dry.gteq360lt720 <- bbw::recode(var = ehw.dry,
                                        recodes = "5=1;NA=NA;else=0")
    ehw.dry.gteq720lt1440 <- bbw::recode(var = ehw.dry,
                                         recodes = "6=1;NA=NA;else=0")
    ehw.dry.gteq1440 <- bbw::recode(var = ehw.dry,
                                    recodes = "7=1;NA=NA;else=0")
    ## Distance wet season
    ehw.wet <- as.integer(df$ancpast_ehw_dist_wet)
    ehw.wet.lt15 <- bbw::recode(var = ehw.wet,
                                recodes = "1=1;NA=NA;else=0")
    ehw.wet.gteq15lt60 <- bbw::recode(var = ehw.wet,
                                      recodes = "2=1;NA=NA;else=0")
    ehw.wet.gteq60lt120 <- bbw::recode(var = ehw.wet,
                                       recodes = "3=1;NA=NA;else=0")
    ehw.wet.gteq120lt360 <- bbw::recode(var = ehw.wet,
                                        recodes = "4=1;NA=NA;else=0")
    ehw.wet.gteq360lt720 <- bbw::recode(var = ehw.wet,
                                        recodes = "5=1;NA=NA;else=0")
    ehw.wet.gteq720lt1440 <- bbw::recode(var = ehw.wet,
                                         recodes = "6=1;NA=NA;else=0")
    ehw.wet.gteq1440 <- bbw::recode(var = ehw.wet,
                                    recodes = "7=1;NA=NA;else=0")
    ## Number of visits
    ehw.visit <- as.integer(df$ancpast_ehw_visit)
    ## Number of visits per trimester
    ehw.visit.first <- as.integer(df$ancpast_ehw_1tri_times)
    ehw.visit.second <- as.integer(df$ancpast_ehw_2tri_times)
    ehw.visit.third <- as.integer(df$ancpast_ehw_3tri_times)
    ## Concatenate ehw provider data.frame
    ehw <- data.frame(ehw, ehw.where, ehw.home,
                      ehw.gov, ehw.private, ehw.srhc,
                      ehw.village, ehw.eho, ehw.dry,
                      ehw.dry.lt15, ehw.dry.gteq15lt60,
                      ehw.dry.gteq60lt120, ehw.dry.gteq120lt360,
                      ehw.dry.gteq360lt720, ehw.dry.gteq720lt1440,
                      ehw.dry.gteq1440, ehw.wet,
                      ehw.wet.lt15, ehw.wet.gteq15lt60,
                      ehw.wet.gteq60lt120, ehw.wet.gteq120lt360,
                      ehw.wet.gteq360lt720, ehw.wet.gteq720lt1440,
                      ehw.wet.gteq1440, ehw.visit,
                      ehw.visit.first, ehw.visit.second,
                      ehw.visit.third)
  }
  ## Return data.frame
  return(ehw)
}


################################################################################
#
#' Function to recode the personnel type providing antenatal care services for
#' the Myanmar MCCT Programme Evaluation survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data for currently pregnant women
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded antenatal care providers visited for
#'   antenatal care
#'
#' @examples
#' ## Recode anc provider for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_anc_provider(df = x, status = "current")
#'
#' ## Recode anc provider for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_anc_provider(df = x, status = "past")
#'
#' @export
#'
#
################################################################################

recode_anc_provider <- function(df, status = NULL) {
  ## Specialist
  specialist <- recode_provider_specialist(df = df, status = status)
  ## Doctor
  doctor <- recode_provider_doctor(df = df, status = status)
  ## Nurse
  nurse <- recode_provider_nurse(df = df, status = status)
  ## Health assistant
  ha <- recode_provider_ha(df = df, status = status)
  ## Private doctor
  pdoc <- recode_provider_pdoc(df = df, status = status)
  ## Lady health visitor
  lhv <- recode_provider_lhv(df = df, status = status)
  ## Midwife
  midwife <- recode_provider_midwife(df = df, status = status)
  ## Assistant midwife
  amw <- recode_provider_amw(df = df, status = status)
  ## Traditional birth attendant
  tba <- recode_provider_tba(df = df, status = status)
  ## Community health worker
  chw <- recode_provider_chw(df = df, status = status)
  ## Ethnic health worker
  ehw <- recode_provider_ehw(df = df, status = status)
  ## Determine status
  if(status == "current") {
    ## Provider costs?
    incurCost <- as.integer(df$ancnow_cost)
    ## Amount
    amountCost <- as.integer(df$ancnow_amount)
    ## Item cost: transportation
    transportCost <- as.integer(df$ancnow_costitem.1)
    transportCost <- ifelse(is.na(incurCost) |
                              incurCost == 0, NA, transportCost)
    ## Item cost: registration fees
    registrationCost <- as.integer(df$ancnow_costitem.2)
    registrationCost <- ifelse(is.na(incurCost) |
                                 incurCost == 0, NA, registrationCost)
    ## Item cost: medicines
    medicineCost <- as.integer(df$ancnow_costitem.3)
    medicineCost <- ifelse(is.na(incurCost) |
                             incurCost == 0, NA, medicineCost)
    ## Item cost: laboratory fees
    labCost <- as.integer(df$ancnow_costitem.4)
    labCost <- ifelse(is.na(incurCost) | incurCost == 0, NA, labCost)
    ## Item cost: provider fees
    providerCost <- as.integer(df$ancnow_costitem.5)
    providerCost <- ifelse(is.na(incurCost) |
                             incurCost == 0, NA, providerCost)
    ## Item cost: gifts
    giftCost <- as.integer(df$ancnow_costitem.6)
    giftCost <- ifelse(is.na(incurCost) |
                         incurCost == 0, NA, giftCost)
    ## Borrow money?
    borrow <- as.integer(df$ancnow_borrow)
  } else {
    ## Provider costs?
    incurCost <- as.integer(df$ancpast_cost)
    ## Amount
    amountCost <- as.integer(df$ancpast_amount)
    ## Item cost: transportation
    transportCost <- as.integer(df$ancpast_costitem.1)
    transportCost <- ifelse(is.na(incurCost) |
                              incurCost == 0, NA, transportCost)
    ## Item cost: registration fees
    registrationCost <- as.integer(df$ancpast_costitem.2)
    registrationCost <- ifelse(is.na(incurCost) |
                                 incurCost == 0, NA, registrationCost)
    ## Item cost: medicines
    medicineCost <- as.integer(df$ancpast_costitem.3)
    medicineCost <- ifelse(is.na(incurCost) |
                             incurCost == 0, NA, medicineCost)
    ## Item cost: laboratory fees
    labCost <- as.integer(df$ancpast_costitem.4)
    labCost <- ifelse(is.na(incurCost) |
                        incurCost == 0, NA, labCost)
    ## Item cost: provider fees
    providerCost <- as.integer(df$ancpast_costitem.5)
    providerCost <- ifelse(is.na(incurCost) |
                             incurCost == 0, NA, providerCost)
    ## Item cost: gifts
    giftCost <- as.integer(df$ancpast_costitem.6)
    giftCost <- ifelse(is.na(incurCost) |
                         incurCost == 0, NA, giftCost)
    ## Borrow money?
    borrow <- as.integer(df$ancpast_borrow)
  }
  ##
  anc1 <- rowSums(cbind(specialist$specialist.visit,
                        doctor$doctor.visit,
                        nurse$nurse.visit,
                        ha$ha.visit,
                        pdoc$pdoc.visit,
                        lhv$lhv.visit,
                        midwife$midwife.visit),
                  na.rm = TRUE)
  anc1 <- bbw::recode(var = anc1,
                      recodes = "1:hi=1;NA=NA;444=NA;448=NA;888=NA;else=0")
  ##
  anc4 <- rowSums(cbind(specialist$specialist.visit,
                        doctor$doctor.visit,
                        nurse$nurse.visit,
                        ha$ha.visit,
                        pdoc$pdoc.visit,
                        lhv$lhv.visit,
                        midwife$midwife.visit,
                        amw$amw.visit,
                        tba$tba.visit,
                        chw$chw.visit,
                        ehw$ehw.visit),
                  na.rm = TRUE)
  anc4 <- bbw::recode(var = anc4,
                      recodes = "4:hi=1;NA=NA;444=NA;448=NA;888=NA;1332=NA;else=0")
  ## Concatenate provider information to a data.frame
  providers <- data.frame(specialist, doctor, nurse, ha, pdoc, lhv,
                 midwife, amw, tba, chw, ehw, anc1, anc4,
                 incurCost, amountCost, transportCost, registrationCost,
                 medicineCost, labCost, providerCost, giftCost, borrow)
  ## Return data.frame
  return(providers)
}


################################################################################
#
#' Function to recode counselling associated with antenatal care for the Myanmar
#' MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded counselling indicators associated with
#'   antenatal care
#'
#' @examples
#' ## Recode anc counselling for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_anc_counselling(df = x, status = "current")
#'
#' ## Recode anc counselling for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_anc_counselling(df = x, status = "past")
#'
#' @export
#'
#
################################################################################

recode_anc_counselling <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## MCH book
    mchBook <- as.integer(df$ancnow_mchbook)
    ## Counselling
    counsel <- as.integer(df$ancnow_conselling)
    ## Restrict some foods?
    restrict <- as.integer(df$ancnow_restrict)
    ## Restrict vegetables
    restrictVeg <- as.integer(df$ancnow_restrict_item.1)
    restrictVeg <- ifelse(is.na(restrict) | restrict == 0, NA, restrictVeg)
    ## Restrict fruits
    restrictFruits <- as.integer(df$ancnow_restrict_item.2)
    restrictFruits <- ifelse(is.na(restrict) | restrict == 0, NA, restrictFruits)
    ## Restrict grains
    restrictGrains <- as.integer(df$ancnow_restrict_item.3)
    restrictGrains <- ifelse(is.na(restrict) | restrict == 0, NA, restrictGrains)
    ## Restrict meat
    restrictMeat <- as.integer(df$ancnow_restrict_item.4)
    restrictMeat <- ifelse(is.na(restrict) | restrict == 0, NA, restrictMeat)
    ## Restrict fish
    restrictFish <- as.integer(df$ancnow_restrict_item.5)
    restrictFish <- ifelse(is.na(restrict) | restrict == 0, NA, restrictFish)
    ## Restrict dairy
    restrictDairy <- as.integer(df$ancnow_restrict_item.6)
    restrictDairy <- ifelse(is.na(restrict) | restrict == 0, NA, restrictDairy)
    ## concatenate counselling data.frame
    counselling <- data.frame(mchBook, counsel, restrict, restrictVeg,
                              restrictFruits, restrictGrains, restrictMeat,
                              restrictFish, restrictDairy)
  } else {
    ## MCH book
    mchBook <- as.integer(df$ancpast_mchbook)
    ## Counselling
    counsel <- as.integer(df$ancpast_conselling)
    ## Restrict some foods?
    restrict <- as.integer(df$ancpast_restrict)
    ## Restrict vegetables
    restrictVeg <- as.integer(df$ancpast_restrict_item.1)
    restrictVeg <- ifelse(is.na(restrict) | restrict == 0, NA, restrictVeg)
    ## Restrict fruits
    restrictFruits <- as.integer(df$ancpast_restrict_item.2)
    restrictFruits <- ifelse(is.na(restrict) | restrict == 0, NA, restrictFruits)
    ## Restrict grains
    restrictGrains <- as.integer(df$ancpast_restrict_item.3)
    restrictGrains <- ifelse(is.na(restrict) | restrict == 0, NA, restrictGrains)
    ## Restrict meat
    restrictMeat <- as.integer(df$ancpast_restrict_item.4)
    restrictMeat <- ifelse(is.na(restrict) | restrict == 0, NA, restrictMeat)
    ## Restrict fish
    restrictFish <- as.integer(df$ancpast_restrict_item.5)
    restrictFish <- ifelse(is.na(restrict) | restrict == 0, NA, restrictFish)
    ## Restrict dairy
    restrictDairy <- as.integer(df$ancpast_restrict_item.6)
    restrictDairy <- ifelse(is.na(restrict) | restrict == 0, NA, restrictDairy)
    ## concatenate counselling data.frame
    counselling <- data.frame(mchBook, counsel, restrict, restrictVeg,
                              restrictFruits, restrictGrains, restrictMeat,
                              restrictFish, restrictDairy)
  }
  ## Return data.frame
  return(counselling)
}


################################################################################
#
#' Function to recode supplementation indicators for antenatal care for the
#' Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded supplementation indicators associated with
#'   antenatal care
#'
#' @examples
#' ## Recode anc supplementation for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_anc_supplementation(df = x, status = "current")
#'
#' ## Recode anc supplementation for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_anc_supplementation(df = x, status = "past")
#'
#' @export
#'
#
################################################################################

recode_anc_supplementation <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## B1 tablets
    b1 <- as.integer(df$ancnow_bone)
    b1 <- bbw::recode(var = b1, recodes = "999=0")
    ## IFA tablets
    ifa1 <- as.integer(df$ancnow_rion)
    ifa1 <- bbw::recode(var = ifa1, recodes = "999=0")
    ## IFA - frequency
    ifa2 <- as.integer(df$ancnow_iron_freq)
    ifa2[ifa2 == 0] <- NA
    ifa2[ifa2 == 1 & !is.na(ifa2)] <- (as.integer(df$ancnow_iron_count) * (365 / 12))[ifa2 == 1 & !is.na(ifa2)]
    ifa2[ifa2 == 2 & !is.na(ifa2)] <- (as.integer(df$ancnow_iron_count) * ((365/12) / 7))[ifa2 == 2 & !is.na(ifa2)]
    ifa2[ifa2 == 3 & !is.na(ifa2)] <- as.integer(df$ancnow_iron_count)[ifa2 == 3 & !is.na(ifa2)]
    ifa2 <- floor(ifa2)
    ifa2[df$ancnow_rion_length == "1" & !is.na(ifa2)] <- ifa2[df$ancnow_rion_length == "1" & !is.na(ifa2)]
    ifa2[df$ancnow_rion_length_oth == "1" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "1" & !is.na(df$ancnow_rion_length_oth)]
    ifa2[df$ancnow_rion_length_oth == "2" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "2" & !is.na(df$ancnow_rion_length_oth)] * 2
    ifa2[df$ancnow_rion_length_oth == "3" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "3" & !is.na(df$ancnow_rion_length_oth)] * 3
    ifa2[df$ancnow_rion_length_oth == "4" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "4" & !is.na(df$ancnow_rion_length_oth)] * 4
    ifa2[df$ancnow_rion_length_oth == "5" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "5" & !is.na(df$ancnow_rion_length_oth)] * 5
    ifa2[df$ancnow_rion_length_oth == "6" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "6" & !is.na(df$ancnow_rion_length_oth)] * 6
    ifa2[df$ancnow_rion_length_oth == "7" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "7" & !is.na(df$ancnow_rion_length_oth)] * 7
    ifa2[df$ancnow_rion_length_oth == "8" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "8" & !is.na(df$ancnow_rion_length_oth)] * 8
    ifa2[df$ancnow_rion_length_oth == "9" & !is.na(df$ancnow_rion_length_oth)] <- ifa2[df$ancnow_rion_length_oth == "9" & !is.na(df$ancnow_rion_length_oth)] * 9
    ##
    ifa3 <- as.integer(df$ancnow_iron_cost)
    ifa3 <- ifelse(ifa3 == 1, as.integer(df$ancnow_iron_amount), ifa3)
    ifa3 <- bbw::recode(var = ifa3, recodes = "777=NA;999=NA")
    ## Government hospital
    ifa4a <- as.integer(df$ancnow_iron_source.1)
    ifa4a <- ifelse(df$ancnow_iron_source == "", NA, ifa4a)
    ## EHO clinic
    ifa4b <- as.integer(df$ancnow_iron_source.2)
    ifa4b <- ifelse(df$ancnow_iron_source == "", NA, ifa4b)
    ## Private doctor/clinic
    ifa4c <- as.integer(df$ancnow_iron_source.3)
    ifa4c <- ifelse(df$ancnow_iron_source == "", NA, ifa4c)
    ## SRHC/RHC
    ifa4d <- as.integer(df$ancnow_iron_source.4)
    ifa4d <- ifelse(df$ancnow_iron_source == "", NA, ifa4d)
    ## Routine ANC location in ward/village
    ifa4e <- as.integer(df$ancnow_iron_source.5)
    ifa4e <- ifelse(df$ancnow_iron_source == "", NA, ifa4e)
  } else {
    ## B1 tablets
    b1 <- as.integer(df$ancpast_bone)
    b1 <- bbw::recode(var = b1, recodes = "999=0")
    ## IFA tablets
    ifa1 <- as.integer(df$ancpast_rion)
    ifa1 <- bbw::recode(var = ifa1, recodes = "999=0")
    ## IFA - frequency
    ifa2 <- as.integer(df$ancpast_iron_freq)
    ifa2a <- as.integer(df$ancpast_iron_count)
    ifa2a <- bbw::recode(var = ifa2a, recodes = "45:hi=NA")
    ifa2[ifa2 == 0] <- NA
    ifa2[ifa2 == 1 & !is.na(ifa2)] <- (ifa2a * (365.25 / 12))[ifa2 == 1 & !is.na(ifa2)]
    ifa2[ifa2 == 2 & !is.na(ifa2)] <- (ifa2a * ((365.25 / 12) / 7))[ifa2 == 2 & !is.na(ifa2)]
    ifa2[ifa2 == 3 & !is.na(ifa2)] <- ifa2a[ifa2 == 3 & !is.na(ifa2)]
    ifa2 <- floor(ifa2)
    ifa2[df$ancpast_rion_length == "1" & !is.na(ifa2)] <- ifa2[df$ancpast_rion_length == "1" & !is.na(ifa2)]
    ifa2[df$ancpast_rion_length_oth == "1" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "1" & !is.na(df$ancpast_rion_length_oth)]
    ifa2[df$ancpast_rion_length_oth == "2" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "2" & !is.na(df$ancpast_rion_length_oth)] * 2
    ifa2[df$ancpast_rion_length_oth == "3" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "3" & !is.na(df$ancpast_rion_length_oth)] * 3
    ifa2[df$ancpast_rion_length_oth == "4" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "4" & !is.na(df$ancpast_rion_length_oth)] * 4
    ifa2[df$ancpast_rion_length_oth == "5" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "5" & !is.na(df$ancpast_rion_length_oth)] * 5
    ifa2[df$ancpast_rion_length_oth == "6" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "6" & !is.na(df$ancpast_rion_length_oth)] * 6
    ifa2[df$ancpast_rion_length_oth == "7" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "7" & !is.na(df$ancpast_rion_length_oth)] * 7
    ifa2[df$ancpast_rion_length_oth == "8" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "8" & !is.na(df$ancpast_rion_length_oth)] * 8
    ifa2[df$ancpast_rion_length_oth == "9" & !is.na(df$ancpast_rion_length_oth)] <- ifa2[df$ancpast_rion_length_oth == "9" & !is.na(df$ancpast_rion_length_oth)] * 9
    ##
    ifa3 <- as.integer(df$ancpast_iron_cost)
    ifa3 <- ifelse(ifa3 == 1, as.integer(df$ancpast_iron_amount), ifa3)
    ifa3 <- bbw::recode(var = ifa3, recodes = "444=NA;777=NA;999=NA")
    ## Government hospital
    ifa4a <- as.integer(df$ancpast_iron_source.1)
    ifa4a <- ifelse(df$ancpast_iron_source == "", NA, ifa4a)
    ## EHO clinic
    ifa4b <- as.integer(df$ancpast_iron_source.2)
    ifa4b <- ifelse(df$ancpast_iron_source == "", NA, ifa4b)
    ## Private doctor/clinic
    ifa4c <- as.integer(df$ancpast_iron_source.3)
    ifa4c <- ifelse(df$ancpast_iron_source == "", NA, ifa4c)
    ## SRHC/RHC
    ifa4d <- as.integer(df$ancpast_iron_source.4)
    ifa4d <- ifelse(df$ancpast_iron_source == "", NA, ifa4d)
    ## Routine ANC location in ward/village
    ifa4e <- as.integer(df$ancpast_iron_source.5)
    ifa4e <- ifelse(df$ancpast_iron_source == "", NA, ifa4e)
  }
  ## concatenate
  ancSupp <- data.frame(b1, ifa1, ifa2, ifa3, ifa4a, ifa4b, ifa4c, ifa4d, ifa4e)
  ## Return
  return(ancSupp)
}


################################################################################
#
#' Function to recode testing for infections indicators for antenatal care for
#' the Myanmar MCCT Programme Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#'
#' @return A data.frame of recoded testing for infections associated with
#'   antenatal care
#'
#' @examples
#' ## Recode data for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_anc_testing(df = x, status = "current")
#'
#' ## Recode data for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_anc_testing(df = x, status = "past")
#'
#' @export
#'
#
################################################################################

recode_anc_testing <- function(df, status = NULL) {
  ## Determine status
  if(status == "current") {
    ## Tests done
    test1 <- suppressWarnings(as.integer(df$ancnow_test_yesno))
    test1 <- bbw::recode(var = test1, recodes = "1:4=1;888=1;0=0;999=0;NA=NA")
    ## Hepatitis B
    test2a <- suppressWarnings(as.integer(df$ancnow_test_yesno.1))
    test2a <- ifelse(is.na(test1), NA, test2a)
    ## Hepatitis C
    test2b <- suppressWarnings(as.integer(df$ancnow_test_yesno.2))
    test2b <- ifelse(is.na(test1), NA, test2b)
    ## HIV/AIDS
    test2c <- suppressWarnings(as.integer(df$ancnow_test_yesno.3))
    test2c <- ifelse(is.na(test1), NA, test2c)
    ## Syphillis
    test2d <- suppressWarnings(as.integer(df$ancnow_test_yesno.4))
    test2d <- ifelse(is.na(test1), NA, test2d)
    ## Code for other tests

    ## Government hospital
    test3a <- suppressWarnings(as.integer(df$ancnow_test_where.1))
    test3a <- ifelse(is.na(test1), NA, test3a)
    ## EHO clinic
    test3b <- suppressWarnings(as.integer(df$ancnow_test_where.2))
    test3b <- ifelse(is.na(test1), NA, test3b)
    ## Private doctor/clinic
    test3c <- suppressWarnings(as.integer(df$ancnow_test_where.3))
    test3c <- ifelse(is.na(test1), NA, test3c)
    ## SRHC/RHC
    test3d <- suppressWarnings(as.integer(df$ancnow_test_where.4))
    test3d <- ifelse(is.na(test1), NA, test3d)
    ## Routine ANC location in ward/village
    test3e <- suppressWarnings(as.integer(df$ancnow_test_where.5))
    test3e <- ifelse(is.na(test1), NA, test3e)
    ## Code for other locations

    ## Does test cost?
    test4 <- suppressWarnings(as.integer(df$ancnow_test_cost))
    test5 <- test4
    test4 <- bbw::recode(var = test4, recodes = "1:hi=1;0=0;NA=NA")
  } else {
    ## Tests done
    test1 <- suppressWarnings(as.integer(df$ancpast_test_yesno))
    test1 <- bbw::recode(var = test1, recodes = "1:4=1;888=1;0=0;999=0;NA=NA")
    ## Hepatitis B
    test2a <- suppressWarnings(as.integer(df$ancpast_test_yesno.1))
    test2a <- ifelse(is.na(test1), NA, test2a)
    ## Hepatitis C
    test2b <- suppressWarnings(as.integer(df$ancpast_test_yesno.2))
    test2b <- ifelse(is.na(test1), NA, test2b)
    ## HIV/AIDS
    test2c <- suppressWarnings(as.integer(df$ancpast_test_yesno.3))
    test2c <- ifelse(is.na(test1), NA, test2c)
    ## Syphillis
    test2d <- suppressWarnings(as.integer(df$ancpast_test_yesno.4))
    test2d <- ifelse(is.na(test1), NA, test2d)
    ## Code for other tests

    ## Government hospital
    test3a <- suppressWarnings(as.integer(df$ancpast_test_where.1))
    test3a <- ifelse(is.na(test1), NA, test3a)
    ## EHO clinic
    test3b <- suppressWarnings(as.integer(df$ancpast_test_where.2))
    test3b <- ifelse(is.na(test1), NA, test3b)
    ## Private doctor/clinic
    test3c <- suppressWarnings(as.integer(df$ancpast_test_where.3))
    test3c <- ifelse(is.na(test1), NA, test3c)
    ## SRHC/RHC
    test3d <- suppressWarnings(as.integer(df$ancpast_test_where.4))
    test3d <- ifelse(is.na(test1), NA, test3d)
    ## Routine ANC location in ward/village
    test3e <- suppressWarnings(as.integer(df$ancpast_test_where.5))
    test3e <- ifelse(is.na(test1), NA, test3e)
    ## Code for other locations

    ## Does test cost?
    test4 <- suppressWarnings(as.integer(df$ancpast_test_cost))
    test5 <- test4
    test4 <- bbw::recode(var = test4, recodes = "1:hi=1;0=0;NA=NA")
  }
  ## Concatenate testing indicators
  test <- data.frame(test1, test2a, test2b, test2c, test2d,
                     test3a, test3b, test3c, test3d, test3e,
                     test4, test5)
  ## Return
  return(test)
}


################################################################################
#
#' Function to recode antenatal care indicators for the Myanmar MCCT Programme
#' Evaluation Survey
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing antenatal care data
#' @param status A character value indicating whether to recode data of
#'   currently pregnant women ("current") or previously pregnant women ("past")
#' @param core.columns A vector of variable names to include in resulting
#'
#' @return A data.frame of recoded antenatal care indicators
#'
#' @examples
#' ## Recode anc indicators for currently pregnant women
#' x <- create_anc(df = anc1, x = hh, y = hhMembers, status = "current")
#' recode_anc(df = x, status = "current")
#'
#' ## Recode anc indicators for non-pregnant women
#' x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past")
#' recode_anc(df = x, status = "past")
#'
#' @export
#
################################################################################

recode_anc <- function(df,
                       status = NULL,
                       core.columns = c("KEY",
                                        "KEY.y",
                                        "geo_rural",
                                        "geo_state",
                                        "geo_villward",
                                        "sample_component")) {
  ## Provider
  provider <- recode_anc_provider(df = df, status = status)
  ## Counselling
  counselling <- recode_anc_counselling(df = df, status = status)
  ## Supplementation
  supplementation <- recode_anc_supplementation(df = df, status = status)
  ## Testing
  testing <- recode_anc_testing(df = df, status = status)
  ## Concatenate all ANC indicators
  ancDF <- data.frame(df[ , core.columns], provider,
                      counselling, supplementation, testing)
  ##
  return(ancDF)
}
