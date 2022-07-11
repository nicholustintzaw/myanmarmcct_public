################################################################################
#
#' Function to recode township data
#'
#' @param df Dataset containing data collected from townships included in
#'   the MCCT baseline study.
#'
#' @return A recoded data.frame with the same number of rows as \code{data}.
#'
#' @examples
#' recode_township(df = townshipData)
#'
#' @export
#'
#
################################################################################

recode_township <- function(df) {
  ##
  df$town_pop_tot <- bbw::recode(var = df$town_pop_tot, recodes = "999=NA")
  df$town_pop_male <- bbw::recode(var = df$town_pop_male, recodes = "999=NA")
  df$town_pop_female <- bbw::recode(var = df$town_pop_female, recodes = "999=NA")
  df$town_pop_urban <- bbw::recode(var = df$town_pop_urban, recodes = "999=NA")
  df$town_pop_rural <- bbw::recode(var = df$town_pop_rural, recodes = "999=NA")
  df$town_pop_u5 <- bbw::recode(var = df$town_pop_u5, recodes = "999=NA")
  df$town_pop_0to14 <- bbw::recode(var = df$town_pop_0to14, recodes = "999=NA")
  df$town_pop_ecoage <- bbw::recode(var = df$town_pop_ecoage, recodes = "999=NA")
  df$town_pop_elder <- bbw::recode(var = df$town_pop_elder, recodes = "999=NA")
  df$town_pop_mwrg <- bbw::recode(var = df$town_pop_mwrg, recodes = "999=NA")
  df$town_pop_preg <- bbw::recode(var = df$town_pop_preg, recodes = "999=NA")
  df$town_pop_womenu2 <- bbw::recode(var = df$town_pop_womenu2, recodes = "999=NA")
  ##
  df$gov_township_num <- bbw::recode(var = df$gov_township_num, recodes = "999=NA")
  df$gov_town_num <- bbw::recode(var = df$gov_town_num, recodes = "999=NA")
  df$gov_ward_num <- bbw::recode(var = df$gov_ward_num, recodes = "999=NA")
  df$gov_villtract_num <- bbw::recode(var = df$gov_villtract_num, recodes = "999=NA")
  df$gov_vill_num <- bbw::recode(var = df$gov_vill_num, recodes = "999=NA")
  ##
  df$health_imr_tot <- bbw::recode(var = df$health_imr_tot, recodes = "999=NA")
  df$health_imr_male <- bbw::recode(var = df$health_imr_male, recodes = "999=NA")
  df$health_imr_female <- bbw::recode(var = df$health_imr_female, recodes = "999=NA")
  ##
  df$health_u5mr_tot <- bbw::recode(var = df$health_u5mr_tot, recodes = "999=NA")
  df$health_u5mr_male <- bbw::recode(var = df$health_u5mr_male, recodes = "999=NA")
  df$health_u5mr_female <- bbw::recode(var = df$health_u5mr_female, recodes = "999=NA")
  ##
  df$health_mmr_tot <- bbw::recode(var = df$health_mmr_tot, recodes = "999=NA")
  ##
  df$health_lifeexp_tot <- bbw::recode(var = df$health_lifeexp_tot, recodes = "999=NA")
  df$health_lifeexp_male <- bbw::recode(var = df$health_lifeexp_male, recodes = "999=NA")
  df$health_lifeexp_female <- bbw::recode(var = df$health_lifeexp_female, recodes = "999=NA")
  ##
  df$health_tfr <- bbw::recode(var = df$health_tfr, recodes = "999=NA")
  df$health_cbr <- bbw::recode(var = df$health_cbr, recodes = "999=NA")
  ##
  df$disable_tot <- bbw::recode(var = df$disable_tot, recodes = "999=NA")
  df$disable_walk <- bbw::recode(var = df$disable_walk, recodes = "999=NA")
  df$disable_see <- bbw::recode(var = df$disable_see, recodes = "999=NA")
  df$disable_hear <- bbw::recode(var = df$disable_hear, recodes = "999=NA")
  df$disable_remember <- bbw::recode(var = df$disable_remember, recodes = "999=NA")
  ##
  df$edu_primary <- bbw::recode(var = df$edu_primary, recodes = "999=NA")
  df$edu_middle <- bbw::recode(var = df$edu_middle, recodes = "999=NA")
  df$edu_high <- bbw::recode(var = df$edu_high, recodes = "999=NA")
  df$edu_uni <- bbw::recode(var = df$edu_uni, recodes = "999=NA")
  df$edu_literacy_male <- bbw::recode(var = df$edu_literacy_male, recodes = "999=NA")
  df$edu_literacy_female <- bbw::recode(var = df$edu_literacy_female, recodes = "999=NA")
  df$edu_literacy_tot <- bbw::recode(var = df$edu_literacy_tot, recodes = "999=NA")
  ##
  return(df)
}

################################################################################
#
#' Function to describe township
#'
#' @param df Dataset containing data collected from townships included in
#'   the MCCT baseline study
#' @param state State for which township description is being generated for
#'
#' @return A character vector describing each township
#'
#' @examples
#' describe_township(df = recode_township(df = townshipData),
#'                   state = "Kayah")
#'
#' @export
#'
#
################################################################################

describe_township <- function(df, state = c("Kayah", "Kayin")) {
  ## Subset townshipData to selected State
  x <- df[df$state_region %in% state, ]
  ## Convert geographical location text to title format using stringr
  x$geo_north <- stringr::str_to_title(x$geo_north)
  x$geo_east <- stringr::str_to_title(x$geo_east)
  x$geo_south <- stringr::str_to_title(x$geo_south)
  x$geo_west <- stringr::str_to_title(x$geo_west)
  ## Construct description list
  desc <- vector(mode = "list", length = nrow(x))
  for(i in 1:nrow(x)) {
    y1 <- paste(x$township[i], " Township is bounded by ",
                x$geo_north[i], " to the north, ",
                x$geo_east[i], " to the east, ",
                x$geo_south[i], " to the south and ",
                x$geo_west[i], " to the west.", sep = "")
    ##
    y2 <- paste(x$township[i], " Township has a total population of ",
                x$town_pop_tot[i], " with ",
                x$town_pop_male[i], " males and ",
                x$town_pop_female[i], " females. There are ",
                x$town_pop_urban[i], " people living in urban locations while ",
                x$town_pop_rural[i], " people are in rural areas. The under 5 years population is ",
                x$town_pop_u5[i], " and children 0-14 years is ",
                x$town_pop_0to14[i], ". The population of those at an economically productive age (15-64) is ",
                x$town_pop_ecoage[i], " while the elderly (65 and above) population is ",
                x$town_pop_elder[i], ". The population of married women at reproductive age is ",
                x$town_pop_mwrg[i], " and the population of pregnant women is ",
                x$town_pop_preg[i], ". The population of women with under 2 years children is ",
                x$town_pop_womenu2[i], ".",
                sep = "")
    y2 <- stringr::str_replace_all(string = y2, pattern = "NA", replacement = "unknown")
    ##
    y3 <- data.frame(as.character(codebookTownship$question[15:26]),
                     as.vector(x[i, as.character(codebookTownship$variable)[15:26]], mode = "numeric"))
    names(y3) <- c("Indicators", "Value")
    ##
    y4 <- paste(x$township[i], " Township has a total of ",
                x$gov_town_num[i], " town/s, ",
                x$gov_ward_num[i], " wards, ",
                x$gov_villtract_num[i], " village tracts, and ",
                x$gov_vill_num[i], " villages.",
                sep = "")
    y4 <- stringr::str_replace_all(string = y4, pattern = "NA", replacement = "unknown")
    ##
    y5 <- data.frame(as.character(codebookTownship$question[c(29, 31, 33, 35)]),
                     as.vector(x[i, as.character(codebookTownship$variable)[c(29, 31, 33, 35)]], mode = "numeric"))
    names(y5) <- c("Indicators", "Value")
    ##
    y6 <- paste(x$township[i], " Township's total infant mortality rate is ",
                x$health_imr_tot[i], " and its infant mortality rate is ",
                x$health_imr_male[i], " for males and ",
                x$health_imr_female[i], " for females. The total under 5 mortality rate for the township is ",
                x$health_u5mr_tot[i], " and its under 5 mortality rate is ",
                x$health_u5mr_male[i], " for males and ",
                x$health_u5mr_female[i], " for females. The total maternal mortality rate for the township is ",
                x$health_mmr_tot[i], ".",
                sep = "")
    y6 <- stringr::str_replace_all(string = y6, pattern = "NA", replacement = "unknown")
    ##
    y7 <- data.frame(as.character(codebookTownship$question[37:43]),
                     as.vector(x[i, as.character(codebookTownship$variable)[37:43]], mode = "numeric"))
    names(y7) <- c("Indicators", "Value")
    ##
    y8 <- paste(x$township[i], " Township's life expectancy is ",
                x$health_lifeexp_tot[i], ". The township's life expectancy for males is ",
                x$health_lifeexp_male[i], " and for females is ",
                x$health_lifeexp_female[i], ". The total fertility rate of the township is ",
                x$health_tfr[i], " while the crude birth rate is ",
                x$health_cbr[i], ".",
                sep = "")
    y8 <- stringr::str_replace_all(string = y8, pattern = "NA", replacement = "unknown")
    ##
    y9 <- data.frame(as.character(codebookTownship$question[44:48]),
                     as.vector(x[i, as.character(codebookTownship$variable)[44:48]], mode = "numeric"))
    names(y9) <- c("Indicators", "Value")
    ##
    y10 <- paste("The total number of disabled in the township is ",
                 x$disable_tot[i], ". The number of those who have disability in walking is ",
                 x$disable_walk[i], ". The number of those who have disability in seeing is ",
                 x$disable_see[i], ". The number of those who have disability in hearing is ",
                 x$disable_hear[i], ". The number of those who have disability in remembering is ",
                 x$disable_remember[i], ".",
                 sep = "")
    y10 <- stringr::str_replace_all(string = y10, pattern = "NA", replacement = "unknown")
    ##
    y11 <- data.frame(as.character(codebookTownship$question[49:53]),
                      as.vector(x[i, as.character(codebookTownship$variable)[49:53]],  mode = "numeric"))
    names(y11) <- c("Indicators", "Value")
    ##
    y12 <- paste(x$township[i], " Township's literacy rate is ",
                 x$edu_literacy_tot[i], ". The literacy rate for males is ",
                 x$edu_literacy_male[i], ". The literacy rate for females is ",
                 x$edu_literacy_female[i], ".",
                 sep = "")
    y12 <- stringr::str_replace_all(string = y12, pattern = "NA", replacement = "unknown")
    ##
    y13 <- data.frame(as.character(codebookTownship$question[62:64]),
                      as.vector(x[i, as.character(codebookTownship$variable)[62:64]], mode = "numeric"))
    names(y13) <- c("Indicators", "Value")
    z <- list(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13)
    desc[[i]] <- z
  }
  names(desc) <- x$township
  return(desc)
}

