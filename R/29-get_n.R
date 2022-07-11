################################################################################
#
#' Function to get number of samples used to calculate indicator stratified
#' by geographic area
#'
#' @param df A data.frame of recoded indicator variables
#' @param vars A character vector of variable names to get number of samples for
#'
#' @return A table of numeric values of number of samples use to calculate
#'   specific indicators by area strata
#'
#' @examples
#'
#' get_n_area(df = recode_anthro(df = create_canthro(df = childAnthro,
#'                                                   x = anthroDF)),
#'            vars = c("global.haz", "moderate.haz", "severe.haz"))
#'
#' @export
#'
#
################################################################################

get_n_area <- function(df, vars) {
  ##
  df$geo_rural <- factor(df$geo_rural,
                         levels = c("1", "0", "2"),
                         labels = c("Urban", "Rural", "Hard-to-reach"))
  ##
  df <- df[df$sample_component == 1, ]
  ##
  x <- data.frame(matrix(nrow = length(levels(df[["geo_rural"]])) + 1,
                         ncol = length(vars)))
  names(x) <- vars
  row.names(x) <- c(levels(df[["geo_rural"]]), "Total")
  ##
  y <- data.frame(matrix(nrow = length(levels(df[["geo_rural"]])) + 1,
                         ncol = length(vars)))
  names(y) <- vars
  row.names(y) <- c(levels(df[["geo_rural"]]), "Total")
  ##
  for(i in levels(df[["geo_rural"]])) {
    for(j in vars) {
      x[row.names(x) == i, j] <- sum(!is.na(df[df$geo_state == "MMR002" & df$geo_rural == i, j]))
      x[row.names(x) == "Total", j] <- sum(x[row.names(x) != "Total", j], na.rm = TRUE)
    }
  }
  ##
  for(i in levels(df[["geo_rural"]])) {
    for(j in vars) {
      y[row.names(y) == i, j] <- sum(!is.na(df[df$geo_state == "MMR003" & df$geo_rural == i, j]))
      y[row.names(y) == "Total", j] <- sum(y[row.names(y) != "Total" , j], na.rm = TRUE)
    }
  }
  ##
  nList <- list(x, y)
  names(nList) <- c("Kayah", "Kayin")
  return(nList)
}


################################################################################
#
#' Function to get number of samples used to calculate indicator stratified
#' by wealth
#'
#' @param df A data.frame of recoded indicator variables
#' @param ppiDF A data.frame produced when \code{recode_ppi} is applied to
#'   the \code{hh} dataset of the Myanmar MCCT Programme Evaluation
#' @param vars A character vector of variable names to get number of samples for
#'
#' @return A table of numeric values of number of samples use to calculate
#'   specific indicators by wealth strata
#'
#' @examples
#'
#' get_n_wealth(df = recode_mcct(create_mcct(df = hh, x = hhMembers)),
#'              vars = c("mcct1", "mcct2"))
#'
#' @export
#'
#
################################################################################

get_n_wealth <- function(df, ppiDF = recode_ppi(df = hh), vars) {
  ##
  ppiDF <- ppiDF[ , c("KEY", "ppi")]
  ## Merge df with hh to get ppi results
  df <- merge(df, ppiDF, by = "KEY")
  ##
  df <- df[df$sample_component == 1, ]
  ##
  df <- split_to_quintiles(df = df, by = "geo_state")
  ##
  df$wealthQuintile <- factor(df$wealthQuintile,
                              levels = c("1", "2", "3", "4", "5"),
                              labels = c("Wealthiest",
                                         "Wealthy",
                                         "Medium",
                                         "Poor",
                                         "Poorest"))
  ##
  df <- df[!is.na(df$wealthQuintile), ]
  ##
  x <- data.frame(matrix(nrow = length(levels(df[["wealthQuintile"]])),
                         ncol = length(vars)))
  names(x) <- vars
  row.names(x) <- levels(df[["wealthQuintile"]])
  ##
  y <- data.frame(matrix(nrow = length(levels(df[["wealthQuintile"]])),
                         ncol = length(vars)))
  names(y) <- vars
  row.names(y) <- levels(df[["wealthQuintile"]])
  ##
  for(i in levels(df[["wealthQuintile"]])) {
    for(j in vars) {
      x[row.names(x) == i, j] <- sum(!is.na(df[df$geo_state == "MMR002" & df$wealthQuintile == i, j]))
    }
  }
  ##
  for(i in levels(df[["wealthQuintile"]])) {
    for(j in vars) {
      y[row.names(y) == i, j] <- sum(!is.na(df[df$geo_state == "MMR003" & df$wealthQuintile == i, j]))
    }
  }
  ##
  nList <- list(x, y)
  names(nList) <- c("Kayah", "Kayin")
  return(nList)
}


################################################################################
#
#' Function to get number of samples used to calculate indicator stratified
#' by sex
#'
#' @param df A data.frame of recoded indicator variables
#' @param vars A character vector of variable names to get number of samples for
#'
#' @return A table of numeric values of number of samples use to calculate
#'   specific indicators by area strata
#'
#' @examples
#'
#' get_n_sex(df = recode_anthro(df = create_canthro(df = childAnthro,
#'                                                   x = anthroDF)),
#'           vars = c("global.haz", "moderate.haz", "severe.haz"))
#'
#' @export
#'
#
################################################################################

get_n_sex <- function(df, vars) {
  ##
  df$sex <- factor(df$sex,
                   levels = c("1", "2"),
                   labels = c("Male", "Female"))
  ##
  df <- df[df$sample_component == 1, ]
  ##
  x <- data.frame(matrix(nrow = length(levels(df[["sex"]])),
                         ncol = length(vars)))
  names(x) <- vars
  row.names(x) <- levels(df[["sex"]])
  ##
  y <- data.frame(matrix(nrow = length(levels(df[["sex"]])),
                         ncol = length(vars)))
  names(y) <- vars
  row.names(y) <- levels(df[["sex"]])
  ##
  for(i in levels(df[["sex"]])) {
    for(j in vars) {
      x[row.names(x) == i, j] <- sum(!is.na(df[df$geo_state == "MMR002" & df$sex == i, j]))
    }
  }
  ##
  for(i in levels(df[["sex"]])) {
    for(j in vars) {
      y[row.names(y) == i, j] <- sum(!is.na(df[df$geo_state == "MMR003" & df$sex == i, j]))
    }
  }
  ##
  nList <- list(x, y)
  names(nList) <- c("Kayah", "Kayin")
  return(nList)
}


################################################################################
#
#' Function to get number of samples used to calculate indicator
#'
#' @param df A data.frame of recoded indicator variables
#' @param sex Logical. Should sample size by sex be reported? Defualt is FALSE
#' @param ppiDF A data.frame produced when \code{recode_ppi} is applied to
#'   the \code{hh} dataset of the Myanmar MCCT Programme Evaluation
#' @param vars A character vector of variable names to get number of samples for
#'
#' @return A table of numeric values of number of samples use to calculate
#'   specific indicators by area and wealth strata
#'
#' @examples
#'
#' get_n(df = recode_mcct(create_mcct(df = hh, x = hhMembers)),
#'       vars = c("mcct1", "mcct2"))
#'
#' @export
#'
#
################################################################################

get_n <- function(df, sex = FALSE, ppiDF = recode_ppi(df = hh), vars) {
  ## Get n for area strata
  nArea <- get_n_area(df = df, vars = vars)
  ##  Get n for wealth strata
  nWealth <- get_n_wealth(df = df, ppiDF = ppiDF, vars = vars)
  ## Combine n tables
  x <- data.frame(rbind(nArea[[1]], nWealth[[1]]))
  y <- data.frame(rbind(nArea[[2]], nWealth[[2]]))
  if(sex) {
    ##
    nSex <- get_n_sex(df = df, vars = vars)
    ## Combine n tables
    x <- data.frame(rbind(nArea[[1]], nSex[[1]], nWealth[[1]]))
    y <- data.frame(rbind(nArea[[2]], nSex[[2]], nWealth[[2]]))
  }
  ## Create n list
  nList <- list(x, y)
  names(nList) <- c("Kayah", "Kayin")
  return(nList)
}
