################################################################################
#
#' Function to create indicator results tables
#'
#' This function performs an exact binomial test on the data for the indicator
#' of interest and provides an estimate of the indicator proportion and the
#' corresponding 95\% confidence interval. This assumes that the probability
#' weights of selection of an respondent are equal (given that sample has
#' been taken using probability proportional to population size, this
#' assumption holds true unless otherwise stated).
#'
#' @param df A recoded data.frame of indicators
#' @param var Indicator to create a results table for
#'
#' @return A data.frame of indicator results with 95\% confidence limits for
#'   each state in total and disaggregated by urban, rural and hard-to-reach
#'   areas.
#'
#' @examples
#' ## Recode anthro indicators
#' temp <- recode_anthro(df = create_canthro(df = childAnthro, x = anthroDF))
#'
#' create_table(df = temp, var = "global.haz")
#'
#' @export
#'
#'
#
################################################################################

create_table <- function(df, var) {
  temp <- df[ , c("geo_rural", "geo_state", var)]

  tab <- with(temp, table(geo_rural, geo_state, get(var)))

  rural.02 <- binom.test(x = tab[1, 1, 2], n = sum(tab[1, 1, 1], tab[1, 2, 1]))
  rural.03 <- binom.test(x = tab[1, 2, 2], n = sum(tab[1, 2, 1], tab[1, 2, 2]))

  urban.02 <- binom.test(x = tab[2, 1, 2], n = sum(tab[2, 1, 1], tab[2, 1, 2]))
  urban.03 <- binom.test(x = tab[2, 2, 2], n = sum(tab[2, 2, 1], tab[2, 2, 2]))

  eho.02 <- binom.test(x = tab[3, 1, 2], n = sum(tab[3, 1, 1], tab[3, 1, 2]))
  eho.03 <- binom.test(x = tab[3, 2, 2], n = sum(tab[3, 2, 1], tab[3, 2, 2]))

  tabTotal <- with(temp, table(get(var), geo_state))

  total.02 <- binom.test(x = tabTotal[2, 1], n = colSums(tabTotal)[1])
  total.03 <- binom.test(x = tabTotal[2, 2], n = colSums(tabTotal)[2])

  resultsDF <- data.frame(matrix(ncol = 7, nrow = 5))

  resultsDF[ , 1] <- c("", "Rural", "Urban", "Hard-to-reach", "Total")
  resultsDF[1, 2:7] <- c(rep("State 1", 3), rep("State 2", 3))
  resultsDF[2, 2:7] <- c(rural.02$estimate, rural.02$conf.int, rural.03$estimate, rural.03$conf.int)
  resultsDF[3, 2:7] <- c(urban.02$estimate, urban.02$conf.int, urban.03$estimate, urban.03$conf.int)
  resultsDF[4, 2:7] <- c(eho.02$estimate, eho.02$conf.int, eho.03$estimate, eho.03$conf.int)
  resultsDF[5, 2:7] <- c(total.02$estimate, total.02$conf.int, total.03$estimate, total.03$conf.int)

  names(resultsDF) <- c("", "Estimate", "95% LCL", "95% UCL", "Estimate", "95% LCL", "95% UCL")

  return(resultsDF)
}


################################################################################
#
#' Function to create indicator results tables
#'
#' @param df A recoded data.frame of indicators
#' @param ppiDF A data.frame produced when \code{recode_ppi} is applied to
#'   the \code{hh} dataset of the Myanmar MCCT Programme Evaluation
#' @param vars Indicator/s to create a results table for
#' @param digits Integer value for number of digits to round off values.
#'   Default is NULL for no rounding off.
#'
#' @return A data.frame of indicator results for each state in total and
#'   disaggregated by urban, rural and hard-to-reach areas and by wealth
#'   quintile.
#'
#' @examples
#' ## Recode wash indicators
#' washDF  <- recode_wash(df = hh)
#'
#' create_unweighted_table(df = washDF,
#'                         vars = c("summer1", "summer6", "summer5",
#'                                  "rain1", "rain6", "rain5",
#'                                  "winter1", "winter6", "winter5"))
#'
#' @export
#'
#
################################################################################

create_unweighted_table <- function(df,
                                    ppiDF = recode_ppi(df = hh),
                                    vars,
                                    digits = NULL) {
  ppiDF <- ppiDF[ , c("KEY", "ppi")]
  ## Merge df with hh to get ppi results
  x <- merge(df, ppiDF, by = "KEY")
  x <- x[x$sample_component == 1, ]
  ## Get quintiles of samples in x by state
  x <- split_to_quintiles(df = x, by = "geo_state")
  ## Aggregate variables by state and geographic strata
  y <- aggregate(x = x[ , vars],
                 by = list(x$geo_state, x$geo_rural),
                 FUN = function(x) {xx <- mean(x, na.rm = TRUE); if(is.na(xx)) xx <- 0; return(xx)})
  ## Rename variables
  names(y) <- c("state", "strata", vars)
  ##
  y$state <- ifelse(y$state == "MMR002", "Kayah", "Kayin")
  y$strata <- ifelse(y$strata == 0, "Rural",
                ifelse(y$strata == 1, "Urban", "Hard-to-reach"))
  ## Aggregate variables by state and wealth strata
  z <- aggregate(x = x[ , vars],
                 by = list(x$geo_state, x$wealthQuintile),
                 FUN = function(x) {xx <- mean(x, na.rm = TRUE); if(is.na(xx)) xx <- 0; return(xx)})
  ## Rename variables
  names(z) <- c("state", "strata", vars)
  ##
  z$state <- ifelse(z$state == "MMR002", "Kayah", "Kayin")
  #z$strata <- paste("Wealth Quintile ", z$strata, sep = "")
  z$strata[z$strata == 1] <- "Wealthiest"
  z$strata[z$strata == 2] <- "Wealthy"
  z$strata[z$strata == 3] <- "Medium"
  z$strata[z$strata == 4] <- "Poor"
  z$strata[z$strata == 5] <- "Poorest"
  ## Concatenate results
  tab <- data.frame(rbind(y, z))
  ## Round off?
  if(!is.null(digits)) {
    tab[ , vars] <- round(tab[ , vars], digits = digits)
  }
  ## Order tab
  tab <- tab[order(tab$state), ]
  ## Return
  return(tab)
}


################################################################################
#
#' Function to create indicator results tables
#'
#' @param df A recoded data.frame of indicators
#' @param vars Indicator/s to create a results table for
#' @param digits Integer value for number of digits to round off values.
#'   Default is NULL for no rounding off.
#'
#' @return A data.frame of indicator results for each state in total and
#'   disaggregated by urban, rural and hard-to-reach areas and by wealth
#'   quintile.
#'
#' @examples
#' ## Recode wash indicators
#' canthro  <- recode_anthro(df = create_canthro(df = childAnthro, x = anthroDF))
#'
#' create_anthro_table(df = canthro,
#'                     vars = c("haz", "global.haz", "moderate.haz", "severe.haz"))
#'
#' @export
#'
#
################################################################################

create_anthro_table <- function(df,
                                vars,
                                digits = NULL) {
  x <- df[df$sample_component == 1, ]
  ## Aggregate variables by state and geographic strata
  y <- aggregate(x = x[ , vars],
                 by = list(x$geo_state, x$geo_rural),
                 FUN = mean, na.rm = TRUE)
  ## Rename variables
  names(y) <- c("state", "strata", vars)
  ##
  y$state <- ifelse(y$state == "MMR002", "Kayah", "Kayin")
  y$strata <- ifelse(y$strata == 0, "Rural",
                     ifelse(y$strata == 1, "Urban", "Hard-to-reach"))
  ## Concatenate results
  tab <- y
  ## Round off?
  if(!is.null(digits)) {
    tab[ , vars] <- round(tab[ , vars], digits = digits)
  }
  ## Order tab
  tab <- tab[order(tab$state), ]
  ## Return
  return(tab)
}


################################################################################
#
#' Function to create weighted indicator results tables
#'
#' @param df A recoded data.frame of indicators
#' @param sex Logical. Should results be disaggregated by sex? Default is FALSE
#' @param ppiDF A data.frame produced when \code{recode_ppi} is applied to
#'   the \code{hh} dataset of the Myanmar MCCT Programme Evaluation
#' @param vars Variable names for indicator/s to create a results table for
#' @param labs Variable labels for indicator/s to create a results table for
#' @param state A character value for name of state to report results on.
#'   Choice between Kayah or Kayin.
#' @param digits Integer value for number of digits to round off values.
#'   Default is NULL for no rounding off.
#'
#' @return A data.frame of indicator results for each state in total and
#'   disaggregated by urban, rural and hard-to-reach areas and by wealth
#'   quintile.
#'
#' @examples
#' ## Recode MCCT indicators
#' mcctDF  <- recode_mcct(df = create_mcct(df = hh, x = hhMembers))
#'
#' create_weighted_table(df = mcctDF,
#'                       vars = c("mcct1", "mcct2"),
#'                       labs = c("MCCT coverage",
#'                                "Mean number of cash transfers received"),
#'                       state = "Kayah")
#'
#' ## Recode IYCF indicators
#' iycfDF <- recode_iycf(df = create_iycf(df = iycf, x = hh, y = hhMembers))
#'
#' create_weighted_table(df = iycfDF,
#'                       sex = TRUE,
#'                       vars = c("eibf", "ebf", "mmf1", "mmf2", "mmf3", "mmf",
#'                                "fgscore", "mdd", "mad1", "mad2", "mad"),
#'                       labs = c("Early initiation of breastfeeding",
#'                                "Exclusive breastfeeding",
#'                                "Minimum meal frequency for breastfed 6-8 month old",
#'                                "Minimum meal frequency for breastfed 9-23 month old",
#'                                "Minimum meal frequency for non-breastfed 6-23 month old",
#'                                "Minimum meal frequency",
#'                                "Mean number of food groups consumed",
#'                                "Minimum dietary diversity",
#'                                "Minimum adequate diet for breastfed children",
#'                                "Minimum adequate diet for non-breastfed children",
#'                                "Minimum adequate diet"),
#'                       state = "Kayah")
#'
#'
#' @export
#'
#
################################################################################

create_weighted_table <- function(df,
                                  sex = FALSE,
                                  ppiDF = recode_ppi(df = hh),
                                  vars,
                                  labs,
                                  state,
                                  digits = NULL) {
  ppi <- ppiDF[ , c("KEY", "ppi")]
  ## Merge df with hh to get ppi results
  x <- merge(df, ppi, by = "KEY")
  x <- x[x$sample_component == 1, ]
  ## Get quintiles of samples in x by state
  x <- split_to_quintiles(df = x, by = "geo_state")
  ##
  stateCode <- ifelse(state == "Kayah", "MMR002", "MMR003")
  ##
  x <- x[x$geo_state == stateCode, ]
  ## Set survey design - Kayah
  strataDesign <- suppressWarnings(
    survey::svydesign(ids = ~geo_villward,
                      strata = ~geo_rural,
                      nest = TRUE,
                      data = x)
  )
  ##
  strataDesign$variables$geo_rural <- factor(x = strataDesign$variables$geo_rural,
                                             levels = c("1", "0", "2"),
                                             labels = c("Urban",
                                                        "Rural",
                                                        "Hard-to-reach"))
  ##
  strataDesign$variables$wealthQuintile <- factor(x = strataDesign$variables$wealthQuintile,
                                                  levels = 1:5,
                                                  labels = c("Wealthiest",
                                                             "Wealthy",
                                                             "Medium",
                                                             "Poor",
                                                             "Poorest"))
  ## Calculate Kayah weighted results
  resultsDF <- data.frame(
    matrix(ncol = 9,
           nrow = 9 * length(vars))
  )

  names(resultsDF) <- c("state", "strata", "variable", "indicator", "n", "estimate", "se", "lcl", "ucl")

  resultsDF$state <- state
  resultsDF$strata <- rep(c("Urban", "Rural", "Hard-to-reach", "Total",
                            "Wealthiest", "Wealthy", "Medium",
                            "Poor", "Poorest"), length(vars))
  ## Check if sex disaggregation is needed
  if(sex) {
    ##
    strataDesign$variables$sex <- factor(x = strataDesign$variables$sex,
                                         levels = c("1", "2"),
                                         labels = c("Male",
                                                    "Female"))
    ##
    resultsDF <- data.frame(
      matrix(ncol = 9,
             nrow = 11 * length(vars))
    )

    names(resultsDF) <- c("state", "strata", "variable", "indicator", "n", "estimate", "se", "lcl", "ucl")

    resultsDF$state <- state
    resultsDF$strata <- rep(c("Urban", "Rural", "Hard-to-reach", "Total",
                              "Male", "Female",
                              "Wealthiest", "Wealthy", "Medium",
                              "Poor", "Poorest"), length(vars))
  }

  variable <- NULL

  for(i in vars) {
    variable <- c(variable, rep(i, nrow(resultsDF) / length(vars)))
  }

  resultsDF$variable <- variable

  indicator <- NULL

  for(i in labs) {
    indicator <- c(indicator, rep(i, nrow(resultsDF) / length(vars)))
  }

  resultsDF$indicator <- indicator

  ## Calculate geographic area strata results
  for(i in levels(strataDesign$variables$geo_rural)) {
    ## Subset kayahDF to current strata
    strataDF <- strataDesign[strataDesign$variables$geo_rural == i, ]
    ## Cycle through vars
    for(j in vars) {
      f <- as.formula(paste("~", j, sep = ""))
      est <- coef(svymean(x = f, design = strataDF, na.rm = TRUE))
      se <- survey::SE(svymean(x = f, design = strataDF, na.rm = TRUE))
      ci <- confint(svymean(x = f, design = strataDF, na.rm = TRUE))
      resultsDF[resultsDF$strata == i & resultsDF$variable == j, "estimate"] <- est
      resultsDF[resultsDF$strata == i & resultsDF$variable == j, "se"] <- se
      resultsDF[resultsDF$strata == i & resultsDF$variable == j, c("lcl", "ucl")] <- ci
    }
  }
  ## Calculate total
  for(i in vars) {
    f <- as.formula(paste("~", i, sep = ""))
    est <- coef(survey::svymean(x = f, design = strataDesign, na.rm = TRUE))
    se <- survey::SE(survey::svymean(x = f, design = strataDesign, na.rm = TRUE))
    ci <- confint(survey::svymean(x = f, design = strataDesign, na.rm = TRUE))
    resultsDF[resultsDF$strata == "Total" & resultsDF$variable == i, "estimate"] <- est
    resultsDF[resultsDF$strata == "Total" & resultsDF$variable == i, "se"] <- se
    resultsDF[resultsDF$strata == "Total" & resultsDF$variable == i, c("lcl", "ucl")] <- ci
  }
  ## Check if sex disaggregation TRUE
  if(sex) {
    ## Calculate sex disaggregation results
    for(i in levels(strataDesign$variables$sex)) {
      ## Subset strataDesign to current sex
      strataDF <- strataDesign[strataDesign$variables$sex == i, ]
      ## Cycle through vars
      for(j in vars) {
        f <- as.formula(paste("~", j, sep = ""))
        est <- coef(survey::svymean(x = f, design = strataDF, na.rm = TRUE))
        se <- survey::SE(survey::svymean(x = f, design = strataDF, na.rm = TRUE))
        ci <- confint(survey::svymean(x = f, design = strataDF, na.rm = TRUE))
        ##
        resultsDF[resultsDF$strata == i & resultsDF$variable == j, "estimate"] <- est
        resultsDF[resultsDF$strata == i & resultsDF$variable == j, "se"] <- se
        resultsDF[resultsDF$strata == i & resultsDF$variable == j, c("lcl", "ucl")] <- ci
      }
    }
  }
  ## Calculate wealth quintiles strata results
  for(i in levels(strataDesign$variables$wealthQuintile)) {
    ## Subset strataDesign to current strata
    strataDF <- strataDesign[strataDesign$variables$wealthQuintile == i, ]
    ## Cycle through vars
    for(j in vars) {
      f <- as.formula(paste("~", j, sep = ""))
      est <- coef(survey::svymean(x = f, design = strataDF, na.rm = TRUE))
      se <- survey::SE(survey::svymean(x = f, design = strataDF, na.rm = TRUE))
      ci <- confint(survey::svymean(x = f, design = strataDF, na.rm = TRUE))

      resultsDF[resultsDF$strata == i & resultsDF$variable == j, "estimate"] <- est
      resultsDF[resultsDF$strata == i & resultsDF$variable == j, "se"] <- se
      resultsDF[resultsDF$strata == i & resultsDF$variable == j, c("lcl", "ucl")] <- ci
    }
  }
  ## Add n
  nSample <- get_n(df = df, sex = sex, ppiDF = ppiDF, vars = vars)
  ##
  n <- NULL
  ##
  for(i in vars) {
    n <- c(n, nSample[[state]][ , i])
  }
  ##
  resultsDF$n <- n
  ## Round off?
  if(!is.null(digits)) {
    resultsDF[ , c("estimate", "se",
                   "lcl", "ucl")] <- round(resultsDF[ , c("estimate", "se",
                                                          "lcl", "ucl")],
                                           digits = digits)
  }
  ## Return
  return(resultsDF)
}


################################################################################
#
#' Function to create weighted anthropometric indicator results tables
#'
#' @param df A recoded data.frame of indicators
#' @param sex Logical. Should results be disaggregated by sex? Default is FALSE.
#' @param vars Variable names for anthropometric indicator/s to create a results
#'   table for
#' @param labs Variable labels for anthropometric indicator/s to create a
#'   results table for
#' @param state A character value for name of state to report results on.
#'   Choice between Kayah or Kayin.
#' @param digits Integer value for number of digits to round off values.
#'   Default is NULL for no rounding off.
#'
#' @return A data.frame of anthropometric indicator results for each state in
#'   total and disaggregated by urban, rural and hard-to-reach areas
#'
#' @examples
#' ## Recode stunting indicators
#' canthro  <- recode_anthro(df = create_canthro(df = childAnthro, x = anthroDF))
#'
#' create_weighted_anthro(df = canthro,
#'                        vars = c("haz", "global.haz", "moderate.haz", "severe.haz"),
#'                        labs = c("Mean height-for-age z-score",
#'                                 "Global stunting/stuntedness",
#'                                 "Moderate stunting/stuntedness",
#'                                 "Severe stunting/stuntedness"),
#'                        state = "Kayah")
#'
#' create_weighted_anthro(df = canthro, sex = TRUE,
#'                        vars = c("haz", "global.haz", "moderate.haz", "severe.haz"),
#'                        labs = c("Mean height-for-age z-score",
#'                                 "Global stunting/stuntedness",
#'                                 "Moderate stunting/stuntedness",
#'                                 "Severe stunting/stuntedness"),
#'                        state = "Kayah")
#'
#' @export
#'
#
################################################################################

create_weighted_anthro <- function(df,
                                   sex = FALSE,
                                   vars,
                                   labs,
                                   state,
                                   digits = NULL) {
  stateCode <- ifelse(state == "Kayah", "MMR002", "MMR003")
  x <- df[df$sample_component == 1 & df$geo_state == stateCode, ]
  x <- x[!is.na(x$geo_villward), ]

  ## Set survey design for selected state
  strataDesign <- suppressWarnings(
    survey::svydesign(ids = ~geo_villward,
                      strata = ~geo_rural,
                      nest = TRUE,
                      data = x)
  )
  ##
  strataDesign$variables$geo_rural <- factor(x = strataDesign$variables$geo_rural,
                                             levels = c("1", "0", "2"),
                                             labels = c("Urban",
                                                        "Rural",
                                                        "Hard-to-reach"))
  ## Calculate selected state weighted results
  resultsDF <- data.frame(
    matrix(ncol = 9,
           nrow = 4 * length(vars))
  )

  names(resultsDF) <- c("state", "strata", "variable",
                        "indicator", "n", "estimate", "se", "lcl", "ucl")

  resultsDF$state <- state
  resultsDF$strata <- rep(c("Urban", "Rural", "Hard-to-reach", "Total"),
                          length(vars))

  if(sex) {
    ##
    strataDesign$variables$sex <- factor(x = strataDesign$variables$sex,
                                         levels = c("1", "2"),
                                         labels = c("Male",
                                                    "Female"))
    ##
    resultsDF <- data.frame(
      matrix(ncol = 9,
             nrow = 6 * length(vars))
    )
    ##
    names(resultsDF) <- c("state", "strata", "variable",
                          "indicator", "n", "estimate", "se", "lcl", "ucl")
    ##
    resultsDF$state <- state
    resultsDF$strata <- rep(c("Urban", "Rural", "Hard-to-reach", "Total",
                              "Male", "Female"),
                            length(vars))
  }

  variable <- NULL

  for(i in vars) {
    variable <- c(variable, rep(i, nrow(resultsDF) / length(vars)))
  }

  resultsDF$variable <- variable

  indicator <- NULL

  for(i in labs) {
    indicator <- c(indicator, rep(i, nrow(resultsDF) / length(vars)))
  }

  resultsDF$indicator <- indicator

  ## Calculate geographic area strata results
  for(i in levels(strataDesign$variables$geo_rural)) {
    ## Subset strateDesign to current strata
    strataDF <- strataDesign[strataDesign$variables$geo_rural == i, ]
    ## Cycle through vars
    for(j in vars) {
      f <- as.formula(paste("~", j, sep = ""))
      est <- coef(svymean(x = f, design = strataDF, na.rm = TRUE))
      se <- survey::SE(svymean(x = f, design = strataDF, na.rm = TRUE))
      ci <- confint(svymean(x = f, design = strataDF, na.rm = TRUE))

      resultsDF[resultsDF$strata == i & resultsDF$variable == j, "estimate"] <- est
      resultsDF[resultsDF$strata == i & resultsDF$variable == j, "se"] <- se
      resultsDF[resultsDF$strata == i & resultsDF$variable == j, c("lcl", "ucl")] <- ci
    }
  }
  ## Calculate total
  for(i in vars) {
    f <- as.formula(paste("~", i, sep = ""))
    est <- coef(survey::svymean(x = f, design = strataDesign, na.rm = TRUE))
    se <- survey::SE(survey::svymean(x = f, design = strataDesign, na.rm = TRUE))
    ci <- confint(survey::svymean(x = f, design = strataDesign, na.rm = TRUE))
    resultsDF[resultsDF$strata == "Total" & resultsDF$variable == i, "estimate"] <- est
    resultsDF[resultsDF$strata == "Total" & resultsDF$variable == i, "se"] <- se
    resultsDF[resultsDF$strata == "Total" & resultsDF$variable == i, c("lcl", "ucl")] <- ci
  }
  ## Check if sex disaggregation TRUE
  if(sex) {
    ## Calculate sex disaggregation results
    for(i in levels(strataDesign$variables$sex)) {
      ## Subset strataDesign to current sex
      strataDF <- strataDesign[strataDesign$variables$sex == i, ]
      ## Cycle through vars
      for(j in vars) {
        f <- as.formula(paste("~", j, sep = ""))
        est <- coef(survey::svymean(x = f, design = strataDF, na.rm = TRUE))
        se <- survey::SE(survey::svymean(x = f, design = strataDF, na.rm = TRUE))
        ci <- confint(survey::svymean(x = f, design = strataDF, na.rm = TRUE))
        ##
        resultsDF[resultsDF$strata == i & resultsDF$variable == j, "estimate"] <- est
        resultsDF[resultsDF$strata == i & resultsDF$variable == j, "se"] <- se
        resultsDF[resultsDF$strata == i & resultsDF$variable == j, c("lcl", "ucl")] <- ci
      }
    }
  }
  ## Add n
  nSample <- get_n_area(df = df, vars = vars)
  if(sex) {
    nSex <- get_n_sex(df = df, vars = vars)
  }
  ##
  n <- NULL
  ##
  for(i in vars) {
    n <- c(n, nSample[[state]][ , i])
    if(sex) {
      n <- c(n, nSex[[state]][ , i])
    }
  }
  ##
  resultsDF$n <- n
  ## Round off?
  if(!is.null(digits)) {
    resultsDF[ , c("estimate", "se",
                   "lcl", "ucl")] <- round(resultsDF[ , c("estimate", "se",
                                                          "lcl", "ucl")],
                                           digits = digits)
  }
  ## Return
  return(resultsDF)
}

