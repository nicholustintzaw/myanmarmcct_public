################################################################################
#
#' Function to clean household data
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing household data. Default is \code{hh}.
#' @param checks A data.frame containing check values for cleaning.
#'
#' @return A data.frame equivalent to \code{df} but with extraneous observations
#'   and other variables cleaned ready for further processing.
#'
#' @examples
#' clean_hh(df = hh, checks = checks)
#'
#' @export
#'
#
################################################################################

clean_hh <- function(df, checks) {
  for(i in checks$id) {
    df[checks$index[checks$id == i], checks$variable[checks$id == i]] <- checks$newvalue[checks$id == i]
  }
  ## Remove row 350
  cleanDF <- df[row.names(df) != 350, ]
  ## Remove observations with geo_villward == ""
  cleanDF <- df[df$geo_villward != "", ]
  ## Reassign geo_vill "168652" as hard-to-reach (currently rural)
  cleanDF[cleanDF$geo_vill == "168652" & !is.na(cleanDF$geo_vill), "geo_rural"] <- "2"
  ## Return cleanDF
  return(cleanDF)
}


################################################################################
#
#' Function to clean anthropometric data
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing anthropometric data.
#' @param checks A data.frame containing check values for cleaning.
#'
#' @return A data.frame equivalent to \code{df} but with extraneous observations
#'   and other variables cleaned ready for further processing.
#'
#' @examples
#' \dontrun{
#' clean_anthro(df = anthroData[[1]], checks = checks)
#' }
#'
#' @export
#'
#
################################################################################

clean_anthro <- function(df, checks) {
  for(i in checks$id) {
    df[checks$index[checks$id == i], checks$variable[checks$id == i]] <- checks$newvalue[checks$id == i]
  }
  ##
  cleanDF <- df
  ## Reassign geo_vill "168652" as hard-to-reach (currently rural)
  cleanDF[cleanDF$geo_vill == "168652" & !is.na(cleanDF$geo_vill), "geo_rural"] <- "2"
  ## Return cleanDF
  return(cleanDF)
}

