################################################################################
#
#'
#' Function to get Myanmar MCCT study datasets and forms from ODK server.
#'
#' This function is a wrapper for a set of functions from the \code{odkr}
#' package.
#'
#' @param id Form ID. Can be one of several form IDs used by the
#'   different forms designed for the MCCT study -
#'   \code{baseline_mcct_final}, \code{baseline_mcct_anthro_final},
#'   \code{baseline_townshipprofile_final}, \code{baseline_villprofile_final},
#'   \code{MCCT_Baseline_Spotcheck}.
#' @param username ONA server username credentials. For
#' @param password ONA server password credentials.
#' @param start Include data from submission dates after (inclusive) this
#'   start date in export to CSV. Date format \code{<yyyy/MM/dd>} and default is
#'   current system date
#' @param end Include data from submission dates before (exclusive) this date
#'   in export to CSV. Date format is \code{<yyyy/MM/dd>} and default value current
#'   system date
#' @param filename Filename to use for data without the CSV file extension.
#' @param rep Logical. Does the form have repeat/s? Default FALSE.
#' @param rep.name A vector of repeat names to read in the form. Default
#'   is NULL. Must be specified if \code{rep} is TRUE.
#'
#' @return A data.frame corresponding to dataset corresponding to the form ID
#'   specified.
#'
#' @examples
#' \dontrun{
#'   get_mcct_data(id = "baseline_mcct_final",
#'                 username = "validmeasures",
#'                 password = "6Y2-8yK-Nmk-Lbf",
#'                 filename = "main_form")
#' }
#'
#' @export
#'
#
################################################################################

get_mcct_data <- function(id, username, password,
                          start = Sys.Date(),
                          end = Sys.Date(),
                          filename,
                          rep = FALSE,
                          rep.name = NULL) {
  ## Create temporary directory
  temp <- tempdir()
  ## Check if ODK Briefcase already available in tempdir
  if(!file.exists(paste(temp, "/odkBriefcase_latest.jar", sep = ""))) {
    ## Get latest briefcase and put in temporary directory
    odkr::get_briefcase(destination = temp)
  }
  ## Pull ODK forms definitions and submissions from server
  odkr::pull_remote(target = temp,
                    id = id,
                    from = "https://ona.io/valid_international",
                    to = temp,
                    username = username,
                    password = password)
  ## Export dataset
  odkr::export_data(target = temp,
                    id = id,
                    from = temp,
                    to = temp,
                    start = start,
                    end = end,
                    filename = paste(filename, ".csv", sep = ""),
                    exclude = TRUE,
                    split = TRUE,
                    group.names = TRUE,
                    overwrite = TRUE)
  ## Read specified dataset
  surveyData <- read.csv(paste(temp, "/", filename, ".csv", sep = ""),
                         stringsAsFactors = FALSE)
  ##
  fullData <- surveyData
  ##
  if(rep) {
    fullData <- vector(mode = "list", length = length(rep.name) + 1)
    names(fullData) <- c("surveyData", rep.name)
    fullData[[1]] <- surveyData
    for(i in rep.name) {
      fullData[[i]] <- read.csv(paste(temp, "/", filename, "-", i, ".csv", sep = ""),
                                stringsAsFactors = FALSE)
    }
  }
  ## Return data.frame
  return(fullData)
}
