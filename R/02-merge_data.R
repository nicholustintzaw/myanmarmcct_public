################################################################################
#
#' Function to merge main ODK form with repeats
#'
#' @param x A list of data.frames produced by `get_mcct_data` after
#'   retreiving data from ODK server.
#' @param rep.name A character value specifying name of repeat form to merge
#'   with main form.
#'
#' @return A data.frame with rows equal to repeat data merged with main form
#'
#' @examples
#' \dontrun{
#' hh <- merge_repeats(x = surveyData, rep.name = "support_gov_rep")
#' head(hh, 10)
#' }
#'
#' @export
#'
#
################################################################################

merge_repeats <- function(x, rep.name) {
  merge(x[[1]], x[[rep.name]], by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
}
