################################################################################
#
#' Split observations into quintiles based on poverty probability
#'
#' @param df A data.frame collected for the Myanmar MCCT Programme Evaluation
#'   Study containing a variable for poverty probability index
#' @param by Character value specifying variable in \code{df} to group the
#'   data before splitting into quintiles. Default is NULL and whole data.frame
#'   is split into quintiles. If specified, \code{df} will first be split based
#'   on \code{by} and then each group is split into quintiles
#' @param add Logical. Should wealth quintile vector be added to \code{df}? If
#'   TRUE (default), a new variable named \code{wealthQuintile} will be added
#'   to \code{df}. If FALSE, a vector coding of wealth quintiles corresponding
#'   to \code{df}
#'
#' @return If \code{add} is TRUE, returns \code{df} with an added variable
#'   named \code{wealthQuintile}. If \code{add} is FALSE, returns a vector
#'   codng of wealth quintiles corresponding to \code{df}
#'
#' @examples
#' ppiDF <- recode_ppi(df = hh)
#' split_to_quintiles(df = ppiDF)
#'
#' @export
#'
#
################################################################################

split_to_quintiles <- function(df, by = NULL, add = TRUE) {
  ##
  wealthQuintile <- cut(df$ppi,
                        breaks = quantile(df$ppi,
                                          probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                          na.rm = TRUE),
                        labels = FALSE)
  ##
  if(!is.null(by)) {
    wealthQuintile <- vector(mode = "numeric", length = nrow(df))
    for(i in unique(df[[by]])) {
      y <- cut(df[df[[by]] == i, "ppi"],
               breaks = quantile(df[df[[by]] == i, "ppi"],
                                 probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                 na.rm = TRUE),
               labels = FALSE)
      wealthQuintile[df[[by]] == i] <- y
    }
  }
  ##
  if(add) {
    z <- df
    z$wealthQuintile <- wealthQuintile
    wealthQuintile <- z
  }
  ##
  return(wealthQuintile)
}

