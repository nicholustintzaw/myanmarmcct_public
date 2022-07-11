################################################################################
#
#' Function to calculate probability of each cluster in a sampling frame to be
#' sampled
#'
#' @param n Population size of cluster
#' @param N Total population
#' @param m Number of clusters
#'
#' @return Numeric value of cluster weight
#'
#' @examples
#' get_cluster_weights(n = 1028, N = 20000, m = 30)
#'
#' @export
#'
#
################################################################################

get_cluster_weights <- function(n, N, m) {
  (n * m) / N
}


################################################################################
#
#' Function to calculate probability of an individual being sampled in a
#' cluster
#'
#' @param n Population size of cluster
#' @param c Number of individuals sampled in a cluster
#'
#' @return Numeric value of probability of an individual being sampled in a
#' cluster
#'
#' @examples
#' get_individual_weights(n = 1028, c = 300)
#'
#' @export
#'
#
################################################################################

get_individual_weights <- function(n, c) {
  c / n
}

################################################################################
#
#' Function to calculate the inverse of the probability of an individual being
#' sampled in the population
#'
#' @param n Population size of cluster
#' @param N Total population
#' @param m Number of clusters
#' @param c Number of individuals to be sampled in a cluster
#'
#' @return Numeric value of inverse probability weight
#'
#' @examples
#' get_weights(n = 1028, N = 20000, m = 30, c = 300)
#'
#' @export
#'
#
################################################################################

get_weights <- function(n, N, m, c) {
  cweight <- get_cluster_weights(n = n, N = N, m = m)
  iweight <- get_individual_weights(n = n, c = c)
  1 / (cweight * iweight)
}
