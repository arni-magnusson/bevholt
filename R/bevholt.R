#' Beverton-Holt
#'
#' Fit a Beverton-Holt recruitment model.
#'
#' @param S spawning biomass.
#' @param R recruitment.
#' @param par initial parameter values.
#'
#' @return Vector of estimated parameter values.
#'
#' @examples
#' bevholt()
#'
#' @useDynLib bevholt
#'
#' @export

bevholt <- function(S, R, par=list(logRmax=0, logS50=0, logSigma=0))
{
  model <- list(env=list(last.par.best=par))
  best <- model$env$last.par.best
  best
}
