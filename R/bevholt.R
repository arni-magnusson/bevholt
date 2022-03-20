#' Beverton-Holt Model
#'
#' Fit a Beverton-Holt recruitment model.
#'
#' @param data data frame containing \code{S} and \code{R}, spawning biomass and
#'        recruitment.
#' @param parameters vector of initial parameter values.
#' @param quiet whether to suppress gradient messages.
#'
#' @return Vector of estimated parameter values.
#'
#' @examples
#' bevholt(recdata)
#'
#' @useDynLib bevholt
#'
#' @importFrom stats nlminb
#' @importFrom TMB MakeADFun
#'
#' @export

bevholt <- function(data, parameters=list(logRmax=0, logS50=0, logSigma=0),
                    quiet=TRUE)
{
  model <- MakeADFun(data, parameters, DLL="bevholt", silent=quiet)
  fit <- nlminb(model$par, model$fn, model$gr)
  fit$par
}
