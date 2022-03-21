#' Fit Model
#'
#' Fit a Beverton-Holt recruitment model.
#'
#' @param data data frame containing \code{S} and \code{R}, spawning biomass and
#'        recruitment.
#' @param parameters vector of initial parameter values.
#' @param quiet whether to suppress gradient messages.
#'
#' @return List containing (TMB) \code{model} and (\code{nlminb}) \code{fit}.
#'
#' @note
#' The formulation used is
#' \eqn{R = R_\mathrm{max} \frac{S}{S+S_{50}}}{R = Rmax * S / (S + S50)}.
#'
#' \eqn{R_\mathrm{max}}{Rmax} is maximum recruitment and \eqn{S_{50}}{S50} is
#' the stock size that produces \eqn{0.50R_\mathrm{max}}{0.50 Rmax}.
#'
#' @examples
#' fm <- fitModel(recdata)
#' fm$fit$par
#' fm$fit$objective
#' report <- sdreport(fm$model)
#' report
#' Rhat <- as.data.frame(summary(report)[rownames(summary(report))=="Rhat",])
#' data.frame(data, Rhat=Rhat$Estimate)
#'
#' @useDynLib bevholt
#'
#' @importFrom stats nlminb
#' @importFrom TMB MakeADFun
#'
#' @export

fitModel <- function(data, parameters=list(logRmax=0, logS50=0, logSigma=0),
                     quiet=TRUE)
{
  model <- MakeADFun(data, parameters, DLL="bevholt", silent=quiet)
  fit <- nlminb(model$par, model$fn, model$gr)
  list(model=model, fit=fit)
}
