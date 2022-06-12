#' Fit Beverton-Holt
#'
#' Fit a Beverton-Holt recruitment model.
#'
#' @param data data frame containing \code{S} and \code{R}, spawning biomass and
#'        recruitment.
#' @param parameters vector of initial parameter values.
#' @param quiet whether to suppress gradient messages.
#'
#' @return
#' List containing \code{obj} (TMB model object), \code{opt} (nlminb fit), and
#' \code{report} (sdreport object).
#'
#' @note
#' The formulation used is \eqn{\hat{R} =
#' R_\mathrm{max} \frac{S}{S+S_{50}}}{Rhat = Rmax * S / (S + S50)}.
#'
#' \eqn{R_\mathrm{max}}{Rmax} is maximum recruitment and \eqn{S_{50}}{S50} is
#' the stock size that produces \eqn{0.50R_\mathrm{max}}{0.50 Rmax}.
#'
#' @examples
#' # Fit model
#' fm <- fitBevHolt(recdata)
#' fm$opt$par
#' fm$opt$objective
#'
#' # Summary and confint
#' results <- data.frame(recdata, Rhat=exp(fm$report$value))
#' results$lower <- exp(fm$report$value + qnorm(0.025) * fm$report$sd)
#' results$upper <- exp(fm$report$value + qnorm(0.975) * fm$report$sd)
#'
#' # Plot
#' sorted <- results[order(results$S),]
#' plot(NA, xlim=c(0,800), ylim=c(0,900), xlab="S", ylab="R")
#' polygon(c(sorted$S,rev(sorted$S)), c(sorted$lower, rev(sorted$upper)),
#'         border=NA, col="lightgray")
#' lines(Rhat~S, sorted)
#' points(R~S, sorted)
#'
#' @useDynLib bevholt
#'
#' @importFrom stats nlminb
#' @importFrom TMB MakeADFun sdreport
#'
#' @export

fitBevHolt <- function(data, parameters=list(logRmax=0, logS50=0, logSigma=0),
                     quiet=TRUE)
{
  obj <- MakeADFun(data, parameters, DLL="bevholt", silent=quiet)
  opt <- nlminb(obj$par, obj$fn, obj$gr)
  report <- sdreport(obj)
  list(obj=obj, opt=opt, report=report)
}
