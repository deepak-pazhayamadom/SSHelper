#' Estimate Minimum Population Length for Stock Synthesis
#'
#' Estimates a minimum population length (\code{Lmin}) from von Bertalanffy
#' growth parameters for use in setting up a Stock Synthesis (SS3) model.
#'
#' The expected length at the specified minimum age is calculated using the
#' von Bertalanffy Growth Function (VBGF):
#'
#' \deqn{
#' L(t) = L_\infty [1 - \exp\{-k(t-t_0)\}]
#' }
#'
#' The estimated length at \code{tmin} is returned as \code{Lmin}.
#'
#' @param Linf Numeric value giving the asymptotic length
#'   (\eqn{L_\infty}).
#' @param k Numeric value giving the von Bertalanffy growth coefficient.
#' @param tmin Numeric value giving the age corresponding to the minimum
#'   population length to be estimated.
#' @param t0 Numeric value giving the theoretical age at zero length.
#'   Defaults to 0.
#'
#' @return A numeric value giving the estimated minimum population length
#'   (\code{Lmin}) at age \code{tmin}.
#'
#' @details
#' In Stock Synthesis, the size-at-age at the time of settlement is set
#' equal to the smallest population length bin. Young fish are modelled
#' with linear growth until they reach \code{Amin}, after which they
#' transition to the specified growth curve.
#'
#' This function provides an estimate of the minimum population length
#' from the VBGF and the selected minimum age. The resulting value is
#' intended as an initial or starting value when configuring the
#' population length range in an SS3 model.
#'
#' The estimated \code{Lmin} should be checked against the smallest
#' population length bin used in the Stock Synthesis model. If \code{Lmin}
#' is less than the smallest population length bin, SS3 can model young
#' fish as declining in length-at-age until they reach \code{Amin}.
#' Stock Synthesis issues a warning when this occurs.
#'
#' Therefore, the value returned by this function should be considered a
#' starting estimate and should be further evaluated and adjusted during
#' model development and tuning to ensure that the early-life growth
#' trajectory is biologically appropriate and consistent with the
#' population length bins.
#'
#' @examples
#' # Estimate a starting value for the minimum population length
#' Lmin <- estLmin(
#'   Linf = 60,
#'   k = 0.25,
#'   tmin = 1,
#'   t0 = 0
#' )
#'
#' Lmin
#'
#' @export
estLmin <- function(Linf, k, tmin, t0 = 0) {

Lmin <- Linf * (1 - exp(-k * (tmin - t0)))

return(Lmin)
}
