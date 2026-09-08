#' Visualise and calculate logistic selectivity parameters from L50 and L95
#'
#' Calculates the parameters required to define a logistic selectivity curve
#' in Stock Synthesis (SS3) from the length at 50\% selectivity (L50) and
#' length at 95\% selectivity (L95).
#'
#' The SS3 logistic selectivity function is:
#'
#' \deqn{
#' S(L) = \frac{1}{1 + \exp[-\log(19)(L-p_1)/p_2]}
#' }
#'
#' where \eqn{p_1 = L50} and \eqn{p_2 = L95 - L50}.
#' This parameterisation gives a selectivity of 0.50 at L50 and
#' 0.95 at L95.
#'
#' @param L50 Numeric value giving the length at which selectivity is 50\%.
#' @param L95 Numeric value giving the length at which selectivity is 95\%.
#'   Must be greater than \code{L50}.
#' @param Lmin Numeric value giving the minimum length used to generate the
#'   selectivity curve. Defaults to 0.
#' @param Lmax Numeric value giving the maximum length used to generate the
#'   selectivity curve. Defaults to 100.
#' @param by Numeric increment between consecutive lengths used to generate
#'   the selectivity curve. Defaults to 1.
#' @param plot Logical; if \code{TRUE}, plots the logistic selectivity curve.
#'   Defaults to \code{TRUE}.
#'
#' @return A list containing:
#' \describe{
#'   \item{p1}{The SS3 logistic selectivity parameter corresponding to L50.}
#'   \item{p2}{The difference between L95 and L50, used as the second SS3
#'   logistic selectivity parameter.}
#'   \item{curve}{A data frame containing the length values (\code{L}) and
#'   corresponding selectivity values (\code{Selectivity}).}
#' }
#'
#' @details
#' The relationship between L50, L95 and the SS3 logistic parameters is:
#'
#' \deqn{p_1 = L50}
#'
#' \deqn{p_2 = L95 - L50}
#'
#' The parameter \eqn{p_2} determines the steepness of the ascending limb
#' of the selectivity curve. Smaller values produce a steeper curve,
#' while larger values produce a more gradual increase in selectivity.
#'
#' At \code{L = L50}, selectivity is 0.50, and at
#' \code{L = L95}, selectivity is 0.95.
#'
#' @examples
#' # Calculate SS3 logistic selectivity parameters
#' sel <- SS3_logistic_from_L50L95(
#'   L50 = 30,
#'   L95 = 40,
#'   Lmin = 10,
#'   Lmax = 70,
#'   plot = TRUE
#' )
#'
#' # SS3 parameters
#' sel$p1
#' sel$p2
#'
#' # Selectivity curve
#' head(sel$curve)
#'
#' @export
SS3_logistic_from_L50L95 <- function(L50, L95,
                                     Lmin = 0,
                                     Lmax = 100,
                                     by = 1,
                                     plot = TRUE) {
  
  if (L95 <= L50) {
    stop("L95 must be greater than L50")
  }
  
  # SS3 parameters
  p1 <- L50
  p2 <- L95 - L50
  
  # Length sequence
  L <- seq(Lmin, Lmax, by = by)
  
  # SS3 logistic function
  Sel <- 1 / (1 + exp(-log(19) * (L - p1) / p2))
  
  if (plot) {
    plot(L, Sel,
         type = "l",
         lwd = 2,
         xlab = "Length",
         ylab = "Selectivity",
         main = "SS3 Logistic Selectivity")
    
    abline(v = L50, col = "blue", lty = 2, lwd = 2)
    abline(v = L95, col = "red", lty = 2, lwd = 2)
  }
  
  return(list(
    p1 = p1,
    p2 = p2,
    curve = data.frame(L = L, Selectivity = Sel)
  ))
}