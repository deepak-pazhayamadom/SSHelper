#' Estimate SS3 Logistic Selectivity Parameters from Length Data
#'
#' Estimates a logistic selectivity curve from observed length data and
#' calculates the corresponding L50, L95, and Stock Synthesis (SS3)
#' logistic selectivity parameters.
#'
#' The observed lengths are grouped into length bins and converted to a
#' cumulative length distribution. A binomial generalized linear model
#' (GLM) with a logit link is then fitted to the cumulative counts as a
#' function of length:
#'
#' \deqn{
#' logit(S(L)) = \beta_0 + \beta_1 L
#' }
#'
#' where \eqn{S(L)} is the estimated cumulative proportion at length
#' \eqn{L}. The length at 50\% selectivity is calculated as:
#'
#' \deqn{
#' L50 = -\beta_0 / \beta_1
#' }
#'
#' and the length at 95\% selectivity is calculated as:
#'
#' \deqn{
#' L95 = [\log(19) - \beta_0] / \beta_1
#' }
#'
#' The corresponding SS3 logistic selectivity parameters are then calculated
#' as:
#'
#' \deqn{p_1 = L50}
#'
#' \deqn{p_2 = L95 - L50}
#'
#' These parameters can be used to describe the ascending limb of a
#' logistic selectivity function in Stock Synthesis.
#'
#' @param lengths Numeric vector of observed fish lengths. Missing values
#'   (\code{NA}) are removed before analysis.
#' @param binwidth Numeric value giving the width of the length bins used
#'   to construct the cumulative length distribution. Defaults to 2.
#' @param plot Logical; if \code{TRUE}, plots the observed cumulative
#'   proportions and the fitted logistic selectivity curve. Defaults to
#'   \code{TRUE}.
#'
#' @return A list containing:
#' \describe{
#'   \item{L50}{Estimated length at 50\% selectivity.}
#'   \item{L95}{Estimated length at 95\% selectivity.}
#'   \item{p1}{SS3 logistic selectivity parameter corresponding to L50.}
#'   \item{p2}{SS3 logistic selectivity parameter calculated as
#'   \code{L95 - L50}.}
#'   \item{fit}{The fitted binomial GLM object returned by
#'   \code{\link[stats]{glm}}.}
#' }
#'
#' @details
#' The function first removes missing length observations and groups the
#' remaining observations into length bins of width \code{binwidth}.
#' Cumulative counts are calculated from the resulting length-frequency
#' distribution.
#'
#' The cumulative counts are represented as binomial successes, with the
#' number of failures equal to the total number of observations minus the
#' cumulative count. A logistic regression is fitted to these data to
#' estimate the relationship between length and cumulative proportion.
#'
#' The estimated logistic curve is used to obtain L50 and L95. L50 is the
#' length corresponding to a predicted cumulative proportion of 0.50,
#' while L95 is the length corresponding to a predicted cumulative
#' proportion of 0.95.
#'
#' The SS3 parameters \code{p1} and \code{p2} are calculated from these
#' estimates. In this parameterisation, \code{p1} corresponds to L50 and
#' \code{p2} represents the distance between L50 and L95.
#'
#' The method is intended to provide an empirical starting estimate or
#' visualisation of logistic selectivity parameters from length data.
#' Selectivity in a Stock Synthesis assessment is normally estimated
#' jointly with other model parameters using the available biological and
#' fishery data.
#'
#' @examples
#' # Generate example length data
#' set.seed(123)
#' lengths <- rnorm(500, mean = 40, sd = 8)
#'
#' # Estimate logistic selectivity parameters
#' sel <- estimate_logistic_from_lengths(
#'   lengths = lengths,
#'   binwidth = 2,
#'   plot = TRUE
#' )
#'
#' # Estimated L50 and L95
#' sel$L50
#' sel$L95
#'
#' # SS3 logistic parameters
#' sel$p1
#' sel$p2
#'
#' @export
estimate_logistic_from_lengths <- function(
    lengths,
    binwidth = 2,
    plot = TRUE
) {
  
  # Remove NA
  lengths <- lengths[!is.na(lengths)]
  
  # Create bins
  breaks <- seq(
    floor(min(lengths)),
    ceiling(max(lengths)),
    by = binwidth
  )
  
  hist_data <- hist(
    lengths,
    breaks = breaks,
    plot = FALSE
  )
  
  mid_lengths <- hist_data$mids
  counts <- hist_data$counts
  
  # Cumulative counts
  cum_counts <- cumsum(counts)
  
  total <- sum(counts)
  
  successes <- cum_counts
  failures  <- total - cum_counts
  
  df <- data.frame(
    L = mid_lengths,
    successes = successes,
    failures  = failures
  )
  
  # Proper binomial GLM
  fit <- glm(
    cbind(successes, failures) ~ L,
    data = df,
    family = binomial
  )
  
  # Extract coefficients
  beta0 <- coef(fit)[1]
  beta1 <- coef(fit)[2]
  
  # Calculate L50 and L95
  L50 <- -beta0 / beta1
  L95 <- (log(19) - beta0) / beta1
  
  # SS3 parameters
  p1 <- L50
  p2 <- L95 - L50
  
  # Prediction curve
  Lseq <- seq(min(mid_lengths),
              max(mid_lengths),
              by = 0.1)
  
  pred <- predict(
    fit,
    newdata = data.frame(L = Lseq),
    type = "response"
  )
  
  # Plot
  if (plot) {
    
    plot(mid_lengths,
         successes / total,
         pch = 16,
         xlab = "Length",
         ylab = "Cumulative proportion",
         main = "Estimated Logistic Selectivity")
    
    lines(Lseq,
          pred,
          lwd = 2)
    
    abline(v = L50,
           col = "blue",
           lty = 2,
           lwd = 2)
    
    abline(v = L95,
           col = "red",
           lty = 2,
           lwd = 2)
    
    legend("bottomright",
           legend = c("L50", "L95"),
           col = c("blue", "red"),
           lty = 2,
           bty = "n")
  }
  
  return(list(
    
    L50 = L50,
    L95 = L95,
    
    p1 = p1,
    p2 = p2,
    
    fit = fit
  ))
}