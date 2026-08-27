#' Fill gaps in a catch time series
#'
#' Fills missing years in a discontinuous catch time series with a
#' user-specified catch value. The resulting data frame contains a
#' continuous sequence of years, the specified Stock Synthesis fleet,
#' and the corresponding catch values.
#'
#' @param catch.ts A data frame containing the catch time series.
#'   The first column must contain year and the second column must
#'   contain catch.
#' @param fleetname An integer or character string identifying the
#'   Stock Synthesis fleet.
#' @param start.year An integer specifying the first year of the
#'   output time series.
#' @param end.year An integer specifying the last year of the
#'   output time series.
#' @param value A numeric value used to fill catch for years that are
#'   missing from \code{catch.ts}.
#'
#' @return A data frame containing a complete sequence of years,
#'   the specified fleet, and catch values. Observed catch values are
#'   retained, and missing years are filled with \code{value}.
#'
#' @examples
#' year <- c(1, 3, 5, 8, 14)
#' catch <- c(1000, 500, 300, 400, 140)
#' annual.catch <- data.frame(year, catch)
#'
#' fill_timeseries(
#'   catch.ts = annual.catch,
#'   fleetname = 1,
#'   start.year = 1,
#'   end.year = 20,
#'   value = 10
#' )
#'
#' @export

fill_timeseries <- function(catch.ts, fleetname, start.year, end.year,value){
  
  sub <- catch.ts
  mother <- start.year:end.year
  child  <- unique(sub[,1])
  nil.year <- mother[!(mother %in% child)]
  repeats  <- length(nil.year)
  
  # Format existing data
  sub <- data.frame(
    Year  = sub[,1],
    Fleet = fleetname,
    Catch = sub[,2]
  )
  
  if(repeats > 0){
    
    # Create missing year rows
    result_missing <- data.frame(
      Year  = nil.year,
      Fleet = fleetname,
      Catch = value
    )
    
    # Combine
    result <- rbind(sub, result_missing)
    
    # Report filled years
    message(
      paste0(
        "Fleet '", fleetname,
        "': Missing years filled -> ",
        paste(nil.year, collapse = ", ")
      )
    )
    
  } else {
    
    result <- sub
    
    # Report no missing years
    message(
      paste0(
        "Fleet '", fleetname,
        "': No missing years to fill."
      )
    )
  }
  
  # Sort and clean row names
  result <- result[order(result$Year), ]
  rownames(result) <- NULL
  
  return(result)
}

