#' Function to fill discontinuous time series of catch with a user chosen value
#'
#' Years with missing catch will be filled with a value
#' @param catch.ts A data frame with year in first column and catch in second column. 
#' @param fleetname An integer or string indicating the fleet type in Stock Synthesis.
#' @param start.year An integer indicating the first year of the resultant time series.
#' @param end.year An integer indicating the last year of the resultant time series.
#' @param value User chosen value to fill the years where there is a missing catch.
#' @return A dataframe with complete time series of year, fleet and catch
#' @examples 
#' year<-c(1,3,5,8,14)
#' catch<-c(1000,500,300,400,140)
#' annual.catch<-data.frame(year,catch)
#' 
#' fill_timeseries(
#'                 catch.ts  =annual.catch,
#'                 fleetname =1,
#'                 start.year=1,
#'                 end.year  =20,
#'                 value     =10
#'                )
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

