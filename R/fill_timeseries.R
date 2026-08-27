fill_timeseries <- function(catch.ts, fleetname, start.year, end.year){
  
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
      Catch = 0
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

