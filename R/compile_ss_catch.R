#' Compile catch data in Stock Synthesis format
#'
#' Compiles a catch time series for a single fleet into the format
#' required by Stock Synthesis. The function assigns the year, season,
#' and fleet number to each catch observation and specifies the catch
#' standard error. An initial equilibrium catch and its standard error
#' can also be included.
#'
#' @param year A numeric vector specifying the years in the catch
#'   time series.
#' @param seas An integer specifying the season associated with the
#'   catch observations in Stock Synthesis.
#' @param fleet_number An integer specifying the Stock Synthesis fleet
#'   number.
#' @param catch A numeric vector containing catch observations
#'   corresponding to \code{year}.
#' @param catch_se A numeric value specifying the standard error
#'   assigned to catch observations for all years in the time series.
#' @param initial_catch A numeric value specifying the initial
#'   equilibrium catch for the fleet in Stock Synthesis.
#' @param initial_catch_se A numeric value specifying the standard
#'   error of the initial equilibrium catch for the fleet.
#'
#' @return A data frame containing the catch time series formatted
#'   for Stock Synthesis, including year, season, fleet number, catch,
#'   and catch standard error. The initial equilibrium catch and its
#'   standard error are included as specified.
#'
#' @examples
#' compile_ss_catch(
#'   year = c(1990, 1991, 1992, 1993, 1994, 1995),
#'   seas = 3,
#'   fleet_number = 2,
#'   catch = c(2234, 2342, 2253, 2332, 2354, 2300),
#'   catch_se = 0.01,
#'   initial_catch = 100,
#'   initial_catch_se = 0.5
#' )
#'
#' @export

compile_ss_catch<-function(years,
                           seas,
                           fleet_number,
                           catch, 
                           catch_se,
                           initial_catch    =1e-20,
                           initial_catch_se = 0.01){
  catch_ss<-data.frame(year   = years,
                       seas   = seas,
                       fleet  = fleet_number,
                       catch  = catch,
                       catch_se = catch_se)
  initial_catch<-c(-999, seas, fleet_number, initial_catch, initial_catch_se)
  catch_ss<-rbind(initial_catch,catch_ss)
  par(mfrow=c(1,1))
  plot(catch_ss$year[-1],catch_ss$catch[-1],
       xlab="Year", 
       ylab="Catch (tonnes)",
       type="b",
       col="blue",
       pch=16)
  return(catch_ss)
}
