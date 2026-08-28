#' Fitting LOESS smoothing to abundance index
#'
#' Abundance indices in Stock Synthesis are typically standardised to remove effects that are not related to fishing and are assumed to have a log-normal error structure with units of se of log (index). Fitting a LOESS smoothing function is one way of explaining this variance.
#' @param data A data frame with year in first column and a relative abundance index (e.g. Catch per unit effort) in second column.
#' @param year_col Column name as a string indicating the year in 'data'.
#' @param cpue_col Column name as a string indicating the index in 'data'.
#' @param span_range A vector indicating a range of smoothing parameters to be applied  e.g., c (0.1, 0.4, 0.6)
#' @param chosen_span A value indicating the choice of smoothing parameter to append SEs to the data.
#' @param palette A colour theme as string for graphical display of smoothing parameters applied to the index.
#' @param plot_results TRUE or FALSE to indicate the choice of a graphical display.
#' @return The temperature in degrees Celsius
#' @examples 
#' year<-c(2010:2024)
#' 
#' Std_CPUE<-c(0.25, 0.26, 0.24, 0.31, 0.28, 0.25, 0.23, 0.19,
#'             0.22, 0.35, 0.45, 0.41, 0.42, 0.49, 0.64)
#'             
#' Abundance <-data.frame(Year=year,Index=Std_CPUE)
#' 
#' loess_result <- loess_cpue_multi(
#'   data = Abundance,
#'   year_col = "Year",
#'   cpue_col = "Index",
#'   span_range = seq(0.6, 0.8, by = 0.1),
#'   chosen_span = 0.6
#' )
#' 
#' print(loess_result$updated_data)
#' @export
loess_cpue_multi <- function(data,
                             year_col = "Year",
                             cpue_col = "CPUE",
                             span_range = seq(0.4, 0.7, by = 0.1),
                             chosen_span = 0.6,
                             palette = "Dark 3",
                             plot_results = TRUE) {
  
  # Extract vectors
  year_vals <- data[[year_col]]
  cpue_vals <- data[[cpue_col]]
  
  # Number of spans
  n_spans <- length(span_range)
  
  # Generate dark colour palette
  span_cols <- grDevices::hcl.colors(
    n_spans,
    palette = palette,
    rev = FALSE
  )
  
  # Storage
  loess_models <- list()
  predictions <- list()
  
  # Fit LOESS models
  for (i in seq_along(span_range)) {
    
    sp <- span_range[i]
    model_name <- paste0("span_", sp)
    
    loess_models[[model_name]] <- loess(
      cpue_vals ~ year_vals,
      span = sp,
      data = data
    )
    
    predictions[[model_name]] <- predict(
      loess_models[[model_name]],
      se = TRUE
    )
  }
  
  # Plot
  if (plot_results) {
    
    plot(year_vals,
         cpue_vals,
         type = "l",
         lwd = 2.5,
         col = "black",
         xlab = "Year",
         ylab = cpue_col,
         main = "LOESS smoothing comparison")
    
    legend_names <- c("Standardised CPUE")
    legend_cols  <- c("black")
    legend_lty   <- c(1)
    legend_lwd   <- c(2.5)
    
    for (i in seq_along(span_range)) {
      
      sp <- span_range[i]
      model_name <- paste0("span_", sp)
      
      # Highlight chosen span
      if (sp == chosen_span) {
        
        lines(year_vals,
              predictions[[model_name]]$fit,
              col = span_cols[i],
              lwd = 3,
              lty = 2)  # dashed
        
        legend_lwd <- c(legend_lwd, 3)
        
      } else {
        
        lines(year_vals,
              predictions[[model_name]]$fit,
              col = span_cols[i],
              lwd = 2,
              lty = 2)  # dashed
        
        legend_lwd <- c(legend_lwd, 2)
      }
      
      legend_names <- c(
        legend_names,
        paste0("LOESS ", sp)
      )
      
      legend_cols <- c(
        legend_cols,
        span_cols[i]
      )
      
      legend_lty <- c(legend_lty, 2)
    }
    
    legend("topleft",
           legend_names,
           col = legend_cols,
           lty = legend_lty,
           lwd = legend_lwd,
           bty = "n",
           cex = 0.9)
  }
  
  # Validate chosen span
  chosen_name <- paste0("span_", chosen_span)
  
  if (!chosen_name %in% names(predictions)) {
    stop("chosen_span must be inside span_range")
  }
  
  chosen_pred <- predictions[[chosen_name]]
  
  # Replace SE (SAC format style)
  data$SE <- chosen_pred$se.fit /
    chosen_pred$fit
  
  return(list(
    updated_data = data,
    loess_models = loess_models,
    predictions = predictions,
    chosen_prediction = chosen_pred
  ))
}
