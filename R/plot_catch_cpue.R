#' Plot catch and CPUE
#'
#' Combines catch across fleets and plots total catch against a CPUE
#' index using a secondary y-axis.
#'
#' @param catch_data A data frame containing catch data.
#' @param cpue_data A data frame containing CPUE index data.
#' @param catch_col Character string giving the name of the catch column.
#' @param year_col Character string giving the name of the year column.
#' @param cpue_year_col Character string giving the name of the year
#'   column in `cpue_data`.
#' @param cpue_index_col Character string giving the name of the CPUE
#'   index column.
#' @param x_tick Numeric value specifying the interval between x-axis
#'   breaks.
#' @param catch_color Character string specifying the colour for catch.
#' @param cpue_color Character string specifying the colour for CPUE.
#'
#' @return A ggplot object showing total catch and CPUE.
#' @examples
#' # Example: Catch and CPUE time series
#'
#' # Create catch data for multiple fleets from 1950 to 2022
#' set.seed(124)
#'
#' years <- 1950:2022
#'
#' catch_data <- data.frame(
#'   Year = rep(years, each = 2),
#'   fleet = rep(c("Fleet 1", "Fleet 2"), length(years))
#' )
#'
#' # Simulate catch with increasing catch through time
#' catch_data$Catch <- ifelse(
#'   catch_data$Year < 1980,
#'   runif(nrow(catch_data), 5, 30),
#'   ifelse(
#'     catch_data$Year < 1995,
#'     runif(nrow(catch_data), 30, 150),
#'     runif(nrow(catch_data), 150, 500)
#'   )
#' )
#'
#'
#' # Create CPUE data for the later part of the time series
#' cpue_years <- 2005:2022
#'
#' cpue_data <- data.frame(
#'   Year = cpue_years,
#'   Index = c(
#'     0.16, 0.17, 0.14, 0.15, 0.13, 0.10,
#'     0.11, 0.15, 0.14, 0.10, 0.11, 0.12,
#'     0.16, 0.20, 0.13, 0.15, 0.12, 0.14
#'   )
#' )
#'
#' # Plot catch and CPUE
#' plot_catch_cpue(
#'   catch_data = catch_data,
#'   cpue_data = cpue_data,
#'   x_tick = 4
#' )
#' @export
#' @importFrom rlang .data
#'
plot_catch_cpue <- function(catch_data, cpue_data,
                            catch_col = "Catch",
                            year_col = "Year",
                            cpue_year_col = "Year",
                            cpue_index_col = "Index",
                            x_tick = 2,
                            catch_color = "blue",
                            cpue_color = "red") {
  
  # Combine catch across fleets
  
  combined_catch <- catch_data |>
    dplyr::filter(.data[[year_col]] != -999) |>
    dplyr::group_by(.data[[year_col]]) |>
    dplyr::summarise(
      total_catch = sum(.data[[catch_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate CPUE scaling factor
  
  max_catch <- max(
    combined_catch$total_catch,
    na.rm = TRUE
  )
  
  max_cpue <- max(
    cpue_data[[cpue_index_col]],
    na.rm = TRUE
  )
  
  if (!is.finite(max_catch) || max_catch <= 0) {
    stop("Catch data must contain at least one positive value.")
  }
  
  if (!is.finite(max_cpue) || max_cpue <= 0) {
    stop("CPUE data must contain at least one positive value.")
  }
  
  scale_factor <- max_catch / max_cpue
  
  # Merge catch and CPUE data
  
  plot_data <- combined_catch |>
    dplyr::left_join(
      cpue_data,
      by = setNames(cpue_year_col, year_col)
    )
  
  # X-axis breaks
  
  x_breaks <- seq(
    from = min(combined_catch[[year_col]], na.rm = TRUE),
    to = max(combined_catch[[year_col]], na.rm = TRUE),
    by = x_tick
  )
  
  # Create plot
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[year_col]])
  ) +
    
  # Catch
  ggplot2::geom_line(
    ggplot2::aes(
      y = total_catch,
      color = "Catch"
    ),
    linewidth = 1
  ) +
    
    ggplot2::geom_point(
      ggplot2::aes(
        y = total_catch,
        color = "Catch"
      ),
      size = 2
    ) +
    
    # CPUE
    ggplot2::geom_line(
      ggplot2::aes(
        y = .data[[cpue_index_col]] * scale_factor,
        color = "CPUE"
      ),
      linewidth = 1,
      linetype = "dashed",
      na.rm = TRUE
    ) +
    
    ggplot2::geom_point(
      ggplot2::aes(
        y = .data[[cpue_index_col]] * scale_factor,
        color = "CPUE"
      ),
      size = 2,
      shape = 17,
      na.rm = TRUE
    ) +
    
    # Y axes
    ggplot2::scale_y_continuous(
      name = "Total Catch",
      sec.axis = ggplot2::sec_axis(
        ~ . / scale_factor,
        name = "CPUE Index"
      )
    ) +
    
    # X axis
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    
    # Colours
    ggplot2::scale_color_manual(
      values = c(
        "Catch" = catch_color,
        "CPUE" = cpue_color
      )
    ) +
    
    # Theme
    ggplot2::theme_minimal() +
    
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    ) +
    
    ggplot2::labs(
      x = "Year",
      title = "Catch and CPUE Comparison"
    )
  
}
