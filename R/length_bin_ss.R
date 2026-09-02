#' Format length-composition data for Stock Synthesis
#'
#' Converts individual length measurements and sampling-event identifiers
#' into length-composition data in the format required by Stock Synthesis.
#' Length measurements are grouped by year and assigned to the specified
#' length bins.
#'
#' @param L.data A data frame in which the first three columns contain,
#'   respectively, year, length measurement, and a unique sampling-event
#'   identifier.
#' @param int Numeric value specifying the length-bin interval.
#' @param month Integer indicating the month within a season (typically 1 season of 12 months within a year).
#' @param fleet Integer indicating the fleet number used in Stock Synthesis.
#' @param sex Integer indicating sex composition:
#'   \code{0} = combined, \code{1} = females only, \code{2} = males only,
#'   \code{3} = sex ratio preserved.
#' @param part Integer indicating the sample component:
#'   \code{0} = combined, \code{1} = discards only, \code{2} = retained only.
#' @param max_len Numeric value specifying the upper limit used to define
#'   the length bins. If \code{NULL}, the maximum observed length is used.
#'
#' @return A data frame containing Stock Synthesis length-composition data
#'   with columns for year, month, fleet, sex, part, number of samples,
#'   and length-frequency bins.
#' @examples
#' # Example length-composition data
#' length_data <- data.frame(
#'   Year = c(
#'     2015, 2015, 2015, 2015, 2015,
#'     2016, 2016, 2016, 2016, 2016,
#'     2017, 2017, 2017, 2017, 2017
#'   ),
#'   Length = c(
#'     28, 31, 34, 36, 40,
#'     25, 29, 32, 35, 38,
#'     27, 30, 33, 37, 42
#'   ),
#'   UniqueTrip = c(
#'     "2015_01", "2015_01", "2015_02", "2015_02", "2015_02",
#'     "2016_01", "2016_01", "2016_01", "2016_02", "2016_02",
#'     "2017_01", "2017_01", "2017_02", "2017_02", "2017_02"
#'   )
#' )
#'
#' # Convert length data to Stock Synthesis format
#' length_bin_ss(
#'   L.data = length_data,
#'   int = 2,
#'   month = 7,
#'   fleet = 1,
#'   sex = 0,
#'   part = 0,
#'   max_len = 50
#' )
#' @export
length_bin_ss <- function(
    L.data,
    int,
    month,
    fleet,
    sex,
    part,
    max_len = NULL
) {

  # Basic input checks
  if (!is.data.frame(L.data)) {
    stop("L.data must be a data frame.")
  }

  if (ncol(L.data) < 3) {
    stop("L.data must contain at least three columns.")
  }

  if (!is.numeric(int) || length(int) != 1 || int <= 0) {
    stop("int must be a single positive numeric value.")
  }

  if (!is.null(max_len) &&
      (!is.numeric(max_len) || length(max_len) != 1 || max_len <= 0)) {
    stop("max_len must be NULL or a single positive numeric value.")
  }

  # Extract required columns by position
  year <- L.data[[1]]
  length_data <- as.numeric(L.data[[2]])
  sample_id <- L.data[[3]]

  # Determine upper length limit
  if (is.null(max_len)) {
    max_len <- max(length_data, na.rm = TRUE)
  }

  # Define length bins
  lbins <- seq(0, max_len + int, by = int)
  n <- length(lbins) - 1

  bin_levels <- levels(
    cut(lbins, lbins, include.lowest = TRUE)
  )

  # Unique years
  years <- sort(unique(year))

  # Store results for each year
  results <- vector("list", length(years))

  for (i in seq_along(years)) {

    yr <- years[i]

    # Select data for the current year
    idx <- year == yr

    fl <- length_data[idx]
    samples <- sample_id[idx]

    # Number of unique sampling events
    sa <- length(unique(na.omit(samples)))

    # Assign lengths to bins
    bins <- cut(
      fl,
      breaks = lbins,
      include.lowest = TRUE
    )

    counts <- table(
      factor(bins, levels = bin_levels)
    )

    # Stock Synthesis length-composition row
    results[[i]] <- c(
      yr,
      month,
      fleet,
      sex,
      part,
      sa,
      as.numeric(counts),
      rep(0, n)
    )
  }

  # Combine years
  lf <- as.data.frame(
    do.call(rbind, results),
    stringsAsFactors = FALSE
  )

  # Calculate total fish
  num_fish <- colSums(
    lf[, -(1:6), drop = FALSE]
  )

  # Summary information
  num_years <- nrow(lf)
  num_samples <- sum(lf[[6]])

  bin_names <- c(
    paste0("f", lbins[-1]),
    paste0("m", lbins[-1])
  )

  # Display summary
  if (num_years > 1) {

    lf_summary <- lf[, -c(2:5), drop = FALSE]

    colnames(lf_summary) <- c(
      "Year",
      "Samples",
      bin_names
    )

    lf_summary_sum <- data.frame(
      Years = num_years,
      Samples = num_samples,
      t(num_fish)
    )

    colnames(lf_summary_sum) <- c(
      "Years",
      "Samples",
      bin_names
    )

    rownames(lf_summary) <- NULL
    rownames(lf_summary_sum) <- NULL

    print(lf_summary)

    cat("\nTotal Fish\n")
    print(sum(num_fish))

    print(lf_summary_sum)

  } else {

    cat("\nOne year data, report returned\n")
  }

  # Add bin-boundary row
  lf <- rbind(
    lf,
    c(
      lbins,
      rep(NA, 6 + (n - 1))
    )
  )

  # Convert to data frame and assign column names
  lf <- as.data.frame(lf)
  rownames(lf) <- NULL

  colnames(lf) <- c(
    "Year",
    "Month",
    "Fleet",
    "Sex",
    "Part",
    "Samples",
    bin_names
  )
  
  lf<-lf[-length(lf[,1]),]

  return(lf)
}

