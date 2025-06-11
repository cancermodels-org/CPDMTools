#' Drug Sensitivity Score Calculation
#'
#' @param data_frame A data frame of the 4-parameter model output within the
#' dr4pl_fit function
#' @param readout A character string specifying the readout type. Values include
#' "inhibition", "activity". Defaults to "inhibition".
#' @param activity_threshold A numeric value specifying the minimim actvity
#' (in terms of %inhibition) needed to be counted towards the DSS. Defaults to
#' 10.
#' @param dss_type A numeric value specifying which DSS score to return. Values
#' include 1, 2, 3. Defaults to 3.
#' @param concentration_unit A character string specying the concentration unit
#' needed to convert to molar. Values include "M", "mM", "uM", "nM", "fM".
#' Defaults to "uM"
#' @param log_transform A logical value specifying whether the min and max
#' concentrations need to be log transformed before hand. Defaults to TRUE.
#' @param slope_threshold A numeric value specifying at what slope is too small
#' and sets the DSS score to 0. Defaults to 0.1.
#' @param max_dss A numeric value specying the maximum a DSS score van be,
#' defaults to 100.
#'
#' @returns A numeric value, the Drug Sensitivity Score
#' @export
#'
dss_calculator <- function(data_frame,
                           readout = "inhibition",
                           activity_threshold = 10,
                           dss_type = 3,
                           concentration_unit = "uM",
                           log_transform = TRUE,
                           slope_threshold = 0.1,
                           max_dss = 100) {
  # Create concentration_scale based on concentration_unit
  if (concentration_unit == "molar" |
      concentration_unit == "M") {
    concentration_scale = 1
  }
  if (concentration_unit == "millimolar" |
      concentration_unit == "mM") {
    concentration_scale = 1e-3
  }
  if (concentration_unit == "micromolar" |
      concentration_unit == "uM" |
      concentration_unit == "μM") {
    concentration_scale = 1e-6
  }
  if (concentration_unit == "nanomolar" |
      concentration_unit == "nM") {
    concentration_scale = 1e-9
  }
  if (concentration_unit == "picomolar" |
      concentration_unit == "pM") {
    concentration_scale = 1e-12
  }
  if (concentration_unit == "femtomolar" |
      concentration_unit == "fM") {
    concentration_scale = 1e-15
  }

  # Check concentration and activity threshold inputs
  if (is.null(concentration_scale) & log_transform) {
    stop(paste0(
      "please set e.g. concentration_scale=1e-9; 1e-9 for nano molar",
      " concentrations, 1e-6 for micro molar concentrations and so on."
    ))
  }

  if (activity_threshold <= 0) {
    stop(
      "Percent inhibition must be greater than Zero.",
      " Please, see manual for details."
    )
  }

  # Extract parameters
  ic50 <- as.numeric(data_frame[1, "rel_ic50_est"])
  slope <- as.numeric(data_frame[1, "slope_est"])
  lower_asy <- as.numeric(data_frame[1, "lower_asy_est"]) * 100
  upper_asy <- as.numeric(data_frame[1, "upper_asy_est"]) * 100
  min_conc_raw <- as.numeric(data_frame[1, "min_conc"])
  max_conc <- as.numeric(data_frame[1, "max_conc"])

  # Establish parameters
  if(readout == "inhibition"){
    # Fitting %inhibition with dr4pl_fit function results in negative slope
    # change sign (ie a positive slope if proper and meets slope threshold)
    b <- slope * -1

    # Assign min and max response
    d <- lower_asy # Min response
    a <- upper_asy # Max response

    # Bound the min and max if applicable
    d <- max(-10, d)
    a <- min(110, a)

  }else{
    # For %activity
    # Assign slope
    b <- slope

    # Convert upper and lower asymptote from %activity to %inhibition
    d <- 100 - upper_asy # Min response
    a <- 100 - lower_asy # Max response

    # Bound the min and max if applicable
    d <- max(-10, d)
    a <- min(110, a)

  }

  # Check slope threshold
  if (b < slope_threshold) {
    DSS <- 0
  } else {
    a <- a - d
    d <- d - d

    if (log_transform) {
      min_conc <- log10(min_conc_raw * concentration_scale) #
      x2 <- log10(max_conc * concentration_scale)
    } else {
      min_conc <- min_conc_raw
      x2 <- max_conc
    }

    if (is.na(ic50) || is.na(b) || is.na(a)) {
      DSS <- NA
    } else if (isTRUE(ic50 >= max_conc)) {
      DSS <- 0
    } else if (b < 0) {
      DSS <- 0
    } else {
      if (log_transform) {
        c <- log10(ic50 * concentration_scale)
      } else {
        c <- ic50
      }
      if (a > activity_threshold) {
        if (activity_threshold != 0) {
          x1 <- (c - ((log(a - activity_threshold) -
                         log(activity_threshold - d)) / (b * log(10))))

          if (isTRUE(x1 < min_conc)) {
            x1 <- min_conc
          } else if (isTRUE(x1 > x2)) {
            x1 <- x2
          }
        } else {
          x1 <- min_conc
        }

        # This is a logistic function used in Dotmatics.com
        # activity_threshold = d+(a-d)/(1+10^(b*(c-x)))
        # inverse function
        # x = c - ((log(a-activity_threshold)-
        # log(d-activity_threshold))/(b*log(10)))

        int_y <- (((((a - d) * log(1 + 10^(b * (c - x2)))) /
                      (b * log(10))) + a * x2) -
                    ((((a - d) * log(1 + 10^(b * (c - x1)))) /
                        (b * log(10))) + a * x1)) -
          (activity_threshold * (x2 - x1))
        if (int_y < 0) {
          int_y <- 0
        }
        total_area <- (x2 - min_conc) * (100 - activity_threshold)

        if (dss_type == 1) {
          norm_area <- ((int_y / total_area) * 100) # DSS1
        }

        if (dss_type == 2) {
          norm_area <- ((int_y / total_area) * 100) / log10(a) # DSS2
        }

        if (dss_type == 3) {
          norm_area <- ((int_y / total_area) * 100) *
            (log10(100) / log10(a)) * ((x2 - x1) / (x2 - min_conc)) # DSS3
        }

        DSS <- round(norm_area, digits = 8)
      } else {
        DSS <- 0
      }
    }
  }
  # Bounds DSS if above upper limit
  if (DSS > max_dss) {
    DSS <- max_dss
  }
  return(DSS)
}
