.run.zones.class.name <- "runZones"
.run.zones.method.karvonen <- "karvonen"
.run.zones.method.standard <- "standard"

#' Create effort zones for runner.
#'
#' @param profile Runner's profile
#' @param method Calculation method ("standard", "karvonen")
#' @return Effort zones.
#' @export
#' @examples
#' runZones(runProfile(50, 175))
runZones <- function(profile,
                     method = "karvonen") {
  .validateRunProfile(profile)

  zones <- list(
    heart.rate.reserve = NA
  )
  class(zones) <- .run.zones.class.name

  if (!is.na(profile$heart.rate.min) & !is.na(profile$heart.rate.max)) {
    if (method == .run.zones.method.karvonen) {
      # Karvonen Methods
      zones$method <- .run.zones.method.karvonen
      zones$heart.rate.reserve <- profile$heart.rate.max - profile$heart.rate.min
      zones$base <- c(
        profile$heart.rate.min + zones$heart.rate.reserve * .5,
        profile$heart.rate.min + zones$heart.rate.reserve * .7
      )
      zones$soft <- c(
        profile$heart.rate.min + zones$heart.rate.reserve * .7,
        profile$heart.rate.min + zones$heart.rate.reserve * .85
      )
      zones$hard <- c(
        profile$heart.rate.min + zones$heart.rate.reserve * .85,
        profile$heart.rate.min + zones$heart.rate.reserve * .95
      )
    }
    else {
      # Standard Method
      zones$method <- .run.zones.method.standard
      zones$base <- c(
        profile$heart.rate.max * .5,
        profile$heart.rate.max * .7
      )
      zones$soft <- c(
        profile$heart.rate.max * .85,
        profile$heart.rate.max * .9
      )
      zones$hard <- c(
        profile$heart.rate.max * .95,
        profile$heart.rate.max
      )
    }
  }

  .validateRunZones(zones)

  return(zones)
}

.validateRunZones <- function(zones) {
  if (is.null(zones)) {
    stop("Null zones")
  }
  if (class(zones) != .run.zones.class.name) {
    stop("Invalid class")
  }
}
