.run.profile.class.name <- "runProfile"
.gender.masculin <- "m"
.gender.feminin <- "f"

#' Create a profile for runner.
#'
#' @param heart.rate.min Minimum heart rate
#' @param heart.rate.max Maximum heart rate
#' @param gender Gender
#' @param age Age
#' @param weight Weight in kilograms
#' @return Runner's profile.
#' @export
#' @examples
#' runProfile(50, 175)
runProfile <- function(heart.rate.min = NA,
                       heart.rate.max = NA,
                       gender = .gender.masculin,
                       age = NA,
                       weight = NA) {
  profile <- list(
    heart.rate.min = heart.rate.min,
    heart.rate.max = heart.rate.max,
    gender = gender,
    age = age,
    weight = weight
  )
  class(profile) <- .run.profile.class.name
  if (is.na(heart.rate.max) & !is.na(age)) {
    profile$heart.rate.max <- .maxHeartRate(age, gender)
  }
  profile$zones <- runZones(profile)

  .validateRunProfile(profile)

  return(profile)
}

.validateRunProfile <- function(profile) {
  if (is.null(profile)) {
    stop("Null profile")
  }
  if (class(profile) != .run.profile.class.name) {
    stop("Invalid class")
  }
  if (!(profile$gender %in% c(.gender.masculin, .gender.feminin))) {
    stop("Invalid gender")
  }
}

# Astrand Method
.maxHeartRate <- function(age, gender = .gender.masculin) {
  return(ifelse(gender == .gender.feminin, 226, 220) - age)
}

# See https://www.r-bloggers.com/a-simple-guide-to-s3-methods/
summary.runProfile <- function(profile) {
  .validateRunProfile(profile)

  nop <- lapply(
    1:length(profile),
    function(i) {
      cat(names(profile[i]))
      cat("\n")
      print(summary(profile[[i]]))
    }
  )
}
