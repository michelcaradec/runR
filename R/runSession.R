.run.session.class.name <- "runSession"

#' Create a running session.
#'
#' @param pace numeric vector giving pace by segment, in seconds
#' @param heart.rate numeric vector giving heart rate by segment, in beats per minute
#' @param profile runner'sprofile
#' @return Running session.
#' @export
#' @examples
#' runSession(sample(seq(10, 13, by = .2), 10, replace = T), rnorm(10, mean = 150))
runSession <- function(pace,
                       heart.rate,
                       profile = NA) {
  session <- list(
    pace = pace,
    heart.rate = heart.rate,
    profile = profile
  )
  class(session) <- .run.session.class.name

  .validateRunSession(session)

  return(session)
}

.validateRunSession <- function(session) {
  if (is.null(session)) {
    stop("Null session")
  }
  if (length(session$pace) <= 0) {
    stop("Empty pace")
  }
  if (length(session$heart.rate) <= 0) {
    stop("Empty heart rate")
  }
  if (length(session$pace) != length(session$heart.rate)) {
    stop("Length mismatch")
  }
  if (!is.na(session$profile)) {
    .validateRunProfile(session$profile)
  }
}

.pace2Speed <- function(pace) {
  return(3600 / pace)
}

# output = speed / heart rate
# The higher the ratio, the better the efficiency
# 0 = mid output for session.
# > 0 = positive output for session.
# < 0 = negative output for session.
.kpiOutput <- function(session) {
  .validateRunSession(session)

  return(scale(.pace2Speed(session$pace) / session$heart.rate))
}

# See https://www.r-bloggers.com/a-simple-guide-to-s3-methods/
summary.runSession <- function(session) {
  .validateRunSession(session)

  nop <- lapply(
    1:length(session),
    function(i) {
      cat(names(session[i]))
      cat("\n")
      print(summary(session[[i]]))
    }
  )
}
