#' Plot running session dashboard.
#'
#' @param session Running session
#' @export
#' @examples
#' s <- runSession(sample(seq(10, 13, by = .2), 10, replace = T), rnorm(10, mean = 150))
#' plotSession(s)
plotSession <- function(session) {
  .validateRunSession(session)

  plot.profile <- F
  if (plot.profile) {
    l <- matrix(
      c(rep(1, 5), 2, rep(c(3, 4), each = 3)),
      2,
      6,
      byrow = T
    )
  } else {
    l <- matrix(
      c(1, 1, 2, 3),
      2,
      2,
      byrow = T
    )
  }

  layout(l)

  plotOutput(session)
  if (plot.profile) {
    plotProfile(session$profile)
  }
  plotPace(session)
  plotHeartRate(session)
}
