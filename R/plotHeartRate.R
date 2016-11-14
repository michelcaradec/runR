plotHeartRate <- function(session) {
  .validateRunSession(session)

  heart.rate <- session$heart.rate
  profile <- session$profile

  plot.new()
  plot.window(
    xlim = c(0, length(heart.rate)),
    ylim = c(0, max(.axis.heart.rate.max, heart.rate)),
    xaxt = "n"
    #cex.lab = .8,
    #xlab = "Segment",
    #ylab = "Heart Rate",
    #main = "Heart Rate"
  )
  box()

  if (is.object(profile)) {
    if (is.object(profile$zones)) {
      x.max <- length(heart.rate) + 1
      zones <- profile$zones

      rect(0, zones$base[1], x.max, zones$base[2],
           border = NA,
           col = .colours$lightgreen
      )
      rect(0, zones$soft[1], x.max, zones$soft[2],
           border = NA,
           col = .colours$lightorange
      )
      rect(0, zones$hard[1], x.max, zones$hard[2],
           border = NA,
           col = .colours$lightred
      )
    }

    if (!is.na(profile$heart.rate.min)) {
      abline(h = profile$heart.rate.min, lty = "dashed", col = .colours$black)
    }
    if (!is.na(profile$heart.rate.max)) {
      abline(h = profile$heart.rate.max, lty = "dashed", col = .colours$black)
    }
  }

  abline(v = seq(6, to = length(heart.rate), by = 5), col = .colours$black, lty = "dashed")

  points(
    heart.rate,
    pch = 19,
    col = .colours$blue
  )

  x.ticks <- 1:length(heart.rate)
  axis(
    1,
    at = x.ticks,
    labels = x.ticks - 1,
    cex.axis = .8
  )

  y.ticks <- seq(from = 1, to = max(.axis.heart.rate.max, heart.rate), by = 10)
  axis(
    2,
    at = y.ticks,
    labels = y.ticks - 1,
    cex.axis = .8
  )

  title("Heart Rate", xlab = "Segment", ylab = "Heart Rate", cex.lab = .8)
}
