plotPace <- function(session) {
  .validateRunSession(session)

  pace <- session$pace
  y.max = max(.axis.pace.min, pace)

  plot(
    pace,
    type = "b",
    pch = 19,
    xlab = "Segment",
    ylab = "Pace",
    col = .colours$blue,
    ylim = c(0, y.max),
    xaxt = "n",
    yaxt = "n",
    cex.lab = .8,
    main = "Pace"
  )
  axis(1, at = 1:length(pace), labels = 0:(length(pace) - 1), cex.axis = .8)
  axis(2, at = seq(1, to = y.max, by = 30), labels = .seconds2time(seq(0, to = y.max, by = 30)), cex.axis = .8, las = 1)
  abline(v = seq(6, to = length(pace), by = 5), col = .colours$black, lty = "dashed")
}
