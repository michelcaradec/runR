plotOutput <- function(session) {
  .validateRunSession(session)

  pace <- session$pace
  output <- .kpiOutput(session)
  output.spline <- spline(output)

  y.limit <- ceiling(max(abs(range(output))))

  plot(
    output.spline,
    type = "n",
    xlab = "Segment",
    ylab = "Output",
    ylim = c(-y.limit, y.limit),
    xaxt = "n",
    cex.lab = .8,
    main = "Output"
  )
  axis(1, at = 1:length(pace), labels = 0:(length(pace) - 1), cex.axis = .8)

  lines(output.spline, col = .colours$black)
  points(output.spline, pch = 19, col = ifelse(output.spline$y < 0, .colours$red, .colours$green))

  abline(h = 0, lty = "dashed", col = .colours$black)
  abline(v = seq(6, to = length(pace), by = 5), col = .colours$black, lty = "dashed")
}
