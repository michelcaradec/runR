plotProfile <- function(profile) {
  .validateRunProfile(profile)

  plot.new()
  plot.window(
    c(0, 10),
    c(0, 10),
    mai = rep(0, 4),
    mar = rep(0, 4)
  )
  text(0, 9, paste0("HR min: ", profile$heart.rate.min), cex = .8, pos = 4)
  text(0, 8, paste0("HR max: ", profile$heart.rate.max), cex = .8, pos = 4)

  #abline(v = c(0, 10))
  #abline(h = c(0, 10))
}
