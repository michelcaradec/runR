# Constants ----
.axis.pace.min <- 400
.axis.heart.rate.max = 200

# Helper Functions ----
.time2seconds <- function(t) {
  dt <- strptime(t, "%H:%M:%S")
  return(dt$hour * 3600 + dt$min * 60 + dt$sec)
}

.seconds2time <- function(s) {
  return(sprintf("%02d:%02d", s %/% 60, s %% 60))
}

.colours <- list(
  red = rgb(.8, 0, 0, .8),
  green = rgb(0, .8, 0, .8),
  blue = rgb(0, 0, .8, .8),
  black = rgb(0, 0, 0, .8),
  lightgreen = rgb(0, .8, 0, .3),
  lightorange = rgb(1, .5, 0, .3),
  lightred = rgb(.8, 0, 0, .3)
)
