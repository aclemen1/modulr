# File: ./modules/vendor/tool/swissknife.R

library(modulr)

"vendor/tool/swissknife" %provides% {
  list(
    large_blade = "Large blade",
    small_blade = "Small blade",
    scissors = "Scissors",
    led_light = "LED light"
  )
}
