# File: ./modules/experimental/arranged_cars.R

library(modulr)

"experimental/arranged_cars" %provides% {
  with_verbosity(0L, with_module_packages({
    if (!"dplyr" %in% rownames(installed.packages()))
      install.packages("dplyr")
    library(dplyr)
    cars %>%
      group_by(speed) %>%
      arrange(desc(dist), .by_group = TRUE) %>%
      ungroup %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      head
  }))
}
