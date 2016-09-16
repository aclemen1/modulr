insertRequiresAddin <- function() {
  rstudioapi::insertText(text = " %requires% ")
}

insertProvidesAddin <- function() {
  rstudioapi::insertText(text = " %provides% ")
}

insertMakeSugarAddin <- function() {
  rstudioapi::insertText(text = " %<=% ")
}
