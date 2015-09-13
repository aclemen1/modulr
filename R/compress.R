.compress <- function(object, type = "gzip") {
  memCompress(deparse(object), type = type)
}

.decompress <- function(pack, type = "gzip", serialize = F, ...) {
  eval(parse(text = memDecompress(pack, type = type, asChar = TRUE)), ...)
}
