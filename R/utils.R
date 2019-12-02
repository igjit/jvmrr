name_lookup <- function(v) {
  function(x) {
    name <- names(which(v == x))
    if (length(name) > 0) name
  }
}
