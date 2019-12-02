name_lookup <- function(v) {
  function(x) names(which(v == x))
}
