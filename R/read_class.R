name_lookup <- function(v) {
  function(x) names(which(v == x))
}

read_class <- function(con) {
  if (is.character(con)) {
    con <- file(con, "rb")
    on.exit(close(con))
  }

  magic <- readBin(con, "raw", 4, NA, FALSE, "big")
  minor_version <- read_u2(con)
  major_version <- read_u2(con)

  list(magic = magic,
       minor_version = minor_version,
       major_version = major_version)
}

read_u2 <- function(con) readBin(con, "integer", 1, 2, FALSE, "big")
