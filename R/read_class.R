name_lookup <- function(v) {
  function(x) names(which(v == x))
}

cp_tags <- c(CONSTANT_Utf8 = 1,
             CONSTANT_Class = 7,
             CONSTANT_String = 8,
             CONSTANT_Fieldref = 9,
             CONSTANT_Methodref = 10,
             CONSTANT_NameAndType = 12)

cp_tag_name_of <- name_lookup(cp_tags)

read_class <- function(con) {
  if (is.character(con)) {
    con <- file(con, "rb")
    on.exit(close(con))
  }

  magic <- readBin(con, "raw", 4, NA, FALSE, "big")
  minor_version <- read_u2(con)
  major_version <- read_u2(con)
  constant_pool_count <- read_u2(con)
  constant_pool <- replicate(constant_pool_count - 1, read_cp_info(con), simplify = FALSE)
  access_flags <- read_u2(con)
  this_class <- read_u2(con)
  this_class_name <- constant_pool[[constant_pool[[this_class]]$name_index]]$bytes
  super_class <- read_u2(con)
  super_class_name <- constant_pool[[constant_pool[[super_class]]$name_index]]$bytes

  list(magic = magic,
       minor_version = minor_version,
       major_version = major_version,
       constant_pool = constant_pool,
       access_flags = access_flags,
       this_class = this_class,
       this_class_name = this_class_name,
       super_class = super_class,
       super_class_name = super_class_name)
}

read_u1 <- function(con) readBin(con, "integer", 1, 1, FALSE, "big")

read_u2 <- function(con) readBin(con, "integer", 1, 2, FALSE, "big")

read_cp_info <- function(con) {
  tag <- read_u1(con)
  tag_name <- cp_tag_name_of(tag)
  info <- switch(tag_name,
                 CONSTANT_Utf8 = {
                   length <- read_u2(con)
                   list(length = length,
                        bytes = intToUtf8(readBin(con, "integer", length, 1, FALSE, "big")))
                 },
                 CONSTANT_Class = list(name_index = read_u2(con)),
                 CONSTANT_String = list(string_index = read_u2(con)),
                 CONSTANT_Fieldref = list(class_index = read_u2(con), name_and_type_index = read_u2(con)),
                 CONSTANT_Methodref = list(class_index = read_u2(con), name_and_type_index = read_u2(con)),
                 CONSTANT_NameAndType = list(name_index = read_u2(con),
                                             descriptor_index = read_u2(con)))

  c(tag = tag, info)
}
