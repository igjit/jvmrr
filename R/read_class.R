cp_tags <- c(CONSTANT_Utf8 = 1,
             CONSTANT_Class = 7,
             CONSTANT_String = 8,
             CONSTANT_Fieldref = 9,
             CONSTANT_Methodref = 10,
             CONSTANT_NameAndType = 12)

cp_tag_name_of <- name_lookup(cp_tags)

#' Read Java class file
#'
#' @include utils.R
#' @param con connection
#' @export
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
  interfaces_count <- read_u2(con)
  # TODO
  if (interfaces_count > 0) stop()
  fields_count <- read_u2(con)
  # TODO
  if (fields_count > 0) stop()
  methods_count <- read_u2(con)
  methods <- replicate(methods_count, read_method_info(con, constant_pool), simplify = FALSE)
  attributes_count <- read_u2(con)
  attributes <- replicate(attributes_count, read_attribute(con, constant_pool), simplify = FALSE)

  list(magic = magic,
       minor_version = minor_version,
       major_version = major_version,
       constant_pool = constant_pool,
       access_flags = access_flags,
       this_class = this_class,
       this_class_name = this_class_name,
       super_class = super_class,
       super_class_name = super_class_name,
       methods = methods,
       attributes = attributes)
}

read_u1 <- function(con) readBin(con, "integer", 1, 1, FALSE, "big")

read_u2 <- function(con) readBin(con, "integer", 1, 2, FALSE, "big")

read_u4 <- function(con) {
  u2_1 <- read_u2(con)
  u2_2 <- read_u2(con)
  bitwShiftL(u2_1, 16) + u2_2
}

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

read_method_info <- function(con, constant_pool) {
  access_flags <- read_u2(con)
  name_index <- read_u2(con)
  name <- constant_pool[[name_index]]$bytes
  descriptor_index <- read_u2(con)
  descriptor <- constant_pool[[descriptor_index]]$bytes
  attributes_count <- read_u2(con)
  attributes <- replicate(attributes_count, read_attribute(con, constant_pool), simplify = FALSE)
  list(access_flags = access_flags,
       name_index = name_index,
       name = name,
       descriptor_index = descriptor_index,
       descriptor = descriptor,
       attributes_count = attributes_count,
       attributes = attributes)
}

read_attribute <- function(con, constant_pool) {
  attribute_name_index <- read_u2(con)
  attribute_length <- read_u4(con)
  attribute_name <- constant_pool[[attribute_name_index]]$bytes
  switch(attribute_name,
         Code = {
           max_stack <- read_u2(con)
           max_locals <- read_u2(con)
           code_length <- read_u4(con)
           code <- readBin(con, "integer", code_length, 1, FALSE, "big")
           exception_table_length <- read_u2(con)
           # TODO
           if (exception_table_length > 0) stop()
           exception_table <- list()
           attributes_count <- read_u2(con)
           attributes <- replicate(attributes_count, read_attribute(con, constant_pool), simplify = FALSE)
           list(attribute_name_index = attribute_name_index,
                attribute_name = attribute_name,
                attribute_length = attribute_length,
                max_stack = max_stack,
                max_locals = max_locals,
                code_length = code_length,
                code = code,
                exception_table_length = exception_table_length,
                exception_table = exception_table,
                attributes_count = attributes_count,
                attributes = attributes)
         },
         LineNumberTable = {
           line_number_table_length <- read_u2(con)
           line_number_table <- replicate(line_number_table_length,
                                          list(start_pc = read_u2(con),
                                               line_number = read_u2(con)),
                                          simplify = FALSE)
           list(attribute_name_index = attribute_name_index,
                attribute_name = attribute_name,
                attribute_length = attribute_length,
                line_number_table_length = line_number_table_length,
                line_number_table = line_number_table)
         },
         StackMapTable = {
           # TODO: parse stack_map_frame
           readBin(con, "integer", attribute_length, 1, FALSE, "big")
           list(attribute_name_index = attribute_name_index,
                attribute_name = attribute_name,
                attribute_length = attribute_length)
         },
         SourceFile = list(attribute_name_index = attribute_name_index,
                           attribute_name = attribute_name,
                           sourcefile_index = read_u2(con)),
         stop("Not implemented: ", attribute_name))
}
