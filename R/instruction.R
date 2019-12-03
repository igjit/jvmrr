#' @import purrr
#' @importFrom magrittr %>%
#' @include utils.R
instruction_of <- function(opcode) {
  name <- opcode_name_of(opcode)
  if (!is.null(name)) instruction_set[[name]]
}

instruction <- function(name, opcode, arity) {
  structure(list(name = name, opcode = opcode, arity = arity), class = "instruction")
}

instruction_set <- list(instruction("bipush", 16, 1),
                        instruction("ldc", 18, 1),
                        instruction("iadd", 96, 0),
                        instruction("isub", 100, 0),
                        instruction("imul", 104, 0),
                        instruction("idiv", 108, 0),
                        instruction("irem", 112, 0),
                        instruction("return", 177, 0),
                        instruction("getstatic", 178, 2),
                        instruction("invokevirtual", 182, 2))

instruction_set <- c(instruction_set,
                     map2(paste0("iconst_", c("m1", 0:5)), 2:8, ~ instruction(.x, .y, 0)),
                     map2(paste0("istore_", 0:3), 59:62, ~ instruction(.x, .y, 0)),
                     map2(paste0("iload_", 0:3), 26:29, ~ instruction(.x, .y, 0)))

instruction_set_name <- instruction_set %>%
  map_chr(~ .$name)

names(instruction_set) <- instruction_set_name

opcodes <- instruction_set %>%
  map_dbl(~ .$opcode)
names(opcodes) <- instruction_set_name

opcode_name_of <- name_lookup(opcodes)
