#' @import purrr
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
                        instruction("iinc", 132, 2),
                        instruction("ifeq", 153, 2),
                        instruction("ifne", 154, 2),
                        instruction("iflt", 155, 2),
                        instruction("ifge", 156, 2),
                        instruction("ifgt", 157, 2),
                        instruction("ifle", 158, 2),
                        instruction("if_icmpeq", 159, 2),
                        instruction("if_icmpne", 160, 2),
                        instruction("if_icmplt", 161, 2),
                        instruction("if_icmpge", 162, 2),
                        instruction("if_icmpgt", 163, 2),
                        instruction("if_icmple", 164, 2),
                        instruction("goto", 167, 2),
                        instruction("return", 177, 0),
                        instruction("getstatic", 178, 2),
                        instruction("invokevirtual", 182, 2))

instruction_set <- c(instruction_set,
                     map2(paste0("iconst_", c("m1", 0:5)), 2:8, ~ instruction(.x, .y, 0)),
                     map2(paste0("istore_", 0:3), 59:62, ~ instruction(.x, .y, 0)),
                     map2(paste0("iload_", 0:3), 26:29, ~ instruction(.x, .y, 0))) %>%
  set_names(map_chr(., "name"))

opcodes <- instruction_set %>%
  map_dbl("opcode")

opcode_name_of <- name_lookup(opcodes)

with_op <- function(...) {
  env <- rlang::env(rlang::caller_env(), !!! as.list(opcodes))
  rlang::exprs(...) %>%
    map(~ eval(., env)) %>%
    flatten_dbl
}
