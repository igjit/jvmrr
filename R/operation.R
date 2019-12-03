#' @import dequer
execute <- function(class) {
  constant_pool <- class$constant_pool
  main_method <- class$methods %>%
    detect(~ .$name == "main")
  code <- main_method$attributes %>%
    detect(~ .$attribute_name == "Code") %>%
    .$code

  execute_code(code, constant_pool)
}

execute_code <- function(code, constant_pool) {
  env <- rlang::env(pc = 1, stack = stack(), frame = list())
  while (env$pc <= length(code)) {
    op <- read_operation(code, env)
    execute_operation(op, constant_pool, env)
  }
}

execute_operation <- function(op, constant_pool, env) {
  opcode_name <- opcode_name_of(op$opcode)
  func <- dispatch_table[[opcode_name]]
  if (is.null(func)) stop("Not implemented: ", opcode_name)
  func(op, constant_pool, env)
}

dispatch_table <- list(
  bipush = function (op, constant_pool, env) push(env$stack, op$operands),
  ldc = function (op, constant_pool, env) {
    index <- op$operands
    name <- constant_pool[[constant_pool[[index]]$string_index]]$bytes
    push(env$stack, name)
  },
  getstatic = function (op, constant_pool, env) {
    cp_index <- as_u2(op$operands[1], op$operands[2])
    symbol_name_index <- constant_pool[[cp_index]]
    cls <- constant_pool[[constant_pool[[symbol_name_index$class_index]]$name_index]]$bytes
    field <- constant_pool[[constant_pool[[symbol_name_index$name_and_type_index]]$name_index]]$bytes
    name <- paste(cls, field, sep = ".")
    push(env$stack, name)
  },
  invokevirtual = function (op, constant_pool, env) {
    index <- as_u2(op$operands[1], op$operands[2])
    callee <- constant_pool[[constant_pool[[index]]$name_and_type_index]]
    method_name <- constant_pool[[callee$name_index]]$bytes
    # TODO
    if (method_name != "println") stop("Not implemented: ", method_name)
    args <- pop(env$stack)
    object_name <- pop(env$stack)
    cat(args, "\n", sep = "")
  },
  return = function (...) NULL
)

iconst_i <- map(-1:5, ~ function(op, constant_pool, env) push(env$stack, .))
names(iconst_i) <- paste0("iconst_", c("m1", 0:5))
dispatch_table <- c(dispatch_table, iconst_i)

operation <- function(opcode, operands) {
  structure(list(opcode = opcode, operands = operands), class = "operation")
}

read_operation <- function(code, env) {
  pc <- env$pc
  opcode <- code[pc]
  inst <- instruction_of(opcode)
  if (is.null(inst)) stop("Unknown opcode: ", opcode)
  operands <- if (inst$arity > 0) code[(pc + 1):(pc + inst$arity)]
  env$pc <- pc + 1 + inst$arity
  operation(inst$opcode, operands)
}

as_u2 <- function(byte1, byte2) bitwShiftL(byte1, 8) + byte2
