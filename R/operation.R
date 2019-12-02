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
