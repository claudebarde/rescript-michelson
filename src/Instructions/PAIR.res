open Instruction

module PAIR: InstructionType = {
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        (false, "test")
    }

    let run = (~stack, ~args as _) => Ok(stack)

    let has_params = () => (has_parameters, parameters)
}