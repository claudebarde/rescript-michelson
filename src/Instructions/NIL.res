open Instruction

module NIL: InstructionWithOneParam = {
    let has_parameters = true
    let parameters = 1
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        (false, "test")
    }

    let run = (~stack, ~args as _) => Ok(stack)

    let has_params = () => (has_parameters, parameters)

    let get_params = () => "test"
}