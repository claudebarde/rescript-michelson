open Instruction

/*
    https://tezos.gitlab.io/michelson-reference/#instr-PUSH
*/

module PUSH: InstructionWithTwoParams = {
    let has_parameters = true
    let parameters = 2
    let has_branches = false

    let check_stack = (~stack as _, ~options as _: option<Instruction.run_args>=?, ()) => {
        (false, "test")
    }

    let run = (~stack, ~args as _) => Ok(stack)

    let has_params = () => (has_parameters, parameters)

    let get_params = () => ["test"]
}