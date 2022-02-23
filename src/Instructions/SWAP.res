open Instruction
open ErrorMsg
open Stack
open MTypes

/*
    https://tezos.gitlab.io/michelson-reference/#instr-SWAP
*/

module SWAP: InstructionType = {
    let has_parameters = false
    let parameters = 0
    let has_branches = false
    let minimum_stack_depth = 2

    let check_stack = (~stack, ~options as _: option<Instruction.run_args>=?, ()) => {
        // there must be 2 elements at el_pos on the stack
        if(stack->Js.Array2.length < minimum_stack_depth){
            (false, Error_msg.stack_not_deep_enough("SWAP", stack->Js.Array2.length))
        } else {
            (true, "")
        }
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?None, ())
        if is_valid_stack {
            // switches the 2 elements on the stack
            let first_el = stack[args.el_pos]
            let second_el = stack[args.el_pos + 1]
            stack[args.el_pos] = second_el
            stack[args.el_pos + 1] = first_el
            Ok(stack)
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}