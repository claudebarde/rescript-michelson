open Instruction
open ErrorMsg

/*
    https://tezos.gitlab.io/michelson-reference/#instr-DUP
*/

module DUP: InstructionWithOneParam = {
    let has_parameters = true
    let parameters = 1
    let has_branches = false
    let minimum_stack_depth = 1

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        switch options {
            | None => 
                if stack->Js.Array2.length < minimum_stack_depth {
                    (false, Error_msg.stack_not_deep_enough("DUP", stack->Js.Array2.length))
                } else {
                    (true, "")
                }
            | Some(opt) => {
                if stack->Js.Array2.length < opt.el_pos {
                    (false, Error_msg.stack_not_deep_enough("DUP", stack->Js.Array2.length))
                } else {
                    (true, "")
                }
            }
        }
    }

    let run = (~stack, ~args, ~params as _) => {
        // checks if the stack is deep enough
        let (is_valid_stack, err) = check_stack(~stack=stack, ~options=?Some(args), ())
        if is_valid_stack {    
            // duplicates the element
            let dupped_el = stack[args.el_pos]
            // splits the stack in 2
            let stack_left_side = stack->Js.Array2.removeCountInPlace(~pos=0, ~count=args.el_pos)
            let stack_right_side = stack->Js.Array2.removeFromInPlace(~pos=args.el_pos)
            // pushes the new element onto the stack and returns it
            stack_left_side->Js.Array2.concatMany([[dupped_el], stack_right_side])->Ok
        } else {
            Error(err)
        }
    }

    let has_params = () => (has_parameters, parameters)

    let get_params = () => "test"
}