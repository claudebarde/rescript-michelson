open Instruction
open ErrorMsg

/*
    https://tezos.gitlab.io/michelson-reference/#instr-DROP
*/

module DROP: InstructionWithOneParam = {
    let has_parameters = true
    let parameters = 1
    let has_branches = false
    let minimum_stack_depth = 1

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        switch options {
            | None => 
                if stack->Js.Array2.length < minimum_stack_depth {
                    (false, Error_msg.stack_not_deep_enough("DROP", stack->Js.Array2.length))
                } else {
                    (true, "")
                }
            | Some(opt) => {
                if stack->Js.Array2.length < opt.el_pos {
                    (false, Error_msg.stack_not_deep_enough("DROP", stack->Js.Array2.length))
                } else {
                    (true, "")
                }
            }
        }
    }

    let run = (~stack, ~args, ~params as _) => {
        // checks if position is not zero
        if args.el_pos === 0 {
            Error(`Argument for DROP instruction cannot be '0'`)
        } else {
            // checks if the stack is deep enough
            let (is_valid_stack, err) = check_stack(~stack=stack, ~options=?Some(args), ())
            if is_valid_stack {    
                // drops the element        
                let _ = stack->Js.Array2.spliceInPlace(~pos=args.el_pos - 1, ~remove=1, ~add=[])
                stack->Ok
            } else {
                Error(err)
            }
        }
    }

    let has_params = () => (has_parameters, parameters)

    let get_params = () => "test"
}