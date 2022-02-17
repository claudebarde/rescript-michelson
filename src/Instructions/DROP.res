open Instruction
open ErrorMsg

/*
    https://tezos.gitlab.io/michelson-reference/#instr-PUSH
*/

module DROP: InstructionWithOneParam = {
    let has_parameters = true
    let parameters = 1
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        switch options {
            | None => (false, "No options provided for function 'check_stack' in DROP module")
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

    let has_params = () => (has_parameters, parameters)

    let get_params = () => "test"
}