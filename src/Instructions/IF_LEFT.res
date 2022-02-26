open Instruction
open Stack
open MTypes
open ErrorMsg

/*
    https://tezos.gitlab.io/michelson-reference/#instr-IF_LEFT
*/

module IF_LEFT: InstructionWithTwoParams = {
    let has_parameters = true
    let parameters = 2
    let has_branches = true
    let minimum_stack_depth = 1

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        let el_pos = switch options {
            | None => 0
            | Some(pos) => pos.el_pos
        }
        // stack must have at least 1 element
        // or be deep enough for the provided el_pos
        if Js.Array.length(stack) < minimum_stack_depth || Js.Array.length(stack) < el_pos + minimum_stack_depth {
            (false, Error_msg.stack_not_deep_enough("IF_LEFT", Js.Array.length(stack)))
        } else {
            (true, "")  
        }
    }

    let run = (~stack, ~args, ~params) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?Some(args), ())
        if is_valid_stack {
            // args array must have 2 elements
            if params->Js.Array2.length !== 2 {
                Error(`The "args" array for IF_LEFT must have exactly 2 elements`)
            } else {
                // value on stack must be a union
                switch stack[args.el_pos].value {
                    | Or({ value }) => {
                        // chooses a branch according to value on stack
                        switch value {
                            | Left(val) =>
                                // creates a new stack element and pushes it to the stack
                                switch Stack.create_new_el(~from_instr="IF_LEFT", ~el_value=val) {
                                    | Ok(el) => {
                                        let _ = [el]->Js.Array2.concat(stack)
                                        // parses and runs the first element of the args for the IF_LEFT instruction
                                        /*let run_output = Remich.run_code_from_json(
                                            ~code=michelson_test.contract, 
                                            ~stack=initial_stack, 
                                            ~storage_type=michelson_test.storage_type
                                            )*/
                                        Error("test")
                                    }
                                    | Error(err) => Error(err)
                                }                                
                            | Right(val) => Error("test")
                        }
                    }
                    | _ => Error(Error_msg.unexpected_value("union", m_type_to_string(stack[args.el_pos].el_type)))
                }
            }
        } else {
            Error(err)
        }
    }

    let has_params = () => (has_parameters, parameters)

    let get_params = () => ["test"]
}