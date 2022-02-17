open Instruction
open ErrorMsg
open MTypes
open Stack

/*
    https://tezos.gitlab.io/michelson-reference/#instr-ABS
*/

module ABS: InstructionType = {
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        let el_pos = switch options {
            | None => 0
            | Some(pos) => pos.el_pos
        }
        // stack must have at least 1 element
        // or be deep enough for the provided el_pos
        if Js.Array.length(stack) < 1 || Js.Array.length(stack) < el_pos + 1 {
            (false, Error_msg.stack_not_deep_enough("ABS", Js.Array.length(stack)))
        } else {
            (true, "")
            // element must be of type int
            /*switch stack[el_pos].el_type {
                | Int => (true, "")
                | _ => 
                    (
                        false, 
                        Error_msg.wrong_type(
                            ~instr="ABS", 
                            ~expected="int", 
                            ~received=m_type_to_string(stack[el_pos].el_type)
                        )
                    )
            }*/        
        }
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?Some(args), ())
        if is_valid_stack {
            // creates a new element for the new value
            switch stack[args.el_pos].value {
                | Int(val) => {
                    switch Stack.create_new_el(~el_value=val->Js.Math.abs_int->Nat, ~from_instr="ABS") {
                        | Ok(el) => {
                            // inserts the new element at el_pos
                            stack[args.el_pos] = el
                            // returns the new stack
                            Ok(stack)
                        }
                        | Error(err) => Error(err)
                    }                    
                }
                | _ => Error(Error_msg.wrong_type(
                            ~instr="ABS", 
                            ~expected="int", 
                            ~received=m_type_to_string(stack[args.el_pos].el_type)
                        ))
            }
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}