open Instruction
open ErrorMsg
open Stack
open MTypes

/*
    https://tezos.gitlab.io/michelson-reference/#instr-CONCAT
*/

module CONCAT: InstructionType = {
    let has_parameters = false
    let parameters = 0
    let has_branches = false
    let minimum_stack_depth = 2

    let check_stack = (~stack, ~options as _: option<Instruction.run_args>=?, ()) => {
        // there must be 2 elements at el_pos on the stack
        if(stack->Js.Array2.length < minimum_stack_depth){
            (false, Error_msg.stack_not_deep_enough("CONCAT", stack->Js.Array2.length))
        } else {
            (true, "")
        }
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?None, ())
        if is_valid_stack {
            // elements on the stack must be string, list of strings, bytes, list of bytes
            // removes the 2 elements on the stack
            switch stack->Js.Array2.removeCountInPlace(~pos=args.el_pos, ~count=2) {
                | [] => Error("Unexpected empty stack for CONCAT instruction")
                | [_] => Error(Error_msg.stack_not_deep_enough("CONCAT", stack->Js.Array2.length))
                | [first_el, second_el] => {
                    // pairs the 2 elements together
                    let new_el = switch (first_el.value, second_el.value) {
                        | (String(left_string), String(right_string)) => {
                            // concats 2 strings
                            let new_string = left_string ++ right_string
                            Stack.create_new_el(~from_instr="CONCAT", ~el_value=String(new_string))
                        }
                        | (List({ el_type: String, value: first_list }), List({ el_type: String, value: second_list })) => {
                            // concats 2 lists of strings
                            let new_list = first_list->Belt.List.concat(second_list)
                            Stack.create_new_el(~from_instr="CONCAT", ~el_value=List({ el_type: String, value: new_list }))
                        }
                        | (Bytes(left_bytes), Bytes(right_bytes)) => {
                            // concats 2 strings of bytes
                            let new_bytes = left_bytes ++ right_bytes
                            Stack.create_new_el(~from_instr="CONCAT", ~el_value=Bytes(new_bytes))
                        }
                        | (List({ el_type: Bytes, value: first_list }), List({ el_type: Bytes, value: second_list })) => {
                            // concats 2 lists of bytes
                            let new_list = first_list->Belt.List.concat(second_list)
                            Stack.create_new_el(~from_instr="CONCAT", ~el_value=List({ el_type: Bytes, value: new_list }))
                        }
                        | _ => Error(Error_msg.wrong_type(
                                        ~instr="CONCAT", 
                                        ~expected="string, list<string>, bytes or list<bytes>",
                                        ~received=`${m_type_to_string(first_el.el_type)} & ${m_type_to_string(second_el.el_type)}`
                                    ))
                    }
                    switch new_el {
                        | Ok(el) => {
                            // inserts the new element at el_pos
                            let stack_left_side = stack->Js.Array2.removeCountInPlace(~pos=0, ~count=args.el_pos)
                            let stack_right_side = stack->Js.Array2.removeFromInPlace(~pos=args.el_pos + 1)
                            // pushes the new element onto the stack and returns it
                            stack_left_side->Js.Array2.concatMany([[el], stack_right_side])->Ok
                        }
                        | Error(err) => Error(err)
                    }
                }
                | _ => Error("Js.Array2.removeCountInPlace returned more than 2 items for CONCAT instruction")
            }            
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}