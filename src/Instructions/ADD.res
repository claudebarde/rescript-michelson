open Instruction
open Stack
open ErrorMsg
open MTypes

/*
    https://tezos.gitlab.io/michelson-reference/#instr-ADD
*/

module ADD: InstructionType = {
    let has_parameters = false
    let parameters = 0
    let has_branches = false
    let minimum_stack_depth = 2

    let check_stack = (
            ~stack, 
            ~options: option<Instruction.run_args>=?, 
            ()
        ): (bool, string) => {
            // checks if the stack is deep enough (must be at least 2 elements to add)
            if Js.Array.length(stack) < minimum_stack_depth {
                (false, Error_msg.stack_not_deep_enough("ADD", Js.Array.length(stack)))
            } else {
                let el_pos = switch options {
                    | None => 0
                    | Some(pos) => pos.el_pos
                }
                // checks if the 2 elements are numeric values
                let first_el = stack[el_pos]
                let second_el = stack[el_pos +1 ]
                switch (first_el.el_type, second_el.el_type) {
                    | (Int | Nat | Mutez | Timestamp, Int | Nat | Mutez | Timestamp) => (true, "")
                    | _ => (false, Error_msg.non_numeric_type("ADD", `(${m_type_to_string(first_el.el_type)} | ${m_type_to_string(second_el.el_type)})`))
                }                
            }
        }

    let run = (~stack, ~args): result<stack, string> => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?None, ())
        if(is_valid_stack){
            let first_el = stack[args.el_pos]
            let second_el = stack[args.el_pos + 1 ]
            let add_result = 
                switch (first_el.value, second_el.value) {
                    | (Int(val1), Int(val2)) => Ok(Int(val1 + val2))
                    | (Nat(val1), Nat(val2)) => Ok(Nat(val1 + val2))
                    | (Int(val1), Nat(val2)) => Ok(Int(val1 + val2))
                    | (Nat(val1), Int(val2)) => Ok(Int(val1 + val2))
                    | (Mutez(val1), Mutez(val2)) => Ok(Mutez(val1 + val2))
                    | (Timestamp(val1), Int(val2)) => Ok(Timestamp(val1 + val2))
                    | (Int(val1), Timestamp(val2)) => Ok(Timestamp(val1 + val2))
                    | _ => Error("Invalid data type for ADD instruction")
                }
            switch add_result {
                | Ok(res) => {
                    // creates a new stack element
                    let new_el = Stack.create_new_el(~el_value=res, ~from_instr="ADD")
                    switch new_el {
                        | Ok(el) => {
                            // removes the previous 2 elements from the stack
                            let stack_left_side = stack->Js.Array2.removeCountInPlace(~pos=0, ~count=args.el_pos)
                            let stack_right_side = stack->Js.Array2.removeFromInPlace(~pos=args.el_pos + 2)
                            // pushes the new element onto the stack and returns it
                            stack_left_side->Js.Array2.concatMany([[el], stack_right_side])->Ok
                        }
                        | Error(err) => Error(err)
                    }
                }
                | Error(err) => Error(err)
            }
        } else {
            Error(err)
        }
    }

    let has_params = () => (has_parameters, parameters)
}