open Instruction
open Stack
open ErrorMsg
open MTypes

/*
    https://tezos.gitlab.io/michelson-reference/#instr-CDR
*/

module CDR: InstructionType = {
    let has_parameters = false
    let parameters = 0
    let has_branches = false
    let minimum_stack_depth = 1

    let has_params = () => (has_parameters, parameters)

    let check_stack = (
            ~stack, 
            ~options as _: option<Instruction.run_args>=?, 
            ()
        ): (bool, string) => {
            // checks if the stack is deep enough
            if Js.Array.length(stack) < minimum_stack_depth {
                (false, Error_msg.stack_not_deep_enough("CDR", Js.Array.length(stack)))
            } else {
                (true, "")           
            }
        }

    let run = (~stack, ~args): result<stack, string> => {
        let (is_stack_valid, invalid_stack_msg) = check_stack(~stack, ~options=?Some(args), ())
        if is_stack_valid {
            // unwraps the pair on top of the stack
            let el_to_unpair: stack_element = stack[args.el_pos]
            switch el_to_unpair.value {
                | Pair({ value: (_, right_val) }) => {
                    // creates 2 new elements for the 2 values in the pair
                    let stack_element = Stack.create_new_el(~el_value=right_val, ~from_instr="CDR")
                    // updates the stack at element position
                    switch stack_element {
                        | Ok(el) => {
                            let stack_left_side = stack->Js.Array2.removeCountInPlace(~pos=0, ~count=args.el_pos)
                            let stack_right_side = stack->Js.Array2.removeFromInPlace(~pos=args.el_pos + 1)
                            stack_left_side->Js.Array2.concatMany([[el], stack_right_side])->Ok
                        }
                        | Error(err) => Error(err)
                    }
                }
                | _ => Error(Error_msg.wrong_type(~instr="CDR", ~expected="pair", ~received=m_type_to_string(el_to_unpair.el_type)))
            } 
        } else {
            Error(invalid_stack_msg)
        }
    }
}