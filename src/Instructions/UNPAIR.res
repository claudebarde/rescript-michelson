open Instruction
open Stack
open ErrorMsg
open MTypes

/*
    https://tezos.gitlab.io/michelson-reference/#instr-UNPAIR
*/

module UNPAIR: InstructionType = {
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let has_params = () => (has_parameters, parameters)

    let check_stack = (
            ~stack, 
            ~options as _: option<Instruction.run_args>=?, 
            ()
        ): (bool, string) => {
            // checks if the stack is deep enough
            if Js.Array.length(stack) == 0 {
                (false, Error_msg.stack_not_deep_enough("UNPAIR", 0))
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
                | Pair({ value: (left_val, right_val) }) => {
                    // creates 2 new elements for the 2 values in the pair
                    let stack_element_left = Stack.create_new_el(~el_value=left_val, ~from_instr="UNPAIR")
                    let stack_element_right = Stack.create_new_el(~el_value=right_val, ~from_instr="UNPAIR")
                    // updates the stack at element position
                    switch (stack_element_left, stack_element_right) {
                        | (Ok(left_el), Ok(right_el)) => {
                            let stack_left_side = stack->Js.Array2.removeCountInPlace(~pos=0, ~count=args.el_pos)
                            let stack_right_side = stack->Js.Array2.removeFromInPlace(~pos=args.el_pos + 1)
                            let new_stack = Js.Array2.concatMany(stack_left_side, [[left_el, right_el], stack_right_side])
                            Ok(new_stack)
                        }
                        | (Ok(_), Error(err)) => Error(err)
                        | (Error(err), Ok(_)) => Error(err)
                        | (Error(err1), Error(err2)) => Error(j`$err1 / $err2`)
                    }
                }
                | _ => Error(Error_msg.wrong_type(~instr="UNPAIR", ~expected="pair", ~received=m_type_to_string(el_to_unpair.el_type)))
            } 
        } else {
            Error(invalid_stack_msg)
        }
    }
}