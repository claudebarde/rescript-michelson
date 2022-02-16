open Instruction
open ErrorMsg
open Stack
open MTypes

module PAIR: InstructionType = {
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options as _: option<Instruction.run_args>=?, ()) => {
        // there must be 2 elements at el_pos on the stack
        if(stack->Js.Array2.length < 2){
            (false, Error_msg.stack_not_deep_enough("PAIR", stack->Js.Array2.length))
        } else {
            (true, "")
        }
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack=stack, ~options=?None, ())
        if is_valid_stack {
            // removes the 2 elements on the stack
            let [left_el, right_el] = stack->Js.Array2.removeCountInPlace(~pos=args.el_pos, ~count=2)
            // pairs the 2 elements together
            let new_el = Stack.create_new_el(
                ~el_value=Pair({ 
                    value: (left_el.value, right_el.value), 
                    el_type: (m_value_to_type(left_el.value), m_value_to_type(right_el.value)) 
                }),
                ~from_instr="PAIR"
            )
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
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}