open Instruction
open Stack
open MTypes

/*
    https://tezos.gitlab.io/michelson-reference/#instr-PUSH
*/

module PUSH: InstructionWithTwoParams = {
    let has_parameters = true
    let parameters = 2
    let has_branches = false
    let minimum_stack_depth = 0

    let check_stack = (~stack as _, ~options as _: option<Instruction.run_args>=?, ()) => {
        // not necessary
        (true, "")
    }

    let run = (~stack, ~args, ~params) => {
        // args array must have 2 elements
        if params->Js.Array2.length !== 2 {
            Error(`The "args" array for PUSH must have exactly 2 elements`)
        } else {
            // parses the type and the value in the array
            switch parse_json_to_type(params[0]) {
                | Ok(type_to_push) => {
                    switch parse_json_to_value(params[1], type_to_push) {
                        | Ok(value_to_push) => {
                            // creates a new stack element
                            switch Stack.create_new_el(~el_value=value_to_push, ~from_instr="PUSH") {
                                | Ok(el) => 
                                    // inserts the new element at el_pos
                                    let stack_left_side = stack->Js.Array2.removeCountInPlace(~pos=0, ~count=args.el_pos)
                                    let stack_right_side = stack->Js.Array2.removeFromInPlace(~pos=args.el_pos)
                                    // pushes the new element onto the stack and returns it
                                    stack_left_side->Js.Array2.concatMany([[el], stack_right_side])->Ok
                                | Error(err) => Error(err)
                            }
                        }
                        | Error(err) => Error(err)
                    }
                }
                | Error(err) => Error(err)
            }
        }
    }

    let has_params = () => (has_parameters, parameters)

    let get_params = () => ["test"]
}