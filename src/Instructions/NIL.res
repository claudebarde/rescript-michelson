open Instruction
open MTypes
open Stack

/*
    https://tezos.gitlab.io/michelson-reference/#instr-NIL
*/

module NIL: InstructionWithOneParam = {
    let has_parameters = true
    let parameters = 1
    let has_branches = false

    let check_stack = (~stack as _, ~options as _: option<Instruction.run_args>=?, ()) => {
        // nothing to check
        (true, "")
    }

    let run = (~stack, ~args, ~params) => {
        // gets the element type from the string in Michelson code
        switch MTypes.string_to_m_type(list{ { base: params[0], params: list{} } }) {
            | Ok(t) => {
                // creates a new stack element
                let new_list = List({ value: list{}, el_type: t })
                switch Stack.create_new_el(~el_value=new_list, ~from_instr="NIL") {
                    | Ok(el) => {
                        // pushes the new element to the stack
                        let stack_left_side = stack->Js.Array2.removeCountInPlace(~pos=0, ~count=args.el_pos)
                        let stack_right_side = stack->Js.Array2.removeFromInPlace(~pos=args.el_pos)
                        // pushes the new element onto the stack and returns it
                        stack_left_side->Js.Array2.concatMany([[el], stack_right_side])->Ok
                    }
                    | Error(err) => Error(err)
                }                
            }
            | Error(err) => Error(err)
        }        
    }

    let has_params = () => (has_parameters, parameters)

    let get_params = () => "test"
}