open Instruction
open Stack
open ErrorMsg
open MTypes

/*
    https://tezos.gitlab.io/michelson-reference/#instr-EDIV
*/

module EDIV: InstructionType = {
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
                (false, Error_msg.stack_not_deep_enough("EDIV", Js.Array.length(stack)))
            } else {
                let el_pos = switch options {
                    | None => 0
                    | Some(pos) => pos.el_pos
                }
                // checks if the 2 elements are numeric values
                let first_el = stack[el_pos]
                let second_el = stack[el_pos +1 ]
                switch (first_el.el_type, second_el.el_type) {
                    | (Int | Nat | Mutez, Int | Nat | Mutez) => (true, "")
                    | _ => (false, Error_msg.non_numeric_type("EDIV", `(${m_type_to_string(first_el.el_type)} | ${m_type_to_string(second_el.el_type)})`))
                }                
            }
        }

    let run = (~stack, ~args): result<stack, string> => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?None, ())
        if(is_valid_stack){
            let first_el = stack[args.el_pos]
            let second_el = stack[args.el_pos + 1 ]
            let ediv_result = {
                let ediv = (~number: int, ~divisor: int): option<(int, int)> => {
                    if number === 0 || divisor === 0 {
                        None
                    } else {
                        let quotient = (number / divisor)->Belt.Int.toFloat->Js.Math.floor_int
                        let remainder = mod(number, divisor)
                        Some((quotient, remainder))
                    }
                }
                switch (first_el.value, second_el.value) {
                    | (Int(val1), Int(val2)) => {
                        let pair_type: (m_type, m_type) = (Int, Nat)
                        let option_type: m_type = Pair(pair_type)
                        switch ediv(~number=val1, ~divisor=val2) {
                            | None => Ok(Option({ value: None, el_type: option_type }))
                            | Some((quotient, remainder)) => 
                                let pair = Pair({ value: (Int(quotient), Nat(remainder)), el_type: pair_type})
                                let option = Option({ value: Some(pair), el_type: option_type })
                                Ok(option)
                        }
                    }
                    | (Nat(val1), Nat(val2)) => {
                        let pair_type: (m_type, m_type) = (Nat, Nat)
                        let option_type: m_type = Pair(pair_type)
                        switch ediv(~number=val1, ~divisor=val2) {
                            | None => Ok(Option({ value: None, el_type: option_type }))
                            | Some((quotient, remainder)) => 
                                let pair = Pair({ value: (Nat(quotient), Nat(remainder)), el_type: pair_type})
                                let option = Option({ value: Some(pair), el_type: option_type })
                                Ok(option)
                        }
                    }
                    | (Int(val1), Nat(val2)) => {
                        let pair_type: (m_type, m_type) = (Int, Nat)
                        let option_type: m_type = Pair(pair_type)
                        switch ediv(~number=val1, ~divisor=val2) {
                            | None => Ok(Option({ value: None, el_type: option_type }))
                            | Some((quotient, remainder)) => 
                                let pair = Pair({ value: (Int(quotient), Nat(remainder)), el_type: pair_type})
                                let option = Option({ value: Some(pair), el_type: option_type })
                                Ok(option)
                        }
                    }
                    | (Nat(val1), Int(val2)) => {
                        let pair_type: (m_type, m_type) = (Int, Nat)
                        let option_type: m_type = Pair(pair_type)
                        switch ediv(~number=val1, ~divisor=val2) {
                            | None => Ok(Option({ value: None, el_type: option_type }))
                            | Some((quotient, remainder)) => 
                                let pair = Pair({ value: (Int(quotient), Nat(remainder)), el_type: pair_type})
                                let option = Option({ value: Some(pair), el_type: option_type })
                                Ok(option)
                        }
                    }
                    | (Mutez(val1), Nat(val2)) => {
                        let pair_type: (m_type, m_type) = (Mutez, Mutez)
                        let option_type: m_type = Pair(pair_type)
                        switch ediv(~number=val1, ~divisor=val2) {
                            | None => Ok(Option({ value: None, el_type: option_type }))
                            | Some((quotient, remainder)) => 
                                let pair = Pair({ value: (Mutez(quotient), Mutez(remainder)), el_type: pair_type})
                                let option = Option({ value: Some(pair), el_type: option_type })
                                Ok(option)
                        }
                    }
                    | (Mutez(val1), Mutez(val2)) => {
                        let pair_type: (m_type, m_type) = (Nat, Mutez)
                        let option_type: m_type = Pair(pair_type)
                        switch ediv(~number=val1, ~divisor=val2) {
                            | None => Ok(Option({ value: None, el_type: option_type }))
                            | Some((quotient, remainder)) => 
                                let pair = Pair({ value: (Nat(quotient), Mutez(remainder)), el_type: pair_type})
                                let option = Option({ value: Some(pair), el_type: option_type })
                                Ok(option)
                        }
                    }
                    | _ => Error(Error_msg.wrong_type(
                                    ~instr="MUL", 
                                    ~expected="int, nat or mutez", 
                                    ~received=`(${m_type_to_string(first_el.el_type)} | ${m_type_to_string(second_el.el_type)})`
                                ))
                }
            }
            switch ediv_result {
                | Ok(res) => {
                    // creates a new stack element
                    let new_el = Stack.create_new_el(~el_value=res, ~from_instr="EDIV")
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