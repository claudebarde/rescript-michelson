open Instruction
open ErrorMsg
open MTypes
open Stack

let check_stack_with_instr = (~stack, ~instruction, ~minimum_stack_depth, ~options, ()) => {
    let el_pos = switch options {
        | None => 0
        | Some(pos) => pos.el_pos
    }
    // stack must have at least 1 element
    // or be deep enough for the provided el_pos
    if Js.Array.length(stack) < minimum_stack_depth || Js.Array.length(stack) < el_pos + minimum_stack_depth {
        (false, Error_msg.stack_not_deep_enough(instruction, Js.Array.length(stack)))
    } else {
        (true, "")  
    }
}

let create_new_element = (~stack: stack, ~el_pos: int, ~instruction: string) => {
    switch stack[el_pos].value {
        | Int(val) => {
            let comp = {
                if instruction === "EQ" {
                    val === 0 ? Ok(true) : Ok(false)
                } else if instruction === "NEQ" {
                    val === 0 ? Ok(false) : Ok(true)
                } else if instruction === "LT" {
                    val < 0 ? Ok(true) : Ok(false)
                } else if instruction === "GT" {
                    val > 0 ? Ok(true) : Ok(false)
                } else if instruction === "LE" {
                    val <= 0 ? Ok(true) : Ok(false)
                } else if instruction === "GE" {
                    val >= 0 ? Ok(true) : Ok(false)
                } else {
                    Error("Unexpected instruction for generic comparison")
                }
            }
            switch comp {
                | Ok(comp) => 
                    switch Stack.create_new_el(~el_value=Bool(comp), ~from_instr=instruction) {
                        | Ok(el) => {
                            // inserts the new element at el_pos
                            stack[el_pos] = el
                            // returns the new stack
                            Ok(stack)
                        }
                        | Error(err) => Error(err)
                    }   
                | Error(err) => Error(err)
            }                             
        }
        | _ => Error(Error_msg.wrong_type(
                    ~instr=instruction, 
                    ~expected="int", 
                    ~received=m_type_to_string(stack[el_pos].el_type)
                ))
    }
}

module EQ: InstructionType = {
    let minimum_stack_depth = 1
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        check_stack_with_instr(~stack, ~instruction="EQ", ~minimum_stack_depth, ~options, ())
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?Some(args), ())
        if is_valid_stack {
            // creates a new element for the new value
            create_new_element(~stack, ~el_pos=args.el_pos, ~instruction="EQ")
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}

module NEQ: InstructionType = {
    let minimum_stack_depth = 1
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        check_stack_with_instr(~stack, ~instruction="NEQ", ~minimum_stack_depth, ~options, ())
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?Some(args), ())
        if is_valid_stack {
            // creates a new element for the new value
            create_new_element(~stack, ~el_pos=args.el_pos, ~instruction="NEQ")
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}

module LT: InstructionType = {
    let minimum_stack_depth = 1
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        check_stack_with_instr(~stack, ~instruction="LT", ~minimum_stack_depth, ~options, ())
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?Some(args), ())
        if is_valid_stack {
            // creates a new element for the new value
            create_new_element(~stack, ~el_pos=args.el_pos, ~instruction="LT")
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}

module GT: InstructionType = {
    let minimum_stack_depth = 1
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        check_stack_with_instr(~stack, ~instruction="GT", ~minimum_stack_depth, ~options, ())
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?Some(args), ())
        if is_valid_stack {
            // creates a new element for the new value
            create_new_element(~stack, ~el_pos=args.el_pos, ~instruction="GT")
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}

module LE: InstructionType = {
    let minimum_stack_depth = 1
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        check_stack_with_instr(~stack, ~instruction="LE", ~minimum_stack_depth, ~options, ())
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?Some(args), ())
        if is_valid_stack {
            // creates a new element for the new value
            create_new_element(~stack, ~el_pos=args.el_pos, ~instruction="LE")
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}

module GE: InstructionType = {
    let minimum_stack_depth = 1
    let has_parameters = false
    let parameters = 0
    let has_branches = false

    let check_stack = (~stack, ~options: option<Instruction.run_args>=?, ()) => {
        check_stack_with_instr(~stack, ~instruction="GE", ~minimum_stack_depth, ~options, ())
    }

    let run = (~stack, ~args) => {
        let (is_valid_stack, err) = check_stack(~stack, ~options=?Some(args), ())
        if is_valid_stack {
            // creates a new element for the new value
            create_new_element(~stack, ~el_pos=args.el_pos, ~instruction="GE")
        } else {
            Error(err)
        }

    }

    let has_params = () => (has_parameters, parameters)
}