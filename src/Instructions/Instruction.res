open Stack

type run_args = {
    el_pos: int
}
/*type run_args_one_param = {
    el_pos: int,
    param: array<string>
}*/

module type InstructionType = {
    let has_parameters: bool
    let parameters: int
    let has_branches: bool

    // checks if the stack has the right properties to run the instruction
    let check_stack: (~stack: stack, ~options: run_args=?, unit) => (bool, string)
    // runs the instruction
    let run: (~stack: stack, ~args: run_args) => result<stack, string>
    // returns has_parameters and number of parameters
    let has_params: () => (bool, int)
}

module type InstructionWithOneParam = {
    include InstructionType

    // overrides 'run' function to pass the parameters of the instruction
    let run: (~stack: stack, ~args: run_args, ~params: array<string>) => result<stack, string>
    // returns has_parameters and number of parameters
    let has_params: () => (bool, int)
    // gets instruction parameter
    let get_params: () => string
}

module type InstructionWithTwoParams = {
    include InstructionType

    // returns has_parameters and number of parameters
    let has_params: () => (bool, int)
    // gets instruction parameters
    let get_params: () => array<string>
}

type instruction =
    | ADD
    | NIL
    | PAIR
    | PUSH
    | UNPAIR

let string_to_variant: (string) => result<instruction, string> = 
    instr => {
        if instr === "ADD" {
            Ok(ADD)
        } else if instr === "NIL" {
            Ok(NIL)
        } else if instr === "PAIR" {
            Ok(PAIR)
        } else if instr === "PUSH" {
            Ok(PUSH)
        } else if instr === "UNPAIR" {
            Ok(UNPAIR)
        } else {
            Error(`Unknow instruction: ${instr}`)
        }
    }

let variant_to_string: (instruction) => result<string, string> =
    instruction => 
        switch instruction {
            | ADD => Ok("ADD")
            | NIL => Ok("NIL")
            | PAIR => Ok("PAIR")
            | PUSH => Ok("PUSH")
            | UNPAIR => Ok("UNPAIR")
        }