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
    let minimum_stack_depth: int

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
    | ABS
    | ADD
    | CAR
    | CDR
    | DIP
    | DROP
    | DUP
    | NIL
    | PAIR
    | PUSH
    | SUB
    | UNPAIR

let string_to_variant: (string) => result<instruction, string> = 
    instr =>
        switch instr {
            | "ABS" => Ok(ABS)
            | "ADD" => Ok(ADD)
            | "CAR" => Ok(CAR)
            | "CDR" => Ok(CDR)
            | "DIP" => Ok(DIP)
            | "DROP" => Ok(DROP)
            | "DUP" => Ok(DUP)
            | "NIL" => Ok(NIL)
            | "PAIR" => Ok(PAIR)
            | "PUSH" => Ok(PUSH)
            | "SUB" => Ok(SUB)
            | "UNPAIR" => Ok(UNPAIR)
            | _ => Error(`Unknow instruction: ${instr}`)
        }

let variant_to_string: (instruction) => result<string, string> =
    instruction => 
        switch instruction {
            | ABS => Ok("ABS")
            | ADD => Ok("ADD")
            | CAR => Ok("CAR")
            | CDR => Ok("CDR")
            | DIP => Ok("DIP")
            | DROP => Ok("DROP")
            | DUP => Ok("DUP")
            | NIL => Ok("NIL")
            | PAIR => Ok("PAIR")
            | PUSH => Ok("PUSH")
            | SUB => Ok("SUB")
            | UNPAIR => Ok("UNPAIR")
        }