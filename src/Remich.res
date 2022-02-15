open Instructions
open MTypes
open Stack
open ErrorMsg

type rec ast_element = {
    instruction: Instruction.instruction,
    branches: array<ast_element>,
    params: array<string>
}

type ast = array<ast_element>

module Remich = {
    // Parses the Michelson code for parameters and storage type and values
    /*let parse_m_type_value = (m_code: string, m_code_type: string): m_value => {

    }*/
    // Initializes the stack
    let init_stack = ((left_el: m_value, right_el: m_value)): result<stack, string> => {
        //let initial_stack_element_res = Stack.create_new_el(~el_type=Pair((params_type, storage_type)), ~from_instr="INIT", ~value=initial_stack_value)
        let initial_stack_element_res = 
            Stack.create_new_el(
                ~el_value=Pair({ value: (left_el, right_el), el_type: (m_value_to_type(left_el), m_value_to_type(right_el))}), 
                ~from_instr="INIT")

        switch initial_stack_element_res {
            | Ok(el) => {
                Ok([el])
            }
            | Error(err) => Error(err)
        }
    }
    // Parses given Michelson code into AST
    let parse_to_ast = (code: string): ast => {
        let formatted_code = 
            code
            ->Js.String2.replaceByRe(%re("/\\n/g"), " ")
            ->Js.String2.replaceByRe(%re("/\\s{2,}/g"), " ")
            ->Js.String2.split("")

        let current_instruction = ref("")
        let found_instructions: ast = []

        let cursor_position = ref(0)
        let break_parsing = ref(false)
        while cursor_position.contents < Js.Array.length(formatted_code) && !break_parsing.contents {
            let index = cursor_position.contents
            let char = formatted_code[index]
        //Js.Array.forEachi((char, index) => {
            if char == " " || char == ";" {
                // spaces and semi-colons are ignored
                // TODO: analyze the content of 'current_instruction'
                current_instruction := ""
                cursor_position := cursor_position.contents + 1
            } else {
                current_instruction.contents = current_instruction.contents ++ char
                // if valid instruction
                if Js.Array.includes(current_instruction.contents, Instructions.valid_instructions) {
                    let instr = Instruction.string_to_variant(current_instruction.contents)
                    let output: result<unit, string> = 
                        switch instr {
                            | Ok(val) => {
                                switch val {
                                    | ADD => {
                                        let ast_el: ast_element = {
                                            instruction: ADD,
                                            branches: [],
                                            params: []
                                        }
                                        let _ = Js.Array.push(ast_el, found_instructions)
                                        Ok()
                                    }
                                    | NIL => {
                                        // finds the associated type
                                        let raw_type_output =
                                            formatted_code
                                            ->Js.Array2.sliceFrom(index + 1)
                                            ->Js.Array2.joinWith("")
                                            ->Js.String2.splitByReAtMost(%re("/;|\}/"), ~limit=1)
                                        if Js.Array.length(raw_type_output) === 1 {
                                            switch raw_type_output[0] {
                                                | None => Error(`No type found for NIL at index ${Belt.Int.toString(index - 2)}`)
                                                | Some(v) => {
                                                    let v = Js.String2.trim(v)
                                                    // checks if valid type
                                                    if Js.Array2.includes(valid_types, v) {
                                                        let ast_el: ast_element = {
                                                            instruction: NIL,
                                                            branches: [],
                                                            params: [v]
                                                        }
                                                        let _ = Js.Array.push(ast_el, found_instructions)
                                                        Ok()
                                                    } else {
                                                        Error(`Unknown type "${v}" after NIL instruction`)
                                                    }                                                    
                                                }
                                            }
                                        } else {
                                            Error("Cannot find NIL associated type")
                                        }
                                    }
                                    | PAIR => {
                                        let ast_el: ast_element = {
                                            instruction: PAIR,
                                            branches: [],
                                            params: []
                                        }
                                        let _ = Js.Array.push(ast_el, found_instructions)
                                        Ok()
                                    }
                                    | PUSH => {
                                        let ast_el: ast_element = {
                                            instruction: PUSH,
                                            branches: [],
                                            params: []
                                        }
                                        let _ = Js.Array.push(ast_el, found_instructions)
                                        Ok()
                                    }
                                    | UNPAIR => {
                                        let ast_el: ast_element = {
                                            instruction: UNPAIR,
                                            branches: [],
                                            params: []
                                        }
                                        let _ = Js.Array.push(ast_el, found_instructions)
                                        Ok()
                                    }
                                }                                
                            }
                            | Error(err) => {
                                Js.log(err)
                                Error(`Unknown instruction: ${current_instruction.contents}`)
                            }
                        }
                        switch output {
                            | Ok(_) => {
                                current_instruction.contents = ""
                                cursor_position := cursor_position.contents + 1
                            }
                            | Error(err) => {
                                Js.log(err)
                                break_parsing := true
                            }
                        }
                } else {
                    cursor_position := cursor_position.contents + 1
                }
            }
        //}, formatted_code)
        }

        found_instructions
    }

    /*%%private(
        // runs Michelson code from AST
        let rec read_ast = (ast: ast): result<stack_element, string> => {
            for i in 0 to Js.Array.length(ast) - 1 {
                let ast_element = ast[i]
                // if instruction has branches, branch is sent to the read_ast function
                if Js.Array.length(ast_element.branches) > 0 {
                    read_ast(ast_element.branches)
                } else {
                    let { instruction, params } = ast_element
                    switch instruction {
                        | ADD => 
                        | NIL =>
                        | PAIR =>
                        | PUSH =>
                        | UNPAIR =>
                        | _ => Error("Unknown instruction ${Instruction.variant_to_string(instruction)}")
                    }
                }
            }
        }
    )*/

    // runs a single instruction and updates the stack
    let run_instruction = (
            stack: result<stack, string>, 
            instruction: ast_element
        ): result<stack, string> => {
            switch stack {
                | Ok(s) => {
                    switch instruction.instruction {
                        | ADD => {
                            open ADD
                            ADD.run(~stack=s, ~args={ el_pos: 0 })
                        }
                        | NIL => {
                            open NIL
                            NIL.run(~stack=s, ~args={ el_pos: 0 })
                        }
                        | PAIR => {
                            open PAIR
                            PAIR.run(~stack=s, ~args={ el_pos: 0 })
                        }
                        | PUSH => {
                            open PUSH
                            PUSH.run(~stack=s, ~args={ el_pos: 0 })
                        }
                        | UNPAIR => {
                            open UNPAIR
                            UNPAIR.run(~stack=s, ~args={ el_pos: 0 })
                        }
                    }
                }
                | Error(err) => Error(err)
            }
        }

    // runs the provided Michelson code
    let run_code = (~ast: ast, ~stack: stack): result<m_value, string> => {            
            // stack must have only 1 element
            if Js.Array.length(stack) !== 1 {
                Error(Error_msg.unexpected_stack_depth(1, Js.Array.length(stack)))
            } else {
                // element on the stack must be a pair
                switch stack[0].value {
                    | Pair(_) => {
                        // runs the code from the AST
                        let new_stack: result<stack, string> = ast->Js.Array2.reduce(run_instruction, Ok(stack))
                        switch new_stack {
                            | Ok(stack) => Ok(stack[0].value)
                            | Error(err) => Error(err)
                        }
                    }
                    | _ => Error(Error_msg.wrong_type(~instr=?None, ~expected="pair", ~received=m_type_to_string(stack[0].el_type)))
                } 
            }
        }
}