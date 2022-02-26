open MTypes
open MTypes_js
open Stack
open TaquitoHelpers

type rec ast_element = {
    instruction: Instruction.instruction,
    branches: array<ast_element>,
    params: array<string>
}

type ast = array<ast_element>

type run_output = {
    result: result<stack, string>,
    stack_snapshots: array<stack>,
    comments: array<string>
}

type checked_output = {
    result: (m_value, m_value_js),
    stack: stack,
    stack_snapshots: array<stack>,
    comments: array<string>
}

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

    // runs every instruction from Michelson JSON
    let rec run_instruction_json = 
        (previous_result: result<(stack, int), string>, json_instruction: Js.Json.t, index: int): result<(stack, int), string> => {
        
        // UTILS FUNCTIONS
        // finds numeric argument for instruction
        let find_numeric_arg = (obj: Js_dict.t<'a>, instruction: string): result<option<int>, string> => {
            switch obj->Js.Dict.get("args") {
                | None => Ok(None)
                | Some(arg) => 
                    // arg should be [ { int: number } ]
                    switch arg->Js.Json.decodeArray {
                        | None => Error(`Expected an array as an argument for "${instruction}"`)
                        | Some(arg_arr) => 
                            switch arg_arr[0]->Js.Json.decodeObject {
                                | None => Error(`Expected an object as first element of "${instruction}" arguments`)
                                | Some(arg_obj) => 
                                    switch arg_obj->Js.Dict.get("int") {
                                        | None =>
                                            switch arg_obj->Js.Json.stringifyAny {
                                                | None => Error(`Expected an object with an "int" property as argument of "${instruction}"`)
                                                | Some(res) => Error(`Expected an object with an "int" property as argument of "${instruction}" but got ${res}`)
                                            }                                             
                                        | Some(int_as_string) =>
                                            switch int_as_string->Js.Json.decodeString {
                                                | None => Error(`Expected a string as the value of the "int" property for "${instruction}"`)
                                                | Some(int) =>
                                                    switch int->Belt.Int.fromString {
                                                        | None => Error(`Expected a numeric value under the "int" value as argument of "${instruction}"`)
                                                        | Some(int) => Ok(Some(int))
                                                    }
                                            }
                                    }
                            }
                    }
            }
        }
        // finds type associated to element created by the instruction (like NIL)
        let find_type_arg = (obj: Js_dict.t<'a>, instruction: string): result<string, string> => {
            switch obj->Js.Dict.get("args") {
                | None => Error("Expected a type argument for ${instruction} but got nothing")
                | Some(arg) => 
                    // arg should be [ { prim: type } ]
                    switch arg->Js.Json.decodeArray {
                        | None => Error(`Expected an array as an argument for "${instruction}"`)
                        | Some(arg_arr) => 
                            switch arg_arr[0]->Js.Json.decodeObject {
                                | None => Error(`Expected an object as first element of "${instruction}" arguments`)
                                | Some(arg_obj) => 
                                    switch arg_obj->Js.Dict.get("prim") {
                                        | None => Error(`Expected an object with a "prim" property as argument of "${instruction}"`)
                                        | Some(prim) =>
                                            switch prim->Js.Json.decodeString {
                                                | None => Error(`Expected a string under the "prim" value as argument of "${instruction}"`)
                                                | Some(v) => 
                                                    // validates the type
                                                    if Js.Array2.includes(valid_types, v) {
                                                        Ok(v)
                                                    } else {
                                                        Error(`Unknown type "${v}" for ${instruction} instruction`)
                                                    } 
                                            }
                                    }
                            }
                    }
            }
        }
        // JSON PARSING
        // checks if the previous result didn't fail
        switch previous_result {
            | Ok((stack, _)) => {
                // determines the type of the JSON
                switch json_instruction->Js.Json.classify {
                    | Js.Json.JSONObject(instr_obj) => {
                        // finds instruction
                        switch instr_obj->Js.Dict.get("prim") {
                            | None => Error(`Ill-formatted value, no "prim" property on object`)
                            | Some(prim) => 
                                switch prim->Js.Json.decodeString {
                                    | None => Error(`"prim" value is not a string`)
                                    | Some(prim) => {
                                        // translates the string to instruction
                                        switch prim->Instruction.string_to_variant {
                                            | Error(err) => Error(err)
                                            | Ok(instr) => {
                                                switch instr {
                                                    // runs the found instruction
                                                    | ABS => {
                                                        open ABS
                                                        // runs the instruction
                                                        switch ABS.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | ADD => {
                                                        open ADD                                                            
                                                        // runs the instruction
                                                        switch ADD.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | CAR => {
                                                        open CAR
                                                        // runs the instruction
                                                        switch CAR.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | CDR => {
                                                        open CDR
                                                        // runs the instruction
                                                        switch CDR.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | CONCAT => {
                                                        open CONCAT
                                                        // runs the instruction
                                                        switch CONCAT.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | DIP => {
                                                        // DIP has more Michelson code in the "args" property
                                                        switch instr_obj->Js.Dict.get("args") {
                                                            | None => Error(`Expected "args" property, but got nothing`)
                                                            | Some(args) => 
                                                                switch args->Js.Json.decodeArray {
                                                                    | None => Error(`Expected an array as argument for DIP`)
                                                                    | Some(arg_arr) => {
                                                                        // removes the first element of the stack
                                                                        switch stack->Js.Array2.shift {
                                                                            | None => Error("Unexpected empty stack for DIP instruction")
                                                                            | Some(first_el) => {
                                                                                switch arg_arr->Js.Array2.reducei(run_instruction_json, Ok((stack, 0))) {
                                                                                    | Ok((new_stack, _)) => Ok([first_el]->Js.Array2.concat(new_stack), 0)
                                                                                    | Error(err) => Error(err)
                                                                                }
                                                                            }
                                                                        }
                                                                    }                                                                            
                                                                }
                                                        }
                                                    }
                                                    | DROP => {
                                                        open DROP
                                                        // finds if DROP has arguments
                                                        switch find_numeric_arg(instr_obj, "DROP") {
                                                            | Ok(res) => {
                                                                let pos = switch res {
                                                                    | None => 1
                                                                    | Some(num) => num
                                                                }                                                                                                                                 
                                                                // runs the instruction
                                                                switch DROP.run(
                                                                    ~stack=stack, 
                                                                    ~args={ el_pos: pos }, 
                                                                    ~params=[pos->Belt.Int.toString]
                                                                    ) {
                                                                    | Ok(new_stack) => Ok((new_stack, 0))
                                                                    | Error(err) => Error(err)
                                                                }
                                                            }
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | DUP => {
                                                        open DUP
                                                        // finds if DUP has arguments
                                                        switch find_numeric_arg(instr_obj, "DUP") {
                                                            | Ok(res) => {
                                                                let params = switch res {
                                                                    | None => ["1"]
                                                                    | Some(num) => [num->Belt.Int.toString]
                                                                }                                                                    
                                                                // runs the instruction
                                                                switch DUP.run(~stack=stack, ~args={ el_pos: 0 }, ~params) {
                                                                    | Ok(new_stack) => Ok((new_stack, 0))
                                                                    | Error(err) => Error(err)
                                                                }
                                                            }
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | EDIV => {
                                                        open EDIV
                                                        // runs the instruction
                                                        switch EDIV.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | EQ => {
                                                        open GenericComp
                                                        // runs the instruction
                                                        switch EQ.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | GE => {
                                                        open GenericComp
                                                        // runs the instruction
                                                        switch GE.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | GT => {
                                                        open GenericComp
                                                        // runs the instruction
                                                        switch GT.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | LE => {
                                                        open GenericComp
                                                        // runs the instruction
                                                        switch LE.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | LT => {
                                                        open GenericComp
                                                        // runs the instruction
                                                        switch LT.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | MUL => {
                                                        open MUL
                                                        // runs the instruction
                                                        switch MUL.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | NEQ => {
                                                        open GenericComp
                                                        // runs the instruction
                                                        switch NEQ.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | NIL => {
                                                        open NIL
                                                        switch find_type_arg(instr_obj, "NIL") {
                                                            | Ok(res) => {
                                                                switch NIL.run(
                                                                    ~stack=stack, 
                                                                    ~args={ el_pos: 0 }, 
                                                                    ~params=[res]) {
                                                                    | Ok(new_stack) => Ok((new_stack, 0))
                                                                    | Error(err) => Error(err)
                                                                }
                                                            }
                                                            | Error(err) => Error(err)
                                                        }                                                            
                                                    }
                                                    | PAIR => {
                                                        open PAIR
                                                        // runs the instruction
                                                        switch PAIR.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | PUSH => {
                                                        open PUSH
                                                        // finds the parameters
                                                        switch instr_obj->Js.Dict.get("args") {
                                                            | None => Error(`PUSH instruction should have an "args" property`)
                                                            | Some(args) => {
                                                                // "args" is an array
                                                                switch args->Js.Json.decodeArray {
                                                                    | None => Error(`Expected the value of "args" to be an array`)
                                                                    | Some(arr_args) => {                                                                        
                                                                        // runs the instruction
                                                                        switch PUSH.run(~stack=stack, ~args={ el_pos: 0 }, ~params=arr_args) {
                                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                                            | Error(err) => Error(err)
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                    | SUB => {
                                                        open SUB
                                                        // runs the instruction
                                                        switch SUB.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | SWAP => {
                                                        open SWAP
                                                        // runs the instruction
                                                        switch SWAP.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                    | UNPAIR => {
                                                        open UNPAIR
                                                        // runs the instruction
                                                        switch UNPAIR.run(~stack=stack, ~args={ el_pos: 0 }) {
                                                            | Ok(new_stack) => Ok((new_stack, 0))
                                                            | Error(err) => Error(err)
                                                        }
                                                    }

                                                }
                                            }
                                        }
                                    }
                                }
                        }
                    }
                    | Js.Json.JSONArray(arr) => arr->Js.Array2.reducei(run_instruction_json, Ok((stack, 0)))
                    | _ => Error("Unknown value for instruction in JSON")
                }
            }
            | Error(err) => Error(err)
        }                      
    }

    let run_code_from_json = (~code: string, ~stack: stack): run_output => {
        // parses Michelson code to JSON
        let formatted_code =
            switch code->Js.String2.trim->Js.String2.match_(%re("/\{(.*)\}/")) {
                | None => "{" ++ code ++ "}"
                | Some(c) => c[0]
            }
        let michelson_json = michel_codec_parser()->parse_micheline_expression(formatted_code)
        // verifies the JSON returned by michel-codec
        let michelson_json = try Js.Json.parseExn(michelson_json->Js.Json.stringify) catch {
        | _ => failwith("Error parsing Michelson JSON returned by @taquito/michel-codec")
        }
        // JSON structure must be an array
        switch michelson_json->Js.Json.classify {
            | Js.Json.JSONArray(arr_of_instructions) => {
                // maps the array of instructions
                if arr_of_instructions->Js.Array2.length < 2 {
                    {
                        result: Error("Array of instructions seems to be too small (less than 2 instructions)"),
                        stack_snapshots: [],
                        comments: []
                    }
                } else {
                    switch arr_of_instructions->Js.Array2.reducei(run_instruction_json, Ok((stack, 0))) {
                        | Ok((stack, _)) => {
                                                result: Ok(stack),
                                                stack_snapshots: [],
                                                comments: []
                                            }
                        | Error(err) => {
                                            result: Error(err),
                                            stack_snapshots: [],
                                            comments: []
                                        }
                    }                            
                }
            }
            | _ => {
                        result: Error("JSON value for instructions tree is not an array"),
                        stack_snapshots: [],
                        comments: []
                    }
        }
    }

    let verify_output_stack = (output_to_check: run_output, storage_type: m_type): result<checked_output, string> => {
        let { result, stack_snapshots, comments } = output_to_check
        switch result {
            | Ok(stack) => {
                let res = stack[0].value
                // element on the stack must be a pair
                // with a list of operation on the left
                // and the new storage on the right
                switch res {
                    | Pair({ el_type }) => {
                        switch el_type {
                            | (List(Operation), storage) => 
                                    if storage === storage_type {
                                        {
                                            result: (res, m_value_to_js(res)),
                                            stack,
                                            stack_snapshots,
                                            comments: ["Expected storage type and new storage type match"]
                                        }->Ok
                                    } else {
                                        {
                                            result: (res, m_value_to_js(res)),
                                            stack,
                                            stack_snapshots,
                                            comments: [`Storage type in result doesn't match, expected ${storage_type->m_type_to_string, got ${storage->m_type_to_string}`]
                                        }->Ok
                                    }
                            | _ => {
                                        result: (res, m_value_to_js(res)),
                                        stack,
                                        stack_snapshots,
                                        comments: ["Unexpected final values"]
                                    }->Ok
                        }                                        
                    }
                    | _ => {
                                result: (res, m_value_to_js(res)),
                                stack,
                                stack_snapshots,
                                comments: [`Final value is not a pair, got ${m_type_to_string(stack[0].el_type)}`]
                            }->Ok
                }
            }
            | Error(err) => Error(err)
        }        
    }
}