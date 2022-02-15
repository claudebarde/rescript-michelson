open MTypes
open ErrorMsg

type stack_element = {
    origin_instruction: string,
    el_type: m_type,
    value: m_value
}

type stack = array<stack_element>

module Stack = {
    let previous_stack: ref<array<stack_element>> = ref([])
    let current_stack: ref<array<stack_element>> = ref([])

    // returns current stack
    let get_current_stack = () => current_stack
    // updates previous stack
    let update_previous_stack = (stack: stack): unit => previous_stack.contents = stack
    // pushes elements to stack
    let push_els_to_stack = (stack_els: array<stack_element>): unit => 
        current_stack.contents = Js.Array.concat(stack_els, current_stack.contents)
    // creates new stack elements
    /*let create_new_el = (~el_type: m_type, ~from_instr: string, ~value: array<string>): result<stack_element, string> =>
        if Js.Array.length(value) == 0 {
            Error("Empty array of values to create stack element")
        } else {
            switch el_type {
                | Address =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Address,
                        value: Address(value[0])
                    })
                | Bool => {
                        let value = value[0]
                        if value !== "true" && value !== "false" {
                            Error(Error_msg.unexpected_value("true or false", value))
                        } else {
                            Ok({
                                origin_instruction: from_instr,
                                el_type: Bool,
                                value: Bool(value == "true" ? true : false)
                            })
                        }
                    }
                | Bytes =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Bytes,
                        value: Bytes(value[0])
                    })
                | Chain_id =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Chain_id,
                        value: Chain_id(value[0])
                    })
                | Int =>
                    switch Belt.Int.fromString(value[0]) {
                        | None => Error(Error_msg.unexpected_value("numeric value", value[0]))
                        | Some(num) => Ok({
                                origin_instruction: from_instr,
                                el_type: Int,
                                value: Int(num)
                            })
                    }                    
                | Key =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Key,
                        value: Key(value[0])
                    })
                | Key_hash =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Key_hash,
                        value: Key_hash(value[0])
                    })
                | Mutez =>
                    switch Belt.Int.fromString(value[0]) {
                        | None => Error(Error_msg.unexpected_value("numeric value", value[0]))
                        | Some(num) => 
                            if MTypes_verify.valid_mutez(num) {
                                Ok({
                                    origin_instruction: from_instr,
                                    el_type: Mutez,
                                    value: Mutez(num)
                                })
                            } else {
                                Error("Mutez value cannot be negative")
                            }
                    }                    
                | Nat =>
                    switch Belt.Int.fromString(value[0]) {
                        | None => Error(Error_msg.unexpected_value("numeric value", value[0]))
                        | Some(num) => 
                            if MTypes_verify.valid_nat(num) {
                                Ok({
                                    origin_instruction: from_instr,
                                    el_type: Nat,
                                    value: Nat(num)
                                })
                            } else {
                                Error("Mutez value cannot be negative")
                            }
                    } 
                | Never =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Never,
                        value: Never
                    })
                | Signature =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Signature,
                        value: Signature(value[0])
                    })
                | String =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: String,
                        value: String(value[0])
                    })
                | Timestamp =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Timestamp,
                        value: Timestamp(value[0])
                    })
                | Unit =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Unit,
                        value: Unit
                    })
                | Operation =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Operation,
                        value: Operation(value[0])
                    })
                | List(_) =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: List(String),
                        value: List(list{})
                    })
                | Option(_) =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Option(String),
                        value: Option(None)
                    })
                | Set(_) =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Set(String),
                        value: Set([])
                    })
                | Or(_) =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Or((String, String)),
                        value: Or(Left(String(value[0])))
                    })
                | Pair(_) =>
                    if Js.Array.length(value) === 2 {
                        Ok({
                            origin_instruction: from_instr,
                            el_type: Pair((String, String)),
                            value: Pair((String(value[0]), String(value[1])))
                        })
                    } else {
                        Error("Value to create a stack element of type `pair` must be an array of 2 elements")
                    }
                | Map(_) =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Map(String, String),
                        value: Map({
                            key_type: String,
                            value_type: String,
                            elements: [],
                            size: () => 0
                        })
                    })
                | Big_map(_) =>
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Big_map((String, String)),
                        value: Big_map({
                            key_type: String,
                            value_type: String,
                            elements: [],
                        })
                    })
            }
        }*/
    let create_new_el = (~el_value: m_value, ~from_instr: string): result<stack_element, string> => {
        switch el_value {
            | Address(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Address,
                    value: Address(val)
                })
            | Bool(val) => {
                    if val !== true && val !== false {
                        Error(Error_msg.unexpected_value("true or false", val ? "true" : "false"))
                    } else {
                        Ok({
                            origin_instruction: from_instr,
                            el_type: Bool,
                            value: Bool(val)
                        })
                    }
                }
            | Bytes(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Bytes,
                    value: Bytes(val)
                })
            | Chain_id(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Chain_id,
                    value: Chain_id(val)
                })
            | Int(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Int,
                    value: Int(val)
                })                   
            | Key(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Key,
                    value: Key(val)
                })
            | Key_hash(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Key_hash,
                    value: Key_hash(val)
                })
            | Mutez(val) =>
                if MTypes_verify.valid_mutez(val) {
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Mutez,
                        value: Mutez(val)
                    })
                } else {
                    Error("Mutez value cannot be negative")
                }                    
            | Nat(val) =>
                if MTypes_verify.valid_nat(val) {
                    Ok({
                        origin_instruction: from_instr,
                        el_type: Nat,
                        value: Nat(val)
                    })
                } else {
                    Error("Mutez value cannot be negative")
                } 
            | Never =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Never,
                    value: Never
                })
            | Signature(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Signature,
                    value: Signature(val)
                })
            | String(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: String,
                    value: String(val)
                })
            | Timestamp(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Timestamp,
                    value: Timestamp(val)
                })
            | Unit =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Unit,
                    value: Unit
                })
            | Operation(val) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Operation,
                    value: Operation(val)
                })
            | List(_) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: List(String),
                    value: List({ value: list{}, el_type: String})
                })
            | Option(_) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Option(String),
                    value: Option({ value: None, el_type: String })
                })
            | Set(_) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Set(String),
                    value: Set({ value: [], el_type: String })
                })
            | Or(_) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Or((String, String)),
                    value: Or({ value: Left(String("test")), el_type: (String, String) })
                })
            | Pair({ value, el_type }) =>
                let (left_type, right_type) = el_type
                Ok({
                    origin_instruction: from_instr,
                    el_type: Pair((left_type, right_type)),
                    value: Pair({ value, el_type })
                })
            | Map(_) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Map(String, String),
                    value: Map({
                        value: {
                            key_type: String,
                            value_type: String,
                            elements: [],
                            size: () => 0
                        },
                        el_type: (String, String)
                    })
                })
            | Big_map(_) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Big_map((String, String)),
                    value: Big_map({
                        value: {
                            key_type: String,
                            value_type: String,
                            elements: [],
                        },
                        el_type: (String, String)
                    })
                })
        }
        }
}