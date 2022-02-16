open MTypes
open ErrorMsg

type stack_element = {
    origin_instruction: string,
    el_type: m_type,
    value: m_value
}
type snapshot_stack_element = {
    origin_instruction: string,
    el_type_js: string,
    value_js: string
}

type stack = array<stack_element>
type stack_snapshots = array<snapshot_stack_element>

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
            | List({ value, el_type }) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: List(String),
                    value: List({ value, el_type })
                })
            | Option({ value, el_type }) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Option(String),
                    value: Option({ value, el_type })
                })
            | Set({ value, el_type }) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Set(String),
                    value: Set({ value, el_type })
                })
            | Or({ value, el_type }) =>
                Ok({
                    origin_instruction: from_instr,
                    el_type: Or((String, String)),
                    value: Or({ value, el_type })
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