let valid_types = 
    ["address", "bool", "bytes", "chain_id", "int", "key", "key_hash", "mutez", "nat", "never", "signature", 
    "string", "timestamp", "unit", "operation", "list", "option", "or", "pair", "set", "map", "big_map"]

type m_address = string
type m_bool = bool
type m_bytes = string
type m_chain_id = string
type m_int = int
type m_key = string
type m_key_hash = string
type m_mutez = int
type m_nat = int
type m_never = unit
type m_signature = string
type m_string = string
type m_timestamp = int
type m_unit = unit
type m_operation = string

type m_list<'t> = list<'t>
type m_option<'t> = option<'t>
type m_or<'a, 'b> = | Left('a) | Right('b)
type m_pair<'a, 'b> = ('a, 'b)
type m_set<'t> = array<'t>

type m_map<'a, 'b> = {
    key_type: 'a,
    value_type: 'b,
    elements: array<('a, 'b)>,
    size: () => int
}
type m_big_map<'a, 'b> = {
    key_type: 'a,
    value_type: 'b,
    elements: array<('a, 'b)>
}

type rec m_type =
    | Address
    | Bool
    | Bytes
    | Chain_id
    | Int
    | Key
    | Key_hash
    | Mutez
    | Nat
    | Never
    | Signature
    | String
    | Timestamp
    | Unit
    | Operation
    | List(m_type)
    | Option(m_type)
    | Set(m_type)
    | Or((m_type, m_type))
    | Pair((m_type, m_type))
    | Map((m_type, m_type))
    | Big_map((m_type, m_type))

type rec m_value =
    | Address(m_address)
    | Bool(m_bool)
    | Bytes(m_bytes)
    | Chain_id(m_chain_id)
    | Int(m_int)
    | Key(m_key)
    | Key_hash(m_key_hash)
    | Mutez(m_mutez)
    | Nat(m_nat)
    | Never
    | Signature(m_signature)
    | String(m_string)
    | Timestamp(m_timestamp)
    | Unit
    | Operation(m_operation)
    | List({ value: m_list<m_value>, el_type: m_type}) // list + element type
    | Option({ value: m_option<m_value>, el_type: m_type })
    | Set({ value: m_set<m_value>, el_type: m_type })
    | Or({ value: m_or<m_value, m_value>, el_type: (m_type, m_type) })
    | Pair({ value: m_pair<m_value, m_value>, el_type: (m_type, m_type) })
    | Map({ value: m_map<m_type, m_type>, el_type: (m_type, m_type) })
    | Big_map({ value: m_big_map<m_type, m_type>, el_type: (m_type, m_type) })

module MTypes_verify = {
    // verifies if a mutez value is valid
    let valid_mutez = (val: int): bool => val >= 0
    // verifies if a nat value is valid
    let valid_nat = (val: int): bool => val >= 0
}

let m_type_to_string = (el: m_type): string => 
    switch el {
        | Address => "address"
        | Bool => "bool"
        | Bytes => "bytes"
        | Chain_id => "chain_id"
        | Int => "int"
        | Key => "key"
        | Key_hash => "key_hash"
        | Mutez => "mutez"
        | Nat => "nat"
        | Never => "never"
        | Signature => "signature"
        | String => "string"
        | Timestamp => "timestamp"
        | Unit => "unit"
        | Operation => "operation"
        | List(_) => "list"
        | Option(_) => "option"
        | Set(_) => "set"
        | Or(_) => "or"
        | Pair(_) => "pair"
        | Map(_) => "map"
        | Big_map(_) => "big_map"
    }

/*
    string = list{("string", list{"string"})}
    list operation = list{("list", list{"operation"})}
    list pair nat nat = list{("list", list{("pair", list{"nat, nat"})})}
    or nat string = list{{base: "or", params: list{{base: nat}, {base: string}}}}
*/
type rec string_to_m_type_param = {
    base: string,
    params: list<string_to_m_type_param>
}
let rec string_to_m_type = (els: list<string_to_m_type_param>): result<m_type, string> =>
    switch els {
        | list{} => Error("No parameters passed to function 'string_to_m_type'")
        | list{ head } =>
            switch head.base {
                | "address" => Ok(Address)
                | "bool" => Ok(Bool)
                | "bytes" => Ok(Bytes)
                | "chain_id" => Ok(Chain_id)
                | "int" => Ok(Int)
                | "key" => Ok(Key)
                | "key_hash" => Ok(Key_hash)
                | "mutez" => Ok(Mutez)
                | "nat" => Ok(Nat)
                | "never" => Ok(Never)
                | "signature" => Ok(Signature)
                | "string" => Ok(String)
                | "timestamp" => Ok(Timestamp)
                | "unit" => Ok(Unit)
                | "operation" => Ok(Operation)
                | "list" => 
                    switch string_to_m_type(head.params) {
                        | Ok(val) => Ok(List(val))
                        | Error(err) => Error(err)
                    }
                | "option" => {
                    switch string_to_m_type(head.params) {
                        | Ok(val) => Ok(Option(val))
                        | Error(err) => Error(err)
                    }
                }
                | "set" => {
                    switch string_to_m_type(head.params) {
                        | Ok(val) => Ok(Set(val))
                        | Error(err) => Error(err)
                    }
                }
                | "or" | "pair" | "map" | "big_map" => {
                    switch head.params {
                        | list{} => Error(`Missing parameters for '${head.base}' in function 'string_to_m_type'`)
                        | list{_} => Error(`Missing second parameter for '${head.base}' in function 'string_to_m_type'`)
                        | list{left, right} => 
                            switch (string_to_m_type(list{left}), string_to_m_type(list{right})) {
                                | (Ok(left_val), Ok(right_val)) => 
                                    if head.base === "or" {
                                        Ok(Or((left_val, right_val)))
                                    } else if head.base === "pair" {
                                        Ok(Pair((left_val, right_val)))
                                    } else if head.base === "map" {
                                        Ok(Map((left_val, right_val)))
                                    } else if head.base === "big_map" {
                                        Ok(Big_map((left_val, right_val)))
                                    } else {
                                        Error("Unexpected error in function 'string_to_m_type'")
                                    }
                                | (Ok(_), Error(err)) => Error(err)
                                | (Error(err), Ok(_)) => Error(err)
                                | (Error(err1), Error(err2)) => Error(`${err1} / ${err2}`)
                            }
                        | _ => Error("Unexpected number of parameters for '${head.base}' in function 'string_to_m_type'")
                    }
                }
                | _ => Error(`Unrecognized type for string "${head.base}" in function 'string_to_m_type'`)
            }
        | _ => Error("test")
    }

let m_value_to_type = (el: m_value): m_type => 
    switch el {
        | Address(_) => Address
        | Bool(_) => Bool
        | Bytes(_) => Bytes
        | Chain_id(_) => Chain_id
        | Int(_) => Int
        | Key(_) => Key
        | Key_hash(_) => Key_hash
        | Mutez(_) => Mutez
        | Nat(_) => Nat
        | Never => Never
        | Signature(_) => Signature
        | String(_) => String
        | Timestamp(_) => Timestamp
        | Unit => Unit
        | Operation(_) => Operation
        | List({ el_type }) => List(el_type)
        | Option({ el_type }) => Option(el_type)
        | Set({ el_type }) => Set(el_type)
        | Or({ el_type: (left, right) }) => Or((left, right))
        | Pair({ el_type: (left, right) }) => Pair((left, right))
        // TODO
        | Map(_) => Map((String, String))
        | Big_map(_) => Big_map((String, String))
    }

module Pair = {
    // useful methods for pairs
    
    // unwraps values in a pair
    let unpair = (el: m_value): result<(m_value, m_value), string> => {
        switch el {
            | Pair({ value: (left_el, right_el) }) => Ok((left_el, right_el))
            | _ => Error("Element to unpair must be a pair")
        }
    }
}