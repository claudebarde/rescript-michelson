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

let rec parse_json_to_type = (json_obj: Js.Json.t): result<m_type, string> => {
    // the value must be an object
    switch json_obj->Js.Json.decodeObject {
        | None => Error("The provided value is not a valid object")
        | Some(obj) => {
            // the value must have a "prim" property
            switch obj->Js.Dict.get("prim") {
                | None => Error(`The provided value doesn't have a "prim" property`)
                | Some(prim) => {
                    switch prim->Js.Json.decodeString {
                        | None => Error(`The value of the "prim" property is expected to be a string`)
                        | Some(prim_val) => {
                            switch prim_val {
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
                                | "list" | "option" | "set" => {
                                    // gets the value in "args"
                                    switch obj->Js.Dict.get("args") {
                                        | None => Error(`Missing "args" property for "${prim_val}" type`)
                                        | Some(args) => {
                                            // args must be an array
                                            switch args->Js.Json.decodeArray {
                                                | None => Error(`"args" property for "${prim_val}" type must be an array`)
                                                | Some(args_arr) => {
                                                    // there must be 1 object in the array
                                                    if args_arr->Js.Array2.length !== 1 {
                                                        Error(`"args" array for "${prim_val}" type must have exactly 1 element`)
                                                    } else {
                                                        // decodes the value in the array
                                                        switch args_arr[0]->parse_json_to_type {
                                                            | Ok(prim_type) => 
                                                                if prim_val === "option" {
                                                                    Ok(Option(prim_type))
                                                                } else if prim_val === "list" {
                                                                    Ok(List(prim_type))
                                                                } else if prim_val === "set" {
                                                                    Ok(Set(prim_type))
                                                                } else {
                                                                    Error(`It should be "option", "list" or "set" but it's not!`)
                                                                }
                                                            | Error(err) => Error(err)
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                | "or" | "pair" => {
                                    // gets the value in "args"
                                    switch obj->Js.Dict.get("args") {
                                        | None => Error(`Missing "args" property for "${prim_val}" type`)
                                        | Some(args) => {
                                            // args must be an array
                                            switch args->Js.Json.decodeArray {
                                                | None => Error(`"args" property for "${prim_val}" type must be an array`)
                                                | Some(args_arr) => {
                                                    // there must be 2 objects in the array
                                                    if args_arr->Js.Array2.length !== 2 {
                                                        Error(`"args" array for "${prim_val}" type must have exactly 2 elements`)
                                                    } else {
                                                        // decodes the values in the array
                                                        switch (args_arr[0]->parse_json_to_type, args_arr[1]->parse_json_to_type) {
                                                            | (Ok(left_type), Ok(right_type)) => {
                                                                if prim_val === "pair" {
                                                                    Ok(Pair((left_type, right_type)))
                                                                } else if prim_val === "or" {
                                                                    Ok(Or((left_type, right_type)))
                                                                } else {
                                                                    Error(`It should be "pair" or "or" but it's not!`)
                                                                }
                                                            }
                                                            | (Error(err), Ok(_)) => Error(err)
                                                            | (Ok(_), Error(err)) => Error(err)
                                                            | (Error(err1), Error(err2)) => Error(j`$err1 & $err2`)
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                | "map" => Ok(Unit)
                                | "big_map" => Ok(Unit)
                                | _ => Error(`Unrecognized type for string "${prim_val}"`)
                            }
                        }
                    }                    
                }
            }
        }
    }
}

let rec parse_json_to_value = (json_obj: Js.Json.t, value_type: m_type): result<m_value, string> => {
    switch json_obj->Js.Json.classify {
        | JSONObject(obj) => {
            // the value is an object
            // parses the value according to its type
            switch value_type {
                | Address | Bytes | Chain_id | Key | Key_hash | Operation | Never | Signature | String | Unit => {
                    switch obj->Js.Dict.get("string") {
                        | None => Error(`The provided value was expected to have a "string" property`)
                        | Some(val) => {
                            switch val->Js.Json.decodeString {
                                | None => Error(`The provided value under the "string" property must be a string`)
                                | Some(val) => {
                                    switch value_type {
                                        | Address => Ok(Address(val))
                                        | Bytes => Ok(Bytes(val))
                                        | Chain_id => Ok(Chain_id(val))
                                        | Key => Ok(Key(val))
                                        | Key_hash => Ok(Key_hash(val))
                                        | Never => 
                                            if val === "Never" {
                                                Ok(Never)
                                            } else {
                                                Error(`Unexpected value for "never" type`)
                                            }
                                        | Operation => Ok(Operation(val))
                                        | Signature => Ok(Signature(val))
                                        | String => Ok(String(val))
                                        | Unit => 
                                            if val === "Unit" {
                                                Ok(Unit)
                                            } else {
                                                Error(`Unexpected value for "unit" type`)
                                            }
                                        | _ => Error("Error while parsing JSON value")
                                    }
                                }
                            }
                        }
                    }
                }
                | Bool => {
                    switch obj->Js.Dict.get("prim") {
                        | None => Error(`The provided value was expected to have a "prim" property`)
                        | Some(val) =>
                            switch val->Js.Json.decodeString {
                                | None => Error(`The provided value under the "prim" property must be a string`)
                                | Some(val) =>
                                    if val === "True" {
                                        Ok(Bool(true))
                                    } else if val === "False" {
                                        Ok(Bool(false))
                                    } else {
                                        Error(`Unexpected value for boolean type, must be "True" or "False"`)
                                    }
                            }
                    }
                }
                | Int | Mutez | Nat | Timestamp => {
                    switch obj->Js.Dict.get("int") {
                        | None => Error(`The provided value was expected to have a "int" property`)
                        | Some(val) => 
                            switch val->Js.Json.decodeString {
                                | None => Error(`The provided value under the "int" property must be a string`)
                                | Some(val) => {
                                    switch val->Belt.Int.fromString {
                                        | None => Error(`A numeric value was expected for the "int" property`)
                                        | Some(num) =>
                                            switch value_type {
                                                | Int => Ok(Int(num))
                                                | Mutez => {
                                                    if MTypes_verify.valid_mutez(num) {
                                                        Ok(Mutez(num))
                                                    } else {
                                                        Error(`Invalid value provided for mutez type in JSON object`)
                                                    }
                                                }
                                                | Nat => {
                                                    if MTypes_verify.valid_nat(num) {
                                                        Ok(Nat(num))
                                                    } else {
                                                        Error(`Invalid value provided for nat type in JSON object`)
                                                    }
                                                }
                                                | Timestamp => Ok(Timestamp(num))
                                                | _ => Error("Error while parsing JSON value")
                                            }
                                    }
                                }
                            }
                    }
                }
                | Pair((left_type, right_type)) => {
                    // must have a "prim" property equal to "Pair"
                    switch obj->Js.Dict.get("prim") {
                        | None => Error(`The provided value for pair was expected to have a "prim" property`)
                        | Some(val) =>
                            // val must be "Pair"
                            switch val->Js.Json.decodeString {
                                | None => Error(`Value of "prim" property must be a string`)
                                | Some(val) =>
                                    if val !== "Pair" {
                                        Error(`Value of "prim" property must be equal to "Pair"`)
                                    } else {
                                        // finds args property
                                        switch obj->Js.Dict.get("args") {
                                            | None => Error(`The provided value for pair was expected to have an "args" property`)
                                            | Some(args) =>
                                                // args must be an array
                                                switch args->Js.Json.decodeArray {
                                                    | None => Error("The provided arguments for a pair value must be in an array")
                                                    | Some(arr_args) => {
                                                        // the array must be of length 2
                                                        if arr_args->Js.Array2.length !== 2 {
                                                            Error(`There must be only 2 arguments for a value of type "pair"`)
                                                        } else {
                                                            // TODO: checks that the values match the type
                                                            switch (
                                                                parse_json_to_value(arr_args[0], left_type), 
                                                                parse_json_to_value(arr_args[1], right_type)
                                                            ) {
                                                                | (Ok(left_value), Ok(right_value)) => {
                                                                    Ok(Pair({ 
                                                                                value: (left_value, right_value), 
                                                                                el_type: (left_type, right_type)
                                                                            }))
                                                                }
                                                                | (Error(err), Ok(_)) => Error(err)
                                                                | (Ok(_), Error(err)) => Error(err)
                                                                | (Error(err1), Error(err2)) => Error(j`$err1 & $err2`)
                                                            }
                                                        }
                                                    }
                                                }
                                        }
                                    }
                            }
                    }
                }
                | _ => Error("Unexpected value type for the shape of the value")
            }
        }
        | JSONArray(arr) => {
            // the value is an array
            // parses the array
            Error("test")
        }
        | _ => Error("Unexpected value in JSON, expected an array or an object")
    }    
}

module MPair = {
    // useful methods for pairs
    
    // unwraps values in a pair
    let unpair = (el: m_value): result<(m_value, m_value), string> => {
        switch el {
            | Pair({ value: (left_el, right_el) }) => Ok((left_el, right_el))
            | _ => Error("Element to unpair must be a pair")
        }
    }
}

module MList = {
    // useful methods for list

    // checks that all the values in a list are all the same m_type
    let rec check_list_values = (list: m_list<'a>, expected_type: m_type): result<(), ()> => {
        switch list {
            | list{} => Ok()
            | list{_} => Ok()
            | list{hd, ...tl} => 
                if m_value_to_type(hd) != expected_type {
                    Error()
                } else {
                    check_list_values(tl, expected_type)
                }
        }
    }
}