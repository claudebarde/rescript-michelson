open MTypes

type rec m_type_js =[
    | #address
    | #bool
    | #bytes
    | #chain_id
    | #int
    | #key
    | #key_hash
    | #mutez
    | #nat
    | #never
    | #signature
    | #string
    | #timestamp
    | #unit
    | #operation
    | #list(m_type_js)
    | #option(m_type_js)
    | #set(m_type_js)
    | #or((m_type_js, m_type_js))
    | #pair((m_type_js, m_type_js))
    | #map((m_type_js, m_type_js))
    | #big_map((m_type_js, m_type_js))
]

type rec m_value_js =[
    | #address(m_address)
    | #bool(m_bool)
    | #bytes(m_bytes)
    | #chain_id(m_chain_id)
    | #int(m_int)
    | #key(m_key)
    | #key_hash(m_key_hash)
    | #mutez(m_mutez)
    | #nat(m_nat)
    | #never
    | #signature(m_signature)
    | #string(m_string)
    | #timestamp(m_timestamp)
    | #unit
    | #operation(m_operation)
    | #list((m_list<m_value_js>, m_type_js))
    | #option((m_option<m_value_js>, m_type_js))
    | #set((m_set<m_value_js>, m_type_js))
    | #or((m_or<m_value_js, m_value_js>, m_type_js))
    | #pair((m_pair<m_value_js, m_value_js>, m_type_js))
    | #map((m_map<m_type_js, m_type_js>, m_type_js))
    | #big_map((m_big_map<m_type_js, m_type_js>, m_type_js))
]
/*and list_arg = { value: m_list<m_value_js>, el_type: m_type_js}
and option_arg = { value: m_option<m_value_js>, el_type: m_type_js }
and set_arg = { value: m_set<m_value_js>, el_type: m_type_js }
and or_arg = { value: m_or<m_value_js, m_value_js>, el_type: (m_type_js, m_type_js) }
and pair_arg = { value: m_pair<m_value_js, m_value_js>, el_type: (m_type_js, m_type_js) }
and map_arg = { value: m_map<m_type_js, m_type_js>, el_type: (m_type_js, m_type_js) }
and big_map_arg = { value: m_big_map<m_type_js, m_type_js>, el_type: (m_type_js, m_type_js) }*/

let rec m_type_to_js = (el: m_type): m_type_js => 
    switch el {
        | Address => #address
        | Bool => #bool
        | Bytes => #bytes
        | Chain_id => #chain_id
        | Int => #int
        | Key => #key
        | Key_hash => #key_hash
        | Mutez => #mutez
        | Nat => #nat
        | Never => #never
        | Signature => #signature
        | String => #string
        | Timestamp => #timestamp
        | Unit => #unit
        | Operation => #operation
        | List(t) => #list(m_type_to_js(t))
        | Option(t) => #option(m_type_to_js(t))
        | Set(t) => #set(m_type_to_js(t))
        | Or((a, b)) => #or((m_type_to_js(a), m_type_to_js(b)))
        | Pair((a, b)) => #pair((m_type_to_js(a), m_type_to_js(b)))
        | Map((a, b)) => #map((m_type_to_js(a), m_type_to_js(b)))
        | Big_map((a, b)) => #big_map((m_type_to_js(a), m_type_to_js(b)))
    }

let rec m_value_to_js = (el: m_value): m_value_js => 
    switch el {
        | Address(val) => #address(val)
        | Bool(val) => #bool(val)
        | Bytes(val) => #bytes(val)
        | Chain_id(val) => #chain_id(val)
        | Int(val) => #int(val)
        | Key(val) => #key(val)
        | Key_hash(val) => #key_hash(val)
        | Mutez(val) => #mutez(val)
        | Nat(val) => #nat(val)
        | Never => #never
        | Signature(val) => #signature(val)
        | String(val) => #string(val)
        | Timestamp(val) => #timestamp(val)
        | Unit => #unit
        | Operation(val) => #operation(val)
        | List({ value, el_type }) => #list((value->Belt.List.map(el => m_value_to_js(el)), m_type_to_js(el_type)))
        | Option({ value, el_type }) => 
            #option(( switch value { | None => None | Some(v) => Some(m_value_to_js(v)) }, m_type_to_js(el_type)))
        | Set({ value, el_type }) => 
            #set((value->Js.Array2.map(el => m_value_to_js(el)), m_type_to_js(el_type)))
        | Or({ value, el_type: (left, right) }) => 
            #or(( 
                    switch value { | Left(v) => Left(m_value_to_js(v)) | Right(v) => Right(m_value_to_js(v)) }, 
                    #or((m_type_to_js(left), m_type_to_js(right)))
                ))
        | Pair({ value: (left_val, right_val), el_type: (left_type, right_type) }) => 
            #pair(( 
                    (m_value_to_js(left_val), m_value_to_js(right_val)), 
                    #pair((m_type_to_js(left_type), m_type_to_js(right_type)))
                ))
        // TODO
        | Map(_) => #never
        | Big_map(_) => #never
    }