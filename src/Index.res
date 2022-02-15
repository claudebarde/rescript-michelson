open Remich

/*let michelson_contract = "UNPAIR ;
    IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
    NIL operation ;
    PAIR";*/

let michelson_contract = "UNPAIR ; ADD ; NIL operation ; PAIR"

// parses the Michelson code
let parsed_code = Remich.parse_to_ast(michelson_contract)
Js.log(parsed_code)
// creates the initial stack
//let initial_stack_result = Remich.init_stack(~params_type=Int, ~storage_type=Int, ~initial_stack_value=["3", "4"])
let initial_stack_result = Remich.init_stack((Int(3), Int(4)))
let initial_stack = 
    switch initial_stack_result {
        | Ok(v) => {
            //Js.log2(`Element type:`, v[0].el_type)
            //Js.log2(`Element value:`, v[0].value)
            v
        }
        | Error(err) => {
            Js.log(err)
            []
        }
    }
// runs the Michelson code
let result = Remich.run_code(~ast=parsed_code, ~stack=initial_stack)
switch result {
    | Ok(res) => Js.log2("Result -> OK:", res)
    | Error(err) => Js.log2("Result -> Error:", err)
}