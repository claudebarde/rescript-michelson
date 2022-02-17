open Remich

/*let michelson_contract = "UNPAIR ;
    IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
    NIL operation ;
    PAIR";*/

type michelson_test = {
    contract: string,
    initial_storage: MTypes.m_value,
    parameter: MTypes.m_value,
    storage_type: MTypes.m_type,
    expected_new_storage: MTypes.m_value
}
/*
    ADD test (+)

    let michelson_test = {
        contract: "UNPAIR ; ADD ; NIL operation ; PAIR",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_new_storage: Int(8)
    }
*/
/*
    SUB test

    let michelson_test = {
        contract: "UNPAIR ; SUB ; NIL operation ; PAIR",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_new_storage: Int(-2)
    }
*/
let michelson_test = {
    contract: "UNPAIR ; SUB ; ABS ; NIL operation ; PAIR",
    initial_storage: Nat(5),
    parameter: Int(3),
    storage_type: Int,
    expected_new_storage: Int(2)
}

// parses the Michelson code
let parsed_code = Remich.parse_to_ast(michelson_test.contract)
Js.log("\nParsed code:")
Js.log(parsed_code)
// creates the initial stack
//let initial_stack_result = Remich.init_stack(~params_type=Int, ~storage_type=Int, ~initial_stack_value=["3", "4"])
let initial_stack_result = Remich.init_stack((michelson_test.parameter, michelson_test.initial_storage))
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
let (result, stack_snapshots) = Remich.run_code(~ast=parsed_code, ~stack=initial_stack, ~storage_type=michelson_test.storage_type)
switch result {
    | Ok(res) => {
        let (rescript_res, js_res) = res
        Js.log("\nMichelson code successfully processed!")
        Js.log2("ReScript result:", rescript_res)
        Js.log2("JavaScript result:", 
            switch Js.Json.stringifyAny(js_res) {
                | Some(v) => v
                | None => "Couldn't format value into JSON"
            }
        )
        Js.log2("Stack snapshots:", stack_snapshots)
    }
    | Error(err) => Js.log2("\nResult -> Error:", err)
}