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
/*
    ABS test
    let michelson_test = {
        contract: "UNPAIR ; SUB ; ABS ; NIL operation ; PAIR",
        initial_storage: Nat(5),
        parameter: Int(3),
        storage_type: Nat,
        expected_new_storage: Nat(2)
    }
*/
/*
    DROP test

    let michelson_test = {
        contract: "UNPAIR ; DROP ; NIL operation ; PAIR",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_new_storage: Nat(5)
    }

    let michelson_test = {
        contract: "UNPAIR ; DROP 2 ; NIL operation ; PAIR",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_new_storage: Nat(3)
    }

    let michelson_test = {
        contract: "UNPAIR ; DROP two ; NIL operation ; PAIR",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_new_storage: Nat(3)
    }
*/
let michelson_test = {
    contract: "UNPAIR ; DROP ; NIL operation ; PAIR",
    initial_storage: Nat(5),
    parameter: Nat(3),
    storage_type: Nat,
    expected_new_storage: Nat(3)
}

// parses the Michelson code
switch Remich.parse_to_ast(michelson_test.contract) {
    | Ok(parsed_code) => {
        Js.log("\nParsed code:")
        Js.log(parsed_code)
        // creates the initial stack
        //let initial_stack_result = Remich.init_stack(~params_type=Int, ~storage_type=Int, ~initial_stack_value=["3", "4"])
        let initial_stack_result = Remich.init_stack((michelson_test.parameter, michelson_test.initial_storage))
        switch initial_stack_result {
            | Ok(initial_stack) => {
                // runs the Michelson code
                let (result, stack_snapshots) = Remich.run_code(~ast=parsed_code, ~stack=initial_stack, ~storage_type=michelson_test.storage_type)
                switch result {
                    | Ok(res) => {
                        let (rescript_res, js_res) = res
                        Js.log("\nMichelson code successfully processed!")
                        switch rescript_res {
                            | Pair({ value: (_, right) }) => {
                                if michelson_test.expected_new_storage == right {
                                    Js.log("Storage is of the expected type and value")
                                } else {
                                    Js.log("Storage is not of the expected type and value")
                                }
                            }
                            | _ => Js.log("Unexpected final value")
                        }
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
            }
            | Error(err) => {
                Js.log(err)
            }
        }
    }
    | Error(err) => {
        Js.log("\nError while parsing Michelson code")
        Js.log(err)
    }
}