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
/*
    DUP tests

    let michelson_test = {
        contract: "UNPAIR ; DUP ; ADD ; ADD ; NIL operation ; PAIR",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_new_storage: Nat(11)
    }
*/
/*
    CAR tests

    let michelson_test = {
        contract: "CAR ; NIL operation ; PAIR",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_new_storage: Nat(3)
    }
*/
/*
    CDR tests

    let michelson_test = {
        contract: "CDR ; NIL operation ; PAIR",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_new_storage: Nat(5)
    }
*/
let michelson_test = {
        contract: "CDR ; NIL operation ; PAIR",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_new_storage: Nat(5)
    }

// creates the initial stack
switch Remich.init_stack((michelson_test.parameter, michelson_test.initial_storage)) {
    | Ok(initial_stack) => {
        // runs the Michelson code
        let run_output = Remich.run_code_from_json(~code=michelson_test.contract, ~stack=initial_stack, ~storage_type=michelson_test.storage_type)
        switch run_output.result {
            | Ok((rescript_res, js_res)) => {
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
                Js.log2("Stack snapshots:", run_output.stack_snapshots)
            }
            | Error(err) => Js.log2("\nResult -> Error:", err)
        }
    }
    | Error(err) => {
        Js.log("Error while creating the initial stack")
        Js.log(err)
    }
}