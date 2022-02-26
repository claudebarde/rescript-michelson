open Remich
open MTypes

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
/*
    CONCAT tests

    let michelson_test = {
        contract: "UNPAIR ; CONCAT ; NIL operation ; PAIR",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_new_storage: String("Hello world")
    }

    let michelson_test = {
        contract: "UNPAIR ; CONCAT ; NIL operation ; PAIR",
        initial_storage: List({ el_type: String, value: list{String(" a"), String(" test")}}),
        parameter: List({ el_type: String, value: list{String("this"), String(" is")}}),
        storage_type: List(String),
        expected_new_storage: List({ el_type: String, value: list{String("this"), String(" is"), String(" a"), String(" test")}})
    }

    let michelson_test = {
        contract: "UNPAIR ; CONCAT ; NIL operation ; PAIR",
        initial_storage: String(" world"),
        parameter: Int(8),
        storage_type: String,
        expected_new_storage: String("Hello world")
    }

    let michelson_test = {
        contract: "UNPAIR ; CONCAT ; NIL operation ; PAIR",
        initial_storage: List({ el_type: String, value: list{String(" a"), String(" test")}}),
        parameter: List({ el_type: String, value: list{Int(6), Int(7)}}),
        storage_type: List(String),
        expected_new_storage: List({ el_type: String, value: list{String("this"), String(" is"), String(" a"), String(" test")}})
    }
*/
/*
    SWAP tests

    let michelson_test = {
        contract: "UNPAIR ; CONCAT ; NIL operation ; PAIR",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_new_storage: String("Hello world")
    }
*/
let michelson_test = {
        contract: `UNPAIR ; DROP ; PUSH string " world" ; SWAP ; CONCAT ; NIL operation ; PAIR`,
        initial_storage: String("Hello"),
        parameter: Unit,
        storage_type: String,
        expected_new_storage: String("Hello world")
    }


// creates the initial stack
switch Remich.init_stack((michelson_test.parameter, michelson_test.initial_storage)) {
    | Ok(initial_stack) => {
        // runs the Michelson code
        let run_output = Remich.run_code_from_json(
            ~code=michelson_test.contract, 
            ~stack=initial_stack
        )->Remich.verify_output_stack(michelson_test.storage_type)
        switch run_output {
            | Ok({ result: (rescript_res, js_res), stack_snapshots }) => {
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
        Js.log("Error while creating the initial stack")
        Js.log(err)
    }
}

// tests MTypes.parse_json_to_type
/*type code_to_test = { micheline: string, expected_output: m_type, should_work: bool }
let test_code = (code_to_test: code_to_test) => {
    let michelson_json = 
        TaquitoMichelCodec.michel_codec_parser()->TaquitoMichelCodec.parse_micheline_expression(code_to_test.micheline)
    // verifies the JSON returned by michel-codec
    let michelson_json = try Js.Json.parseExn(michelson_json->Js.Json.stringify) catch {
    | _ => failwith("Error parsing Michelson JSON returned by @taquito/michel-codec")
    }
    switch parse_json_to_type(michelson_json) {
        | Ok(res) =>
            /*if res == code_to_test.expected_output {
                Js.log3(code_to_test.should_work ? "+" : "-", `Test for ${code_to_test.micheline}`, ": Success!")
            } else {
                Js.log3(code_to_test.should_work ? "-" : "+", `Test for ${code_to_test.micheline}`, ": Failed!")
            }*/
            if res == code_to_test.expected_output && (code_to_test.should_work || !code_to_test.should_work) {
                Js.log3(code_to_test.should_work ? "+" : "-", `Test for ${code_to_test.micheline}`, ": Success!")
            } else {
                Js.log2(`Test for ${code_to_test.micheline}`, ": Failed!")
                Js.log(`(output: ${m_type_to_string(res)} / expected: ${m_type_to_string(code_to_test.expected_output)})`)
            }
        | Error(err) => 
            Js.log2(`Test for ${code_to_test.micheline}`, ": Failed!")
            Js.log(err)
    }
}

Js.log("\n")
let code_to_test = { micheline: "nat 5", expected_output: Nat, should_work: true }
test_code(code_to_test)
let code_to_test = { micheline: `string "Tezos"`, expected_output: String, should_work: true }
test_code(code_to_test)
let code_to_test = { micheline: `(pair string nat)`, expected_output: Pair((String, Nat)), should_work: true }
test_code(code_to_test)
let code_to_test = { micheline: `(or nat string)`, expected_output: Or((Nat, String)), should_work: true }
test_code(code_to_test)
let code_to_test = { micheline: `(list nat)`, expected_output: List(Nat), should_work: true }
test_code(code_to_test)
// more complex
let code_to_test = { 
    micheline: `(pair (pair string string) (pair int nat))`, 
    expected_output: Pair((Pair(String, String), Pair(Int, Nat))), 
    should_work: true }
test_code(code_to_test)
let code_to_test = { 
    micheline: `(list (pair (pair string string) (pair int nat)))`, 
    expected_output: List(Pair((Pair(String, String), Pair(Int, Nat)))), 
    should_work: true }
test_code(code_to_test)
let code_to_test = { 
    micheline: `(list (list (pair (pair string string) (pair int nat))))`, 
    expected_output: List(List(Pair((Pair(String, String), Pair(Int, Nat))))), 
    should_work: true
 }
test_code(code_to_test)
*/
