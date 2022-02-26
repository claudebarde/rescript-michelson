open InstructionsTest
open Test
open Remich

test("LE instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; DROP ; LE ",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Bool(false)
    }

    let run_output = run_michelson(
        ~contract=test_contract.contract, 
        ~param=test_contract.parameter, 
        ~initial_storage=test_contract.initial_storage,
        ~storage_type=test_contract.storage_type
    )
    switch run_output {
        | Ok(output) => {
            switch output.result {
            | Ok((rescript_res, _)) => {
                assertExpectedOutput(
                    ~message="Test LE instruction with greater than zero int", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => fail(~message=err, ())
        }
        }
        | Error(err) => fail(~message=err, ())
    }

    // second test
    let test_contract = {
        contract: "UNPAIR ; DROP ; LE",
        initial_storage: Int(-2),
        parameter: Int(9),
        storage_type: Int,
        expected_output: Bool(true)
    }

    let run_output = run_michelson(
        ~contract=test_contract.contract, 
        ~param=test_contract.parameter, 
        ~initial_storage=test_contract.initial_storage,
        ~storage_type=test_contract.storage_type
    )
    switch run_output {
        | Ok(output) => {
            switch output.result {
            | Ok((rescript_res, _)) => {
                assertExpectedOutput(
                    ~message="Test LE instruction with less than zero int", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => fail(~message=err, ())
        }
        }
        | Error(err) => fail(~message=err, ())
    }

    // second test
    let test_contract = {
        contract: "UNPAIR ; DROP ; LE",
        initial_storage: Int(0),
        parameter: Int(9),
        storage_type: Int,
        expected_output: Bool(true)
    }

    let run_output = run_michelson(
        ~contract=test_contract.contract, 
        ~param=test_contract.parameter, 
        ~initial_storage=test_contract.initial_storage,
        ~storage_type=test_contract.storage_type
    )
    switch run_output {
        | Ok(output) => {
            switch output.result {
            | Ok((rescript_res, _)) => {
                assertExpectedOutput(
                    ~message="Test LE instruction with zero int", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => fail(~message=err, ())
        }
        }
        | Error(err) => fail(~message=err, ())
    }

    // fourth test
    let test_contract = {
        contract: "UNPAIR ; DROP ; LE",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let run_output = run_michelson(
        ~contract=test_contract.contract, 
        ~param=test_contract.parameter, 
        ~initial_storage=test_contract.initial_storage,
        ~storage_type=test_contract.storage_type
    )
    switch run_output {
        | Ok(output) => {
            switch output.result {
            | Ok((rescript_res, _)) => {
                assertExpectedOutput(
                    ~message="Test LE instruction with string values", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => {
                if err === "Wrong type for instruction LE, expected int, got string" {
                    pass(~message="LE should fail when given non int values (string)", ())
                } else {
                    fail(~message=err, ())
                }
            }
        }
        }
        | Error(err) => fail(~message=err, ())
    }

    // fifth test
    let test_contract = {
        contract: "UNPAIR ; DROP ; LE",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_output: Unit
    }

    let run_output = run_michelson(
        ~contract=test_contract.contract, 
        ~param=test_contract.parameter, 
        ~initial_storage=test_contract.initial_storage,
        ~storage_type=test_contract.storage_type
    )
    switch run_output {
        | Ok(output) => {
            switch output.result {
            | Ok((rescript_res, _)) => {
                assertExpectedOutput(
                    ~message="Test LE instruction with nat values", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => {
                if err === "Wrong type for instruction LE, expected int, got nat" {
                    pass(~message="LE should fail when given non int values (nat)", ())
                } else {
                    fail(~message=err, ())
                }
            }
        }
        }
        | Error(err) => fail(~message=err, ())
    }
})