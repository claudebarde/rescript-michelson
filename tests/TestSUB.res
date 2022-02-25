open InstructionsTest
open Test
open Remich

test("SUB instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; SUB",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Int(-2)
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
                    ~message="Test SUB instruction with int values", 
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
        contract: "UNPAIR ; SUB",
        initial_storage: Nat(3),
        parameter: Int(9),
        storage_type: Nat,
        expected_output: Int(6)
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
                    ~message="Test SUB instruction with int and nat values", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => fail(~message=err, ())
        }
        }
        | Error(err) => fail(~message=err, ())
    }

    // third test
    let test_contract = {
        contract: "UNPAIR ; SUB",
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
                    ~message="Test SUB instruction with string values", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => {
                if err === "Wrong type for instruction SUB, expected two numeric types, got string and string" {
                    pass(~message="SUB should fail when given non numeric values", ())
                } else {
                    fail(~message=err, ())
                }
            }
        }
        }
        | Error(err) => fail(~message=err, ())
    }
})