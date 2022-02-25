open InstructionsTest
open Test
open Remich

test("SWAP instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; SWAP ; SUB ",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Int(2)
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
                    ~message="Test SWAP instruction", 
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
        contract: "UNPAIR ; SWAP ; CONCAT",
        initial_storage: String("Hello"),
        parameter: String(" world"),
        storage_type: String,
        expected_output: String("Hello world")
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
                    ~message="Test SWAP instruction with string values", 
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
        contract: "UNPAIR ; DROP ; SWAP",
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
                    ~message="Test SWAP instruction with invalid stack", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => {
                if err === "The provided stack is not deep enough for instruction SWAP, got depth of 1" {
                    pass(~message="SWAP should fail when there is only 1 element on the stack", ())
                } else {
                    fail(~message=err, ())
                }
            }
        }
        }
        | Error(err) => fail(~message=err, ())
    }
})