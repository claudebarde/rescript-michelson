open InstructionsTest
open Test
open Remich

test("ADD instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; ADD",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Int(8)
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
                    ~message="Test ADD instruction with int values", 
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
        contract: "UNPAIR ; ADD",
        initial_storage: Nat(9),
        parameter: Int(3),
        storage_type: Nat,
        expected_output: Int(12)
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
                    ~message="Test ADD instruction with int and nat values", 
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
        contract: "UNPAIR ; ADD",
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
                    ~message="Test ADD instruction with string values", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => {
                if err === "Expected a numeric type for instruction ADD, but got (string | string)" {
                    pass(~message="ADD should fail when given non numeric values", ())
                } else {
                    fail(~message=err, ())
                }
            }
        }
        }
        | Error(err) => fail(~message=err, ())
    }
})