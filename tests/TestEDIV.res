open InstructionsTest
open Test
open Remich

test("EDIV instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; EDIV",
        initial_storage: Int(4),
        parameter: Int(15),
        storage_type: Int,
        expected_output: 
            Option({ 
                    value: Some(Pair({ value: (Int(3), Nat(3)), el_type: (Int, Nat) })), 
                    el_type: Pair((Int, Nat))
                })
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
                    ~message="Test EDIV instruction with int values", 
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
        contract: "UNPAIR ; EDIV",
        initial_storage: Nat(4),
        parameter: Nat(15),
        storage_type: Nat,
        expected_output: 
            Option({ 
                    value: Some(Pair({ value: (Nat(3), Nat(3)), el_type: (Nat, Nat) })), 
                    el_type: Pair((Nat, Nat))
                })
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
                    ~message="Test EDIV instruction with int values", 
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
        contract: "UNPAIR ; EDIV",
        initial_storage: Nat(0),
        parameter: Nat(15),
        storage_type: Nat,
        expected_output: 
            Option({ 
                    value: None, 
                    el_type: Pair((Nat, Nat))
                })
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
                    ~message="Test EDIV instruction with division by zero (1)", 
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
        contract: "UNPAIR ; EDIV",
        initial_storage: Nat(15),
        parameter: Nat(0),
        storage_type: Nat,
        expected_output: 
            Option({ 
                    value: None, 
                    el_type: Pair((Nat, Nat))
                })
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
                    ~message="Test EDIV instruction with division by zero (2)", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => fail(~message=err, ())
        }
        }
        | Error(err) => fail(~message=err, ())
    }

    // fifth test
    let test_contract = {
        contract: "UNPAIR ; EDIV",
        initial_storage: Nat(15),
        parameter: Mutez(15_000_000),
        storage_type: Nat,
        expected_output: 
            Option({ 
                    value: Some(Pair({ value: (Mutez(1_000_000), Mutez(0)), el_type: (Mutez, Mutez) })), 
                    el_type: Pair((Mutez, Mutez))
                })
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
                    ~message="Test EDIV instruction with mutez/nat values", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => fail(~message=err, ())
        }
        }
        | Error(err) => fail(~message=err, ())
    }

    // sixth test
    let test_contract = {
        contract: "UNPAIR ; EDIV",
        initial_storage: Mutez(15),
        parameter: Mutez(15_000_000),
        storage_type: Mutez,
        expected_output: 
            Option({ 
                    value: Some(Pair({ value: (Nat(1_000_000), Mutez(0)), el_type: (Nat, Mutez) })), 
                    el_type: Pair((Nat, Mutez))
                })
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
                    ~message="Test EDIV instruction with mutez/mutez values", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => fail(~message=err, ())
        }
        }
        | Error(err) => fail(~message=err, ())
    }

    // seventh contract
    let test_contract = {
        contract: "UNPAIR ; EDIV",
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
                    ~message="Test EDIV instruction with string values", 
                    ~new_storage=rescript_res,
                    ~expected_output=test_contract.expected_output
                )
            }
            | Error(err) => {
                if err === "Expected a numeric type for instruction EDIV, but got (string | string)" {
                    pass(~message="EDIV should fail when given non numeric values", ())
                } else {
                    fail(~message=err, ())
                }
            }
        }
        }
        | Error(err) => fail(~message=err, ())
    }
})