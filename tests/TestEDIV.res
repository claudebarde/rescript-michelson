open InstructionsTest
open Test

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

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test EDIV instruction with int values")

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

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test EDIV instruction with int values")

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

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test EDIV instruction with division by zero (1)")

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

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test EDIV instruction with division by zero (2)")

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

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test EDIV instruction with mutez/nat values")

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

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test EDIV instruction with mutez/mutez values")

    // seventh contract
    let test_contract = {
        contract: "UNPAIR ; EDIV",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test EDIV instruction with string values",
        ~expected_error="Expected a numeric type for instruction EDIV, but got (string | string)",
        ~error_message="EDIV should fail when given non numeric values")
})