open InstructionsTest
open Test

test("MUL instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; MUL",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Int(15)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test MUL instruction with int values")

    // second test
    let test_contract = {
        contract: "UNPAIR ; MUL",
        initial_storage: Nat(9),
        parameter: Int(3),
        storage_type: Nat,
        expected_output: Int(27)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test MUL instruction with int and nat values")

    // third test
    let test_contract = {
        contract: "UNPAIR ; MUL",
        initial_storage: Nat(6),
        parameter: Nat(3),
        storage_type: Nat,
        expected_output: Nat(18)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test MUL instruction with nat values")

    // fourth test
    let test_contract = {
        contract: "UNPAIR ; MUL",
        initial_storage: Nat(6),
        parameter: Mutez(3_000_000),
        storage_type: Nat,
        expected_output: Mutez(18_000_000)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test MUL instruction with mutez and nat values")

    // fifth test
    let test_contract = {
        contract: "UNPAIR ; MUL",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test MUL instruction with string values",
        ~expected_error="Expected a numeric type for instruction MUL, but got (string | string)",
        ~error_message="MUL should fail when given non numeric values")

    // sixth test
    let test_contract = {
        contract: "UNPAIR ; MUL",
        initial_storage: Mutez(6),
        parameter: Mutez(3_000_000),
        storage_type: Mutez,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test MUL instruction with mutez values",
        ~expected_error="Cannot use MUL instruction with 2 mutez values",
        ~error_message="MUL should fail when given 2 mutez values")
})