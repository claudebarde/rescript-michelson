open InstructionsTest
open Test

test("SUB instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; SUB",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Int(-2)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test SUB instruction with int values")

    // second test
    let test_contract = {
        contract: "UNPAIR ; SUB",
        initial_storage: Nat(3),
        parameter: Int(9),
        storage_type: Nat,
        expected_output: Int(6)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test SUB instruction with int and nat values")

    // third test
    let test_contract = {
        contract: "UNPAIR ; SUB",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test SUB instruction with string values",
        ~expected_error="Wrong type for instruction SUB, expected two numeric types, got string and string",
        ~error_message="SUB should fail when given non numeric values")
})