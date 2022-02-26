open InstructionsTest
open Test

test("ABS instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; SUB ; ABS ",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Nat(2)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test ABS instruction with int values")

    // second test
    let test_contract = {
        contract: "UNPAIR ; ADD ; ABS",
        initial_storage: Int(3),
        parameter: Int(9),
        storage_type: Int,
        expected_output: Nat(12)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test SUB instruction with int values")

    // third test
    let test_contract = {
        contract: "UNPAIR ; ABS",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test ABS instruction with string values",
        ~expected_error="Wrong type for instruction ABS, expected int, got string",
        ~error_message="ABS should fail when given non numeric values")    
})