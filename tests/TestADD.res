open InstructionsTest
open Test

test("ADD instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; ADD",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Int(8)
    }  

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test ADD instruction with int values")

    // second test
    let test_contract = {
        contract: "UNPAIR ; ADD",
        initial_storage: Nat(9),
        parameter: Int(3),
        storage_type: Nat,
        expected_output: Int(12)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test ADD instruction with int and nat values")

    // third test
    let test_contract = {
        contract: "UNPAIR ; ADD",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test ADD instruction with string values",
        ~expected_error="Expected a numeric type for instruction ADD, but got (string | string)",
        ~error_message="ADD should fail when given non numeric values")
})