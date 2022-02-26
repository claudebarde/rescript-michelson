open InstructionsTest
open Test

test("EQ instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; DROP ; EQ ",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Bool(false)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test EQ instruction with non zero int")

    // second test
    let test_contract = {
        contract: "UNPAIR ; DROP ; EQ",
        initial_storage: Int(0),
        parameter: Int(9),
        storage_type: Int,
        expected_output: Bool(true)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test EQ instruction with int 0")

    // third test
    let test_contract = {
        contract: "UNPAIR ; DROP ; EQ",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test EQ instruction with string values",
        ~expected_error="Wrong type for instruction EQ, expected int, got string",
        ~error_message="EQ should fail when given non int values (string)") 

    // fourth test
    let test_contract = {
        contract: "UNPAIR ; DROP ; EQ",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test EQ instruction with nat values",
        ~expected_error="Wrong type for instruction EQ, expected int, got nat",
        ~error_message="EQ should fail when given non int values (nat)") 
})