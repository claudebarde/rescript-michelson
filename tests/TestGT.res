open InstructionsTest
open Test

test("GT instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; DROP ; GT ",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Bool(true)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test GT instruction with greater than zero int")

    // second test
    let test_contract = {
        contract: "UNPAIR ; DROP ; GT",
        initial_storage: Int(-2),
        parameter: Int(9),
        storage_type: Int,
        expected_output: Bool(false)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test GT instruction with less than zero int")

    // second test
    let test_contract = {
        contract: "UNPAIR ; DROP ; GT",
        initial_storage: Int(0),
        parameter: Int(9),
        storage_type: Int,
        expected_output: Bool(false)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test GT instruction with zero int")

    // fourth test
    let test_contract = {
        contract: "UNPAIR ; DROP ; GT",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test GT instruction with string values",
        ~expected_error="Wrong type for instruction GT, expected int, got string",
        ~error_message="GT should fail when given non int values (string)")

    // fifth test
    let test_contract = {
        contract: "UNPAIR ; DROP ; GT",
        initial_storage: Nat(5),
        parameter: Nat(3),
        storage_type: Nat,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test GT instruction with nat values",
        ~expected_error="Wrong type for instruction GT, expected int, got nat",
        ~error_message="GT should fail when given non int values (nat)")
})