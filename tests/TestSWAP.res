open InstructionsTest
open Test

test("SWAP instruction", () => {
    // first test
    let test_contract = {
        contract: "UNPAIR ; SWAP ; SUB ",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_output: Int(2)
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test SWAP instruction")

    // second test
    let test_contract = {
        contract: "UNPAIR ; SWAP ; CONCAT",
        initial_storage: String("Hello"),
        parameter: String(" world"),
        storage_type: String,
        expected_output: String("Hello world")
    }

    let _ = run_test_pass(
        ~test_data=test_contract, 
        ~message="Test SWAP instruction with string values")

    // third test
    let test_contract = {
        contract: "UNPAIR ; DROP ; SWAP",
        initial_storage: String(" world"),
        parameter: String("Hello"),
        storage_type: String,
        expected_output: Unit
    }

    let _ = run_test_fail(
        ~test_data=test_contract, 
        ~message="Test SWAP instruction with invalid stack",
        ~expected_error="The provided stack is not deep enough for instruction SWAP, got depth of 1",
        ~error_message="SWAP should fail when there is only 1 element on the stack")
})