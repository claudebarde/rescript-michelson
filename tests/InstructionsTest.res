open Test
open Remich

type michelson_test = {
    contract: string,
    initial_storage: MTypes.m_value,
    parameter: MTypes.m_value,
    storage_type: MTypes.m_type,
    expected_output: MTypes.m_value
}

let run_michelson = (
    ~contract: string, 
    ~param: MTypes.m_value, 
    ~initial_storage: MTypes.m_value
    ): run_output => {
        // creates the initial stack
        switch Remich.init_stack((param, initial_storage)) {
            | Ok(initial_stack) => {
                // runs the Michelson code
                Remich.run_code_from_json(
                    ~code=contract, 
                    ~stack=initial_stack
                )
            }
            | Error(err) => {
                                result: Error(err),
                                stack_snapshots: [],
                                comments: []
                            }
        }
}

let assertExpectedOutput = (
        ~message, 
        ~new_output: MTypes.m_value, 
        ~expected_output: MTypes.m_value
    ) => 
    assertion(
        ~message,
        ~operator="Instruction failed",
        (a, b) => a == b,
        new_output,
        expected_output
    )

let run_test_pass = (~test_data: michelson_test, ~message: string) => {
    let run_output = run_michelson(
        ~contract=test_data.contract, 
        ~param=test_data.parameter, 
        ~initial_storage=test_data.initial_storage
    )
    switch run_output.result {
        | Ok(stack) => {
            assertExpectedOutput(
                ~message, 
                ~new_output=stack[0].value,
                ~expected_output=test_data.expected_output
            )
        }
        | Error(err) => fail(~message=err, ())
    }
}

let run_test_fail = (~test_data: michelson_test, ~message: string, ~expected_error: string, ~error_message: string) => {
    let run_output = run_michelson(
        ~contract=test_data.contract, 
        ~param=test_data.parameter, 
        ~initial_storage=test_data.initial_storage
    )
    switch run_output.result {
        | Ok(stack) => {
            assertExpectedOutput(
                ~message, 
                ~new_output=stack[0].value,
                ~expected_output=test_data.expected_output
            )
        }
        | Error(err) => {
            if err === expected_error {
                pass(~message=error_message, ())
            } else {
                fail(~message=err, ())
            }
        }
    }
}