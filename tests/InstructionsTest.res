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
    ~initial_storage: MTypes.m_value, 
    ~storage_type: MTypes.m_type): result<run_output, string> => {
        // creates the initial stack
        switch Remich.init_stack((param, initial_storage)) {
            | Ok(initial_stack) => {
                // runs the Michelson code
                Ok(Remich.run_code_from_json(
                    ~code=contract, 
                    ~stack=initial_stack, 
                    ~storage_type=storage_type
                ))
            }
            | Error(err) => Error(err)
        }
}

let assertExpectedOutput = (
        ~message, 
        ~new_storage: MTypes.m_value, 
        ~expected_output: MTypes.m_value
    ) => 
    assertion(
        ~message,
        ~operator="Instruction failed",
        (a, b) => a == b,
        new_storage,
        expected_output
    )