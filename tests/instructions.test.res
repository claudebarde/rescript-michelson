open Test
open Remich

type michelson_test = {
    contract: string,
    initial_storage: MTypes.m_value,
    parameter: MTypes.m_value,
    storage_type: MTypes.m_type,
    expected_new_storage: MTypes.m_value
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

let assertExpectedStorage = (
        ~message, 
        ~new_storage: MTypes.m_value, 
        ~expected_storage: MTypes.m_value
    ) => 
    assertion(
        ~message,
        ~operator="ADD instruction failed",
        (a, b) => a === b,
        new_storage,
        expected_storage
    )

test("ADD instruction", () => {
    let test_contract = {
        contract: "UNPAIR ; ADD ; NIL operation ; PAIR",
        initial_storage: Int(5),
        parameter: Int(3),
        storage_type: Int,
        expected_new_storage: Int(8)
    }

    let run_output = run_michelson(
        ~contract=test_contract.contract, 
        ~param=test_contract.parameter, 
        ~initial_storage=test_contract.initial_storage,
        ~storage_type=test_contract.storage_type
    )
    switch run_output {
        | Ok(output) => {
            switch output.result {
            | Ok((rescript_res, _)) => {
                assertExpectedStorage(
                    ~message="Test ADD instruction with valid contract", 
                    ~new_storage=rescript_res,
                    ~expected_storage=test_contract.expected_new_storage
                )
            }
            | Error(err) => fail(~message=err, ())
        }
        }
        | Error(err) => fail(~message=err, ())
    }
})