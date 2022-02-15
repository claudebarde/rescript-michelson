module Error_msg = {
    let stack_not_deep_enough = 
        (instr, depth) => `The provided stack is not deep enough for instruction ${instr}, got ${Belt.Int.toString(depth)}`
    let unexpected_stack_depth =
        (expected, received) => j`Unexpected stack depth, expected $expected, got $received`
    let wrong_type =
        (~instr=?, ~expected, ~received) =>
            switch instr {
                | None => `Wrong type, expected ${expected}, got ${received}`
                | Some(i) => `Wrong type for instruction ${i}, expected ${expected}, got ${received}`
            }
    let unexpected_value =
        (expected, received) => `Unexpected value, expected ${expected}, but got ${received}`
    let non_numeric_type =
        (instr, received) => `Expected a numeric type for instruction ${instr}, but got ${received}`
}