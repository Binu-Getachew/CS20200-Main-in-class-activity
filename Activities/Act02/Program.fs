/// KAIST CS220 In-Class Activity Project

/// Modify the function `max` in such a way that the function takes in four
/// 32-bit integers, and returns the largest number as output.
let max a b c d =
  max (max a b) (max c d)

let result2= max -42 42 0 -1
printfn "The maximum is %d" result2

