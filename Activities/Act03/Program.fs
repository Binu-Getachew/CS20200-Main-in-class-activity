/// KAIST CS220 In-Class Activity Project

/// Modify the function `gcd` in such a way that the function computes the
/// greatest common divisor of two 32-bit integers.
let gcd a b =
  let minimum=min a b
  let mutable factor=1
  for i in 1..minimum do
    if a%i=0 && b%i=0 then
        factor<-i
  factor    

let result3=gcd 4 8
printfn "The GCD of %d and %d is %d" 4 8 result3
