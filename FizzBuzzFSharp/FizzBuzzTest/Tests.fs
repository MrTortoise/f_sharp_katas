module Tests

open System
open Xunit

type InputOrOutput = Input of int | Output of string

let pass_through_output parser input=
  match input with
    | Input x -> parser x
    | Output x -> Output x

let input_to_string value = 
  match value with
    | Input x -> x.ToString()
    | Output x -> x   
 
let replace_factor_with condition string value =
  match condition value with
    | true -> Output string
    | false -> Input value

let should_fizz input = input % 3 = 0
let should_buzz input = input % 5 = 0
let should_fizzbuzz input = should_buzz input && should_fizz input

let buzzer = replace_factor_with should_buzz "buzz"
let fizzer = replace_factor_with should_fizz "fizz" 
let fizzbuzzer = replace_factor_with should_fizzbuzz "fizzbuzz" 

let buzz_input_values input = input |> buzzer
let fizz_input_values input = input |> fizzer
let fizzbuzz_input_values input = input |> fizzbuzzer

let fizz_input input = input |> pass_through_output fizz_input_values    
let buzz_input input = input |> pass_through_output buzz_input_values
let fizzbuzz_input input = input |> pass_through_output fizzbuzz_input_values

let fizzBuzz first last = 
  seq { first .. last}
  |> Seq.map (fun x -> Input x)
  |> Seq.map fizzbuzz_input
  |> Seq.map fizz_input
  |> Seq.map buzz_input
  |> Seq.map input_to_string  
  |> String.concat " "

[<Fact>]
let ``My test`` () =
  Assert.True(true)

[<FactAttribute>]
let ``Fizz on 3`` ()  = 
  let result = fizzBuzz 1 3
  Assert.Equal("1 2 fizz", result)

[<FactAttribute>]
let ``Buzz on 5`` ()  = 
  let result = fizzBuzz 1 5
  Assert.Equal("1 2 fizz 4 buzz", result)

[<Fact>]
let ``FizzBuzz on 3 and 5`` () = 
  let result = fizzBuzz 1 15
  Assert.Equal("1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz", result)

