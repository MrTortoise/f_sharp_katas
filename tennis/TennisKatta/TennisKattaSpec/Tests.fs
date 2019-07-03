module Tests

open System
open Xunit

let score = "L-L"

[<Fact>]
let ``Score is L-L by default`` () =
    Assert.Equal("L-L", score)
