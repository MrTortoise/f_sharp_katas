module Tests

open System
open Xunit

type PlayerAScore = int
type PlayerBScore = int
type GameState = { playerAScore: PlayerAScore; playerBScore: PlayerBScore }

type Commands =
    | PlayerAScores of unit
    | PlayerBScores of unit

let initial_state = { playerAScore = 0; playerBScore = 0 }

let score state =
    let calculate_score score = match score with
                                | 0 -> "L"
                                | 1 -> "15"
                                | 2 -> "30"
                                | 3 -> "40"
                                | _ -> "unsupported"

    (calculate_score state.playerAScore) + "-" + (calculate_score state.playerBScore)

let handle command state =
    match command with
    | PlayerAScores _ -> { state with playerAScore = state.playerAScore + 1 }
    | PlayerBScores _ -> { state with playerBScore = state.playerBScore + 1 }


[<Fact>]
let ``Score is L-L by default``() =
    Assert.Equal("L-L", score initial_state)

[<Fact>]
let ``Score is 15-L when player one scores``() =
    let state = initial_state |> handle (PlayerAScores())
    Assert.Equal("15-L", score state)

[<Fact>]
let ``Score is 15-15 when player one scores and player 2 scores``() =
    let state = initial_state
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())

    Assert.Equal("15-15", score state)

[<Fact>]
let ``Score is 30-15 when player one scores twice and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())

    Assert.Equal("30-15", score state)

[<Fact>]
let ``Score is 40-15 when player one scores 3 times and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerAScores())

    Assert.Equal("40-15", score state)
 
[<Fact>]
let ``Score is PlayerAWins when player one scores 4 times and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerAScores())
                |> handle (PlayerAScores())

    Assert.Equal("Player A Wins", score state)

