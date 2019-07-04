module Tests

open System
open Xunit

type PlayerAScore = int
type PlayerBScore = int
type GameState = { playerAScore: PlayerAScore; playerBScore: PlayerBScore }

type Commands =
    | PlayerAScores of unit
    | PlayerBScores of unit

type ScoreState =
    | Scored of string
    | Unhandled of GameState

let initial_state = { playerAScore = 0; playerBScore = 0 }

let score state =
    let apply_if_unscored to_apply state =
        match state with
        | Scored s -> Scored s
        | Unhandled un -> (to_apply un)

    let score_state_to_string state =
        match state with
        | Scored s -> s
        | Unhandled _ -> "error unhandled scoring scenario"

    let early_game_win_scorer state =
        let simple_win_condition (current_player_score: int, other_player_score: int) =
            other_player_score < 3 && current_player_score > 3

        let handlePlayerAWon (state) =
            match simple_win_condition (state.playerAScore, state.playerBScore) with
            | true -> Scored "Player A Won"
            | false -> Unhandled state
            
        let handlePlayerBWon (state) =
            match simple_win_condition (state.playerBScore, state.playerAScore) with
            | true -> Scored "Player B Won"
            | false -> Unhandled state

        state
        |> apply_if_unscored (handlePlayerAWon)
        |> apply_if_unscored (handlePlayerBWon)

    let early_game_scorer state =
        let calculate_score score = match score with
                                    | 0 -> "L"
                                    | 1 -> "15"
                                    | 2 -> "30"
                                    | 3 -> "40"
                                    | _ -> "unsupported"

        let build_score_string state =
            Scored((calculate_score state.playerAScore) + "-" + (calculate_score state.playerBScore))

        state
        |> apply_if_unscored (build_score_string)

    (Unhandled state)
    |> early_game_win_scorer
    |> early_game_scorer
    |> score_state_to_string

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

    Assert.Equal("Player A Won", score state)

[<Fact>]
let ``Score is PlayerBWon when player two scores 4 times and player 1 scores once``() =
    let state = initial_state
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerBScores())
                |> handle (PlayerBScores())

    Assert.Equal("Player B Won", score state)
    
//[<Fact>]
//let ``Score is deuce when it is 40-40``() =
//    let state = initial_state
//                |> handle (PlayerBScores())
//                |> handle (PlayerAScores())
//                |> handle (PlayerBScores())
//                |> handle (PlayerAScores())
//                |> handle (PlayerBScores())
//                |> handle (PlayerAScores())
//    
//    Assert.Equal("Deuce", score state)
    
