module Tests

open System
open Xunit

type PlayerAScore = int
type PlayerBScore = int

exception PhaseHandlingError of string
exception WinScoreExceeded of string

type GamePhase = | Early | Advantage | Won
type PlayerScore = | Deuce | AdvantageA | AdvantageB | WonA | WonB
type EarlyGameScore = | Love| Fifteen | Thirty | Fourty | Won
type GameState = { playerAScore: EarlyGameScore; playerBScore: EarlyGameScore; phase: GamePhase; score: PlayerScore; }


type Commands = | PlayerAScores | PlayerBScores

type ScoreState =
    | Scored of string
    | Unhandled of GameState

let initial_state = { playerAScore = Love; playerBScore = Love; phase = Early; score = Deuce }

let score state =
    let calculate_score (player_score:EarlyGameScore) = match player_score with
                                                        | Love -> "L"
                                                        | Fifteen -> "15"
                                                        | Thirty -> "30"
                                                        | Fourty -> "40"
                                                        | Won -> "unsupported"

    let build_early_score_string state =
        (calculate_score state.playerAScore) + "-" + (calculate_score state.playerBScore)

    let build_advantage_string state =
        match state.score with
        | AdvantageA -> "Adv PlayerA"
        | AdvantageB -> "Adv PlayerB"
        | Deuce -> "Deuce"
        | WonA -> "Player A Won"
        | WonB -> "Player B Won"

    match state.phase with
    | Early -> build_early_score_string state
    | _ -> build_advantage_string state


let set_phase_to_advantage state =
    match state.playerAScore = Fourty && state.playerBScore = Fourty with
    | true -> { state with phase = Advantage }
    | false -> state

let set_playerA_won state =
    match state.playerAScore = Won with
    | true -> { state with score = WonA; phase = Advantage }
    | false -> state

let set_playerB_won state =
    match state.playerBScore = Won with
    | true -> { state with score = WonB; phase = Advantage }
    | false -> state
    
let increment_playerA_score state =
    match state.playerAScore with
    | Love -> Fifteen
    | Fifteen -> Thirty
    | Thirty -> Fourty
    | Fourty -> Won
    | Won -> raise (WinScoreExceeded("Player A score exceeded win condition"))

let increment_playerB_score state =
    match state.playerBScore with
    | Love -> Fifteen
    | Fifteen -> Thirty
    | Thirty -> Fourty
    | Fourty -> Won
    | Won -> raise (WinScoreExceeded("Player B score exceeded win condition"))

let earlyGameScore command state =
    match command with
    | PlayerAScores -> { state with playerAScore = (increment_playerA_score state) }
    | PlayerBScores -> { state with playerBScore = (increment_playerB_score state) }
    |> set_phase_to_advantage
    |> set_playerA_won
    |> set_playerB_won


let handleAdvantage command state =
    match state.score with
    | Deuce -> match command with
               | PlayerAScores -> { state with score = AdvantageA }
               | PlayerBScores -> { state with score = AdvantageB }
    | AdvantageA -> match command with
                    | PlayerAScores -> { state with score = WonA }
                    | PlayerBScores -> { state with score = Deuce }
    | AdvantageB -> match command with
                    | PlayerAScores -> { state with score = Deuce }
                    | PlayerBScores -> { state with score = WonB }
    | _ -> raise (PhaseHandlingError("attempting to score game that isnt in advantage phase using advantage phase rules"))


let handle command state =
    match state.phase with
    | Early -> earlyGameScore command state
    | _ -> handleAdvantage command state


[<Fact>]
let ``Score is L-L by default``() =
    Assert.Equal("L-L", score initial_state)

[<Fact>]
let ``Score is 15-L when player one scores``() =
    let state = initial_state |> handle (PlayerAScores)
    Assert.Equal("15-L", score state)

[<Fact>]
let ``Score is 15-15 when player one scores and player 2 scores``() =
    let state = initial_state
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)

    Assert.Equal("15-15", score state)

[<Fact>]
let ``Score is 30-15 when player one scores twice and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)

    Assert.Equal("30-15", score state)

[<Fact>]
let ``Score is 40-15 when player one scores 3 times and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)

    Assert.Equal("40-15", score state)

[<Fact>]
let ``Score is PlayerAWins when player one scores 4 times and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)

    Assert.Equal("Player A Won", score state)

[<Fact>]
let ``Score is PlayerBWon when player two scores 4 times and player 1 scores once``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerBScores)
                |> handle (PlayerBScores)

    Assert.Equal("Player B Won", score state)

[<Fact>]
let ``Score is deuce when it is 40-40``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)

    Assert.Equal("Deuce", score state)

[<Fact>]
let ``Score is advantage player a  when it is player A scores after deuce``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)

    Assert.Equal("Adv PlayerA", score state)

[<Fact>]
let ``Score is advantage player b  when it is player b scores after deuce``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)

    Assert.Equal("Adv PlayerB", score state)

[<Fact>]
let ``Score is deuce when player A scores after player B advantage``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)

    Assert.Equal("Deuce", score state)

[<Fact>]
let ``Player A wins when scoring after advantage``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)

    Assert.Equal("Player A Won", score state)

[<Fact>]
let ``Player B wins when scoring after advantage``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerBScores)

    Assert.Equal("Player B Won", score state)


