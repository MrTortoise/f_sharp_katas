module Tests

open System
open Xunit

exception PhaseHandlingError of string
exception WinScoreExceeded of string

type PlayerScore = | Deuce | AdvantageA | AdvantageB | WonA | WonB
type EarlyGameScore = | Love | Fifteen | Thirty | Fourty | Won
type EarlyGameState = { playerA: EarlyGameScore; playerB: EarlyGameScore }
type AdvantageState = { score: PlayerScore }
type GameState =
    | EarlyGame of EarlyGameState
    | AdvantageGame of AdvantageState

type Commands = | PlayerAScores | PlayerBScores

type ScoreState =
    | Scored of string
    | Unhandled of GameState

let initial_state = EarlyGame { playerA = Love; playerB = Love }

let getScore state =
    let earlyToString player_score = match player_score with
                                       | Love -> "L"
                                       | Fifteen -> "15"
                                       | Thirty -> "30"
                                       | Fourty -> "40"
                                       | Won -> "unsupported"

    let advantageToString score =
        match score with
        | AdvantageA -> "Adv PlayerA"
        | AdvantageB -> "Adv PlayerB"
        | Deuce -> "Deuce"
        | WonA -> "Player A Won"
        | WonB -> "Player B Won"

    match state with
    | EarlyGame s -> (earlyToString s.playerA) + "-" + (earlyToString s.playerB)
    | AdvantageGame s -> advantageToString s.score

let handle command state =
    let transitionToDeuce state =
        match state.playerA = Fourty && state.playerB = Fourty with
        | true -> AdvantageGame { score = Deuce }
        | false -> EarlyGame state

    let transitionToPlayerAWon state =
        match state with
        | EarlyGame s -> match s.playerA = Won with
                         | true -> AdvantageGame { score = WonA }
                         | false -> EarlyGame s
        | AdvantageGame _ -> state

    let transitionToPlayerBWon state =
        match state with
        | EarlyGame s -> match s.playerB = Won with
                         | true -> AdvantageGame { score = WonB }
                         | false -> EarlyGame s
        | AdvantageGame _ -> state

    let incrementScore score =
        match score with
        | Love -> Fifteen
        | Fifteen -> Thirty
        | Thirty -> Fourty
        | Fourty -> Won
        | Won -> raise (WinScoreExceeded("Player A score exceeded win condition"))

    let scoreEarlyGame command state =
        match command with
        | PlayerAScores -> { state with playerA = (incrementScore state.playerA) }
        | PlayerBScores -> { state with playerB = (incrementScore state.playerB) }
        |> transitionToDeuce
        |> transitionToPlayerAWon
        |> transitionToPlayerBWon

    let scoreAdvantage command state =
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
        
    match state with
    | EarlyGame s -> scoreEarlyGame command s
    | AdvantageGame s -> AdvantageGame(scoreAdvantage command s)


[<Fact>]
let ``Score is L-L by default``() =
    Assert.Equal("L-L", getScore initial_state)

[<Fact>]
let ``Score is 15-L when player one scores``() =
    let state = initial_state |> handle (PlayerAScores)
    Assert.Equal("15-L", getScore state)

[<Fact>]
let ``Score is 15-15 when player one scores and player 2 scores``() =
    let state = initial_state
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)

    Assert.Equal("15-15", getScore state)

[<Fact>]
let ``Score is 30-15 when player one scores twice and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)

    Assert.Equal("30-15", getScore state)

[<Fact>]
let ``Score is 40-15 when player one scores 3 times and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)

    Assert.Equal("40-15", getScore state)

[<Fact>]
let ``Score is PlayerAWins when player one scores 4 times and player 2 scores once``() =
    let state = initial_state
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)
                |> handle (PlayerAScores)

    Assert.Equal("Player A Won", getScore state)

[<Fact>]
let ``Score is PlayerBWon when player two scores 4 times and player 1 scores once``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerBScores)
                |> handle (PlayerBScores)

    Assert.Equal("Player B Won", getScore state)

[<Fact>]
let ``Score is deuce when it is 40-40``() =
    let state = initial_state
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)
                |> handle (PlayerBScores)
                |> handle (PlayerAScores)

    Assert.Equal("Deuce", getScore state)

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

    Assert.Equal("Adv PlayerA", getScore state)

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

    Assert.Equal("Adv PlayerB", getScore state)

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

    Assert.Equal("Deuce", getScore state)

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

    Assert.Equal("Player A Won", getScore state)

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

    Assert.Equal("Player B Won", getScore state)


