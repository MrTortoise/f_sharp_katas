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
    let applyIfUnscored to_apply state =
        match state with
        | Scored s -> Scored s
        | Unhandled un -> (to_apply un)

    let scoreStateToString state =
        match state with
        | Scored s -> s
        | Unhandled _ -> "error unhandled scoring scenario"

    let earlyGameWinScorer state =
        let simpleWinCondition (current_player_score: int, other_player_score: int) =
            other_player_score < 3 && current_player_score > 3

        let handlePlayerAWon (state) =
            match simpleWinCondition (state.playerAScore, state.playerBScore) with
            | true -> Scored "Player A Won"
            | false -> Unhandled state

        let handlePlayerBWon (state) =
            match simpleWinCondition (state.playerBScore, state.playerAScore) with
            | true -> Scored "Player B Won"
            | false -> Unhandled state

        state
        |> applyIfUnscored (handlePlayerAWon)
        |> applyIfUnscored (handlePlayerBWon)

    let advantageGameScorer state =
        let applyIfInAdvantage (to_apply: GameState -> ScoreState, game_state: GameState): ScoreState =
            match game_state.playerAScore > 2 && game_state.playerBScore > 2 with
            | true -> to_apply (game_state)
            | false -> Unhandled game_state

        let deuceScorer game_state =
            match game_state.playerAScore = game_state.playerBScore with
            | true -> Scored "Deuce"
            | false -> Unhandled game_state
            
        let advantageAScorer game_state =
            match game_state.playerAScore > game_state.playerBScore with
            | true -> Scored "Adv PlayerA"
            | false -> Unhandled game_state
            
        let advantageBScorer game_state =
            match game_state.playerBScore > game_state.playerAScore with
            | true -> Scored "Adv PlayerB"
            | false -> Unhandled game_state
            
        let winnerPlayerA game_state =
            match game_state.playerAScore > game_state.playerBScore + 1 with
            | true -> Scored "Player A Won"
            | false -> Unhandled game_state
            
        let winnerPlayerB game_state =
            match game_state.playerBScore > game_state.playerAScore + 1 with
            | true -> Scored "Player B Won"
            | false -> Unhandled game_state
        
        let deuceScorerHandler state = applyIfInAdvantage(deuceScorer, state)
        let advantageAHandler state = applyIfInAdvantage(advantageAScorer, state)
        let advantageBHandler state = applyIfInAdvantage(advantageBScorer, state)
        let winnerPlayerAHandler state = applyIfInAdvantage(winnerPlayerA, state)
        let winnerPlayerBHandler state = applyIfInAdvantage(winnerPlayerB, state)

        state
        |> applyIfUnscored winnerPlayerAHandler
        |> applyIfUnscored winnerPlayerBHandler
        |> applyIfUnscored deuceScorerHandler
        |> applyIfUnscored advantageAHandler
        |> applyIfUnscored advantageBHandler


    let early_game_scorer state =
        let calculate_score player_score = match player_score with
                                           | 0 -> "L"
                                           | 1 -> "15"
                                           | 2 -> "30"
                                           | 3 -> "40"
                                           | _ -> "unsupported"

        let build_score_string state =
            Scored((calculate_score state.playerAScore) + "-" + (calculate_score state.playerBScore))

        state
        |> applyIfUnscored (build_score_string)

    (Unhandled state)
    |> earlyGameWinScorer
    |> advantageGameScorer
    |> early_game_scorer
    |> scoreStateToString

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

[<Fact>]
let ``Score is deuce when it is 40-40``() =
    let state = initial_state
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())

    Assert.Equal("Deuce", score state)
    
[<Fact>]
let ``Score is advantage player a  when it is player A scores after deuce``() =
    let state = initial_state
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerAScores())

    Assert.Equal("Adv PlayerA", score state)

[<Fact>]
let ``Score is advantage player b  when it is player b scores after deuce``() =
    let state = initial_state
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())

    Assert.Equal("Adv PlayerB", score state)
    
[<Fact>]
let ``Score is deuce when player A scores after player B advantage``() =
    let state = initial_state
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())

    Assert.Equal("Deuce", score state)
 
[<Fact>]
let ``Player A wins when scoring after advantage``() =
    let state = initial_state
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerAScores())
                |> handle (PlayerAScores())

    Assert.Equal("Player A Won", score state)
    
[<Fact>]
let ``Player B wins when scoring after advantage``() =
    let state = initial_state
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerAScores())
                |> handle (PlayerBScores())
                |> handle (PlayerBScores())

    Assert.Equal("Player B Won", score state)
    

