module FS.Kata.Two_Guards_Puzzle_Kata

open System

// There are two doors. One leads to paradise, the other to hell.
// There are two guards. One always tells the truth, and the other always lies.
// You may ask one guard one question to determine the correct door.

type Door =
    | Paradise
    | Hell

type Guard =
    | Honest
    | Liar

let truth (a: bool) = a
let truth' (predicate: 'a -> bool) = predicate
let lie (a: bool) = not a
let lie' (predicate: 'a -> bool) = predicate >> not

// Questions about the world
// Is x true?
let is a = (=) a
let is4 = is 4
let a1 = 2 + 2 |> is 4
let a2 = 2 + 2 |> (is 4 >> truth)
let a3 = 2 + 2 |> (is 4 |> truth')
let a4 = 2 + 2 |> (is 4 |> lie')

// g(f) = g ∘ f
// | ∘     | truth | lie   |
// | ----- | ----- | ----- |
// | truth | truth | lie   |
// | lie   | lie   | truth |
// composition became XOR.

let a5 = 2 + 2 |> (is 4 >> truth >> lie)
let a6 = 2 + 2 |> (is 4 |> (truth' >> lie'))

let answer guard =
    match guard with
    | Honest -> truth'
    | Liar -> lie'

let randomPair (a: 'a, b: 'a) : 'a * 'a =
    if Random.Shared.Next(2) = 0 then a, b else b, a

type Reader<'env, 'a> = Reader of ('env -> 'a)

module Reader =
    let from value = Reader(fun _ -> value)

type Entrances() =
    let run (Reader f: Reader<Entrances, 'a>) entrances = f entrances

    let mutable isCollapsed = false
    let doors = randomPair (Hell, Paradise)
    let guards = randomPair (Honest, Liar)

    member private _.LeftDoorValue = fst doors
    member _.LeftDoor = Reader(fun (env: Entrances) -> env.LeftDoorValue)
    member private _.RightDoorValue = snd doors
    member _.RightDoor = Reader(fun (env: Entrances) -> env.RightDoorValue)

    member private _.LeftGuard = fst guards
    member private _.RightGuard = snd guards

    member _.AskLeftGuard predicate reader =
        Reader(fun (env: Entrances) -> answer env.LeftGuard predicate (run reader env))

    member _.AskRightGuard predicate reader =
        Reader(fun (env: Entrances) -> answer env.RightGuard predicate (run reader env))

    member this.Reveal(reader: Reader<Entrances, bool>) =
        if isCollapsed then
            Result.Error "The puzzle is already solved"
        else
            isCollapsed <- true
            Result.Ok(run reader this)

let state = Entrances()

// Questions to private Guard
let a7 = Reader.from (2 + 2) |> (is 4 |> state.AskLeftGuard) |> state.Reveal

// Questions about private state
let a8 = state.LeftDoor |> (is Hell |> state.AskLeftGuard) |> state.Reveal
//let a8 = state.AskLeftGuard <| is Hell <| state.LeftDoor |> state.Reveal

// let a9 =
//     state.LeftDoor
//     |> (is Hell |> state.AskLeftGuard)
//     |> (is true |> state.AskRightGuard)
//     |> state.Reveal <| state

//let a9 = state.LeftDoor |> ((is Hell |> state.AskLeftGuard) >> (is true |> state.AskRightGuard)) |> state.Reveal
let a9 =
    state.AskRightGuard <| is true << (state.AskLeftGuard <| is Hell)
    <| state.LeftDoor
    |> state.Reveal

// Quantum state
//let a10 = state.Reveal state.LeftDoor