module FS.Kata.Two_Guards_Puzzle_Kata

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

open System

type Selector<'a> = private Selector of (Entrances -> 'a)

and Entrances() =
    let run (Selector f) state = f state

    let shuffle (list: 'a list) =
        let array = List.toArray list
        let random = Random()

        for i = array.Length - 1 downto 1 do
            let j = random.Next(i + 1)
            let tmp = array.[i]
            array.[i] <- array.[j]
            array.[j] <- tmp

        Array.toList array

    let answer guard =
        match guard with
        | Honest -> truth'
        | Liar -> lie'

    let doors = shuffle [ Paradise; Hell ]
    let guards = shuffle [ Honest; Liar ]

    member private _.LeftDoorValue = doors[0]
    member _.LeftDoor = Selector _.LeftDoorValue
    member private _.RightDoorValue = doors[1]
    member _.RightDoor = Selector _.RightDoorValue

    member private _.LeftGuard = guards[0]
    member private _.RightGuard = guards[1]

    member this.AskLeftGuard predicate selector =
        let value = run selector this

        answer this.LeftGuard predicate value

    member this.AskRightGuard predicate selector =
        let value = run selector this

        answer this.RightGuard predicate value

let state = Entrances()

module Selector =
    let from value = Selector(fun _ -> value)

// Questions to private Guard
let a7 = Selector.from (2 + 2) |> (is 4 |> state.AskLeftGuard)

// Questions about private state
let a8 = state.LeftDoor |> (is Hell |> state.AskLeftGuard)
let a9 = state.LeftDoor |> (is Hell |> (state.AskLeftGuard >> state.AskRightGuard))