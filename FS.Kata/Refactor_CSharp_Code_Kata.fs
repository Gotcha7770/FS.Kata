module FS.Kata.Kata_1open

open System

// Refactor code from Refactor_CSharp_Code_Kata_Reference.cs

type OrderItem = { Id: string }

module OrderItems =
    [<Literal>]
    let private MaxItems = 15

    type OrderItems = private OrderItems of OrderItem list

    let create (items: OrderItem list) =
        match items with
        | [] -> Error "Order must have at least one item"
        | list ->
            if list.Length > MaxItems then Error $"Order must have less than {MaxItems} items"
            else Ok (OrderItems list)

module Order =

    open OrderItems

    type Order =
        private
        | Verified of Id: string * OrderItem list
        | ReadyToProcess of Id: string * Items: OrderItems
        | Processed of Id: string * ProcessedAt: DateTime

    let dispatch (order: Order) =
        match order with
        | Verified (id, items) -> create items |> Result.map (fun x -> ReadyToProcess (id, x))
        | ReadyToProcess (id, _) -> Processed (id, DateTime.Now) |> Ok
        | Processed _ -> Ok order