module FS.Kata.Kata_1

open System

// Refactor code from Refactor_CSharp_Code_Kata_Reference.cs

type OrderItem = { Id: string }

// parse don`t validate
// https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

type OrderInfo =
    { Id: string
      Items: OrderItem list }

type OrderId = OrderId of string
type OrderItems = private OrderItems of OrderItem list

module OrderItems =
    [<Literal>]
    let MaxCount = 15

    let create items =
        match items with
        | [] -> Error "Order must have at least one item"
        | list ->
            let length = List.length list
            if length > MaxCount then Error $"Order must have no more than {MaxCount} items"
            else Ok(OrderItems items)

type UnverifiedOrder =
    { Id: OrderId
      Items: OrderItem list }

type VerifiedOrder =
    private { Id: OrderId
              Items: OrderItems }

type ProcessedOrder =
    private { Order: VerifiedOrder
              ProcessedAt: DateTimeOffset }

let verify (order: UnverifiedOrder) =
    OrderItems.create order.Items
    |> Result.map (fun items -> { Id = order.Id; Items = items })

let processOrder now order  =
    { Order = order; ProcessedAt = now }

type Order =
    | Unverified of UnverifiedOrder
    | Verified of VerifiedOrder
    | Processed of ProcessedOrder

let advance now order =
    match order with
    | Unverified o -> verify o |> Result.map Verified
    | Verified o -> processOrder now o |> Processed |> Ok
    | Processed _ -> Ok order