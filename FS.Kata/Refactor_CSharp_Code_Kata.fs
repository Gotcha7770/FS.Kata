module FS.Kata.Kata_1

// Refactor code from Refactor_CSharp_Code_Kata_Reference.cs

[<Literal>]
let MaxItemsPerOrder = 15

type OrderItem = { Id: string }

// parse don`t validate
// https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

type OrderInfo =
    { Id: string
      Items: OrderItem list }

type OrderId = OrderId of string
type OrderItems = OrderId of string

type Order =
    | NotVerified of OrderId * OrderItems
    | ReadyToProcess of OrderInfo
    | Processed of OrderInfo

let validate order =
    if order.Items.Length > MaxItemsPerOrder then
        Error $"The order {order.Id} has too many items"
    else
        Ok order

let processOrder (order: Order option) =
    order |> Option.map (fun x ->
        match x with
        | NotVerified (id, items) -> Error $"The order {id} isn't ready to process"
        | ReadyToProcess orderInfo -> validate orderInfo |> Result.map Processed
        | Processed _ -> Ok x)