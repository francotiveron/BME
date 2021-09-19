module Types

[<CLIMutable>]
type SellRequest = {
    Odds: decimal
    Amount: decimal
    ParticipantId: int
    MarketId: int
}

type Sell = {
    Odds: decimal
    Amount: decimal
    ParticipantId: int
    mutable Filled: decimal
}

[<CLIMutable>]
type BuyRequest = {
    Amount: decimal
    ParticipantId: int
    MarketId: int
}

type Buy = {
    Odds: decimal
    Amount: decimal
    mutable Filled: decimal
    Sells: ResizeArray<int * decimal>
}

type BuyResponse = {
    Status: string
    Odds: decimal
    Amount: decimal
}

[<CLIMutable>]
type ShowBetsRequest = {
    MarketId: int
    ParticipantId: int
}

type ShowBetsResponse = {
    MarketId: int
    SellerId: int
    BettorId: int
    Amount: decimal
    Odds: decimal
}

type UnableToRemoveReason = | AlreadyFilled | NotFound
type RemoveResponse = | RemovalAccepted | UnableToRemove of UnableToRemoveReason

type Message =
    | AddMarket of AsyncReplyChannel<int>
    | Sell of SellRequest * AsyncReplyChannel<Result<int, string>>
    | Buy of BuyRequest * AsyncReplyChannel<Result<decimal * decimal, string>>
    | GetOrders of marketId:int * AsyncReplyChannel<Result<(int * Sell) list, string>>
    | RemoveSell of marketId:int * sellId:int * AsyncReplyChannel<Result<RemoveResponse, string>>
    | GetBets of marketId:int * participantId:int * AsyncReplyChannel<Result<ShowBetsResponse list, string>>

module Sell = 
    let composeFullId marketId sellId = $"{marketId}-{sellId}"
    let decomposeFullId (fullId: System.String) = 
        try
            match fullId.Split('-') with
            | [| sMarketId; sSellId |] -> Ok (System.Int32.Parse(sMarketId), System.Int32.Parse(sSellId))
            | _ -> failwith "Invalid"
        with x -> Error x.Message