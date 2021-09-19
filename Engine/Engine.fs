module Engine
#nowarn "25" "26"

open Types
open Market

type Engine() = 
    let markets = ResizeArray<Market>()

    let agent =
        let go marketId requestor (ch: AsyncReplyChannel<Result<_,_>>) = async {
            let! r = 
                if marketId > 0 && marketId <= markets.Count then
                    requestor markets.[marketId - 1]
                else
                    async { return (Error $"Market {marketId} does not exist") }
            ch.Reply r
        }

        MailboxProcessor.Start (fun inbox ->
            let rec loop() = async {
                try
                    let! m = inbox.Receive()
                
                    match m with
                    | AddMarket ch -> 
                        markets.Add(Market(markets.Count + 1))
                        ch.Reply markets.Count
                    | Sell (req, ch) -> do! go req.MarketId (fun market -> market.Sell req) ch
                    | Buy (req, ch) -> do! go req.MarketId (fun market -> market.Buy req) ch
                    | GetOrders (marketId, ch) -> do! go marketId (fun market -> market.GetOrders()) ch
                    | RemoveSell (marketId, sellId, ch) -> do! go marketId (fun market -> market.RemoveSell sellId) ch
                    | GetBets (marketId, participantId, ch) -> do! go marketId (fun market -> market.GetBets participantId) ch
                    | _ -> failwith $"Invalid/Unhandled Message {m}"
                with _ -> () //Todo
                return! loop()
            }
            loop()
        )

    let send messageBuilder = agent.PostAndAsyncReply(fun ch -> messageBuilder ch)

    let getBetsInAllMarkets participantId = async {
        let! results = 
            [1..markets.Count] 
            |> List.map (fun i -> send (fun ch -> GetBets (i, participantId, ch)))
            |> Async.Parallel

        return
            match results |> Array.groupBy (function | Error _ -> true | Ok _ -> false) with
            | [| _; true, errs |] | [| true, errs; _ |] | [| true, errs |] -> 
                errs |> Array.map (function | Error msg -> msg) |> String.concat " --- " |> Error
            | [| false, resps |] -> 
                resps |> Array.map (function | Ok rsp -> rsp) |> List.concat |> Ok
    }

    member _.AddMarket() = send (fun ch -> AddMarket ch)

    member _.Sell req = send (fun ch -> Sell (req, ch))

    member _.Buy req = send (fun ch -> Buy (req, ch))

    member _.GetOrders marketId = send (fun ch -> GetOrders (marketId, ch))

    member _.RemoveSell fullId = async {
        return!
            match Sell.decomposeFullId fullId with
            | Ok (marketId, sellId) -> send (fun ch -> RemoveSell (marketId, sellId, ch))
            | Error msg -> async { return Error msg }
    }

    member _.GetBets marketId participantId = 
        if marketId <> 0 then send (fun ch -> GetBets (marketId, participantId, ch))
        else getBetsInAllMarkets participantId