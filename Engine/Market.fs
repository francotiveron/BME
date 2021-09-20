module Market

#nowarn "26"

open Types
open System.Collections.Generic
open System.Linq

type Market(id:int) = 
    let mutable autoIncSellId = 0
    let sells = Dictionary<int, Sell>()
    let unfilledSellsByOdds = SortedDictionary<decimal, ResizeArray<int>>({ new IComparer<decimal> with member _.Compare(d1, d2) = d2.CompareTo(d1) })
    let buys = ResizeArray<Buy>()
    let buysByBettor = Dictionary<int, ResizeArray<int>>()
    let exposures = Dictionary<int, decimal>()

    let toBetResponses bettorId indexes =
        let toBetResponse bettorId buy =
            buy.Sells |> Seq.map (fun (sellerId, amount) -> { MarketId = id; BettorId = bettorId; SellerId = sellerId; Amount = amount; Odds = buy.Odds})

        indexes 
        |> Seq.map (fun index -> buys.[index])
        |> Seq.collect (toBetResponse bettorId)

    let agent =
        MailboxProcessor.Start (fun inbox ->
            let rec loop() = async {
                try
                    let! m = inbox.Receive()
            
                    match m with
                    | Sell (req, ch) -> 
                        if req.Amount > 0M then
                            autoIncSellId <- autoIncSellId + 1
                            let sell = { Odds = req.Odds; Amount = req.Amount; Filled = 0M; SellerId = req.ParticipantId }
                            sells.Add(autoIncSellId, sell)
                            if unfilledSellsByOdds.ContainsKey(sell.Odds) |> not then
                                unfilledSellsByOdds.[sell.Odds] <- ResizeArray()
                            unfilledSellsByOdds.[sell.Odds].Add autoIncSellId
                            let potentialLoss = (sell.Odds - 1M) * sell.Amount
                            match exposures.TryGetValue(sell.SellerId) with
                            | true, exposure -> exposures.[sell.SellerId] <- exposure + potentialLoss
                            | false, _ -> exposures.[sell.SellerId] <- potentialLoss
                            Ok autoIncSellId
                        else
                            Error "Sell amount must be greater than 0"
                        |> ch.Reply

                    | Buy (req, ch) -> 
                        match Seq.tryHead unfilledSellsByOdds with
                        | Some kvp -> 
                            let odds = kvp.Key
                            let sellIds = kvp.Value
                            let buy = { Odds = odds; Amount = req.Amount; Filled = 0M; Sells = ResizeArray<int * decimal>() }
                            while buy.Filled < buy.Amount && sellIds.Count > 0 do
                                let sellId = sellIds.First()
                                let sell = sells.[sellId]
                                let available = sell.Amount - sell.Filled
                                let wanted = buy.Amount - buy.Filled
                                if wanted < available then
                                    buy.Filled <- buy.Amount
                                    sells.[sellId].Filled <- sells.[sellId].Filled + wanted
                                    buy.Sells.Add (sell.SellerId, wanted)
                                else
                                    buy.Filled <- buy.Filled + available
                                    sellIds.Remove sellId |> ignore
                                    sells.Remove sellId |> ignore
                                    buy.Sells.Add (sell.SellerId, available)

                            if sellIds.Count = 0 then unfilledSellsByOdds.Remove odds |> ignore

                            buys.Add buy
                            if buysByBettor.ContainsKey(req.ParticipantId) |> not then
                                buysByBettor.[req.ParticipantId] <- ResizeArray()
                            buysByBettor.[req.ParticipantId].Add (buys.Count - 1)

                            (buy.Filled, buy.Odds) |> Ok |> ch.Reply        
                        | None -> Ok (0M, 0M) |> ch.Reply

                    | GetOrders (_marketId, ch) ->
                        unfilledSellsByOdds
                        |> Seq.collect (fun kvp -> kvp.Value)
                        |> Seq.map (fun id -> id, sells.[id])
                        |> Seq.toList
                        |> Ok |> ch.Reply

                    | RemoveSell (_marketId, sellId, ch) ->
                        match sells.TryGetValue sellId with
                        | true, sell ->
                            if sell.Filled > 0M then
                                UnableToRemove AlreadyFilled
                            else
                                sells.Remove sellId |> ignore
                                unfilledSellsByOdds.[sell.Odds].Remove sellId |> ignore
                                if unfilledSellsByOdds.[sell.Odds].Count = 0 then unfilledSellsByOdds.Remove sell.Odds |> ignore
                                RemovalAccepted
                        | false, _ -> UnableToRemove NotFound                        
                        |> Ok |> ch.Reply

                    | GetBets (_marketId, bettorId, ch) ->
                        if bettorId <> 0 then
                            match buysByBettor.TryGetValue bettorId with
                            | true, bettorBuys ->
                                bettorBuys
                                |> toBetResponses bettorId
                                |> Seq.toList
                            | false, _ -> []
                            |> Ok
                        else
                            buysByBettor
                            |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
                            |> Seq.collect (fun (bettorId, bettorBuys) -> toBetResponses bettorId bettorBuys)
                            |> Seq.toList
                            |> Ok
                        |> ch.Reply

                    | GetExposure (_marketId, sellerId, ch) ->
                        match exposures.TryGetValue sellerId with
                        | true, exposure -> exposure
                        | false, _ -> 0M
                        |> Ok
                        |> ch.Reply

                    | _ -> failwith $"Invalid/Unhandled Message {m}"

                with _ -> () //Todo
                return! loop()
            }
            loop()
        )
    
    let send messageBuilder = agent.PostAndAsyncReply(fun ch -> messageBuilder ch)
    
    member _.Sell req = send (fun ch -> Sell (req, ch))

    member _.Buy req = send (fun ch -> Buy (req, ch))

    member _.GetOrders() = send (fun ch -> GetOrders (id, ch))

    member _.RemoveSell sellId = send (fun ch -> RemoveSell (id, sellId, ch))
    
    member _.GetBets bettorId = send (fun ch -> GetBets (id, bettorId, ch))

    member _.GetExposure sellerId = send (fun ch -> GetExposure (id, sellerId, ch))
