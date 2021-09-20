module BettingMatchingEngine

open Xunit
open Types
open Engine
open Controllers
open Microsoft.AspNetCore.Mvc


[<AutoOpen>]
module private Utilities = 
    let checkOrders (controller: BmeController) market expected = async {
        let! ret = controller.ShowOrders market
        let result = Assert.IsType<OkObjectResult>(ret)
        let returned = Assert.IsType<string list>(result.Value)
        Assert.Equal<string>(expected, returned)
    }

    let checkSellRemove (controller: BmeController) sellId expected = async {
        let! ret = controller.RemoveSell sellId
        let result = Assert.IsType<OkObjectResult>(ret)
        let returned = Assert.IsType<string>(result.Value)
        Assert.Equal(expected, returned)
    }

    let checkBets (controller: BmeController) marketId bettorId expected = async {
        let! ret = controller.ShowBets(marketId, bettorId)
        let result = Assert.IsType<OkObjectResult>(ret)
        let returned = Assert.IsType<string list>(result.Value)
        Assert.Equal<string>(expected, returned)
    }

    let checkExposure (controller: BmeController) marketId sellerId expected = async {
        let! ret = controller.ShowExposure(marketId, sellerId)
        let result = Assert.IsType<OkObjectResult>(ret)
        let returned = Assert.IsType<decimal>(result.Value)
        Assert.Equal(expected, returned)
    }

[<Fact>]
let ``01 - can add markets`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)
    let! arr = [| for i in 0..99 do controller.AddMarket() |] |> Async.Parallel
    Assert.Equal<int>([|1..100|], arr)
}

[<Fact>]
let ``02 - can recognise missing market`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let sell = {
        Odds = 1.1M
        Amount = 1000M
        ParticipantId = 1
        MarketId = 1
    }

    let! result = controller.SellABet sell
    let ret = Assert.IsType<BadRequestObjectResult>(result)
    let msg = Assert.IsType<string>(ret.Value)
    Assert.Equal("Market 1 does not exist", msg)
}

[<Fact>]
let ``03 - can take sell orders`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    for i in 0..9 do controller.AddMarket() |> ignore

    let! results = 
        [|
        for i in 0..99 do 
            let sell = {
                Odds = 1.1M
                Amount = 1000M
                ParticipantId = 1
                MarketId = (i / 10) + 1
            }
            controller.SellABet sell
        |] |> Async.Parallel

    let results = results |> Seq.cast<OkObjectResult> |> Seq.map (fun ok -> ok.Value :?> string) |> Set.ofSeq
    let expected = seq { for i in 0..99 do $"{(i / 10) + 1}-{(i % 10) + 1}" } |> Set.ofSeq
    Assert.Equal<string>(expected, results)
}

[<Fact>]
let ``04 - can match scenario 1`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market = controller.AddMarket()
    let! sell1 = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market }
    let! sell2 = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 1; MarketId = market }
    let! ret = controller.BuyABet { Amount = 3000M; ParticipantId = 2; MarketId = market }
    
    let result = Assert.IsType<OkObjectResult>(ret)
    let payload = Assert.IsType<BuyResponse>(result.Value)
    Assert.Equal({ Status = "Fully Accepted"; Odds = 1.1M; Amount = 3000M }, payload)
}

[<Fact>]
let ``05 - can match scenario 2`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market = controller.AddMarket()
    let! sell1 = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market }
    let! sell2 = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 1; MarketId = market }
    let! ret = controller.BuyABet { Amount = 5000M; ParticipantId = 2; MarketId = market }
    
    let result = Assert.IsType<OkObjectResult>(ret)
    let payload = Assert.IsType<BuyResponse>(result.Value)
    Assert.Equal({ Status = "Partially Accepted"; Odds = 1.1M; Amount = 4000M }, payload)
}

[<Fact>]
let ``06 - can get scenario 1 sell orders`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market = controller.AddMarket()

    let check = checkOrders controller market

    do! check []

    let! _ = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market }
    do! check ["Order 1-1, $4,000.00 remaining of $4,000.00 at $1.10"]

    let! _ = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 1; MarketId = market }
    do! check ["Order 1-1, $4,000.00 remaining of $4,000.00 at $1.10"; "Order 1-2, $5,000.00 remaining of $5,000.00 at $1.05"]

    let! _ = controller.BuyABet { Amount = 3000M; ParticipantId = 2; MarketId = market }
    do! check ["Order 1-1, $1,000.00 remaining of $4,000.00 at $1.10"; "Order 1-2, $5,000.00 remaining of $5,000.00 at $1.05"]

}

[<Fact>]
let ``07 - can get scenario 2 sell orders`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market = controller.AddMarket()
    let check = checkOrders controller market

    do! check []

    let! _ = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market }
    do! check ["Order 1-1, $4,000.00 remaining of $4,000.00 at $1.10"]

    let! _ = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 1; MarketId = market }
    do! check ["Order 1-1, $4,000.00 remaining of $4,000.00 at $1.10"; "Order 1-2, $5,000.00 remaining of $5,000.00 at $1.05"]

    let! _ = controller.BuyABet { Amount = 5000M; ParticipantId = 2; MarketId = market }
    do! check ["Order 1-2, $5,000.00 remaining of $5,000.00 at $1.05"]

}

[<Fact>]
let ``08 - can remove sells`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market = controller.AddMarket()

    let checkRemove = checkSellRemove controller
    let checkOrders = checkOrders controller market

    do! checkRemove "1-1" "Unable to remove (not found)"

    let! sell1 = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market }
    let sell1Id = (sell1 :?> OkObjectResult).Value :?> string
    let! sell2 = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 1; MarketId = market }
    let sell2Id = (sell2 :?> OkObjectResult).Value :?> string

    do! checkOrders ["Order 1-1, $4,000.00 remaining of $4,000.00 at $1.10"; "Order 1-2, $5,000.00 remaining of $5,000.00 at $1.05"]
    
    let! _ = controller.BuyABet { Amount = 3000M; ParticipantId = 2; MarketId = market }
    do! checkOrders ["Order 1-1, $1,000.00 remaining of $4,000.00 at $1.10"; "Order 1-2, $5,000.00 remaining of $5,000.00 at $1.05"]


    do! checkRemove "1-3" "Unable to remove (not found)"
    do! checkOrders ["Order 1-1, $1,000.00 remaining of $4,000.00 at $1.10"; "Order 1-2, $5,000.00 remaining of $5,000.00 at $1.05"]


    do! checkRemove sell1Id "Unable to remove (already filled)"
    do! checkOrders ["Order 1-1, $1,000.00 remaining of $4,000.00 at $1.10"; "Order 1-2, $5,000.00 remaining of $5,000.00 at $1.05"]
    
    do! checkRemove sell2Id "Removal accepted"
    do! checkOrders ["Order 1-1, $1,000.00 remaining of $4,000.00 at $1.10"]
}

[<Fact>]
let ``09 - can get bets for one bettor in one market`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market = controller.AddMarket()

    let! _ = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market }
    let! _ = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 2; MarketId = market }

    let! _ = controller.BuyABet { Amount = 3000M; ParticipantId = 3; MarketId = market }
    let! _ = controller.BuyABet { Amount = 6000M; ParticipantId = 7; MarketId = market }
    let! _ = controller.BuyABet { Amount = 6000M; ParticipantId = 3; MarketId = market }

    let check = checkBets controller

    do! [
        "Bet 1 - Market 1: Seller 1 must pay bettor 3 a payout of $3,300.00 ($3,000.00 invested at $1.10)"
        "Bet 2 - Market 1: Seller 2 must pay bettor 3 a payout of $5,250.00 ($5,000.00 invested at $1.05)"
        ] 
        |> check market 3
}

[<Fact>]
let ``10 - can get bets for all bettors in one market`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market = controller.AddMarket()

    let! _ = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market }
    let! _ = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 2; MarketId = market }

    let! _ = controller.BuyABet { Amount = 3000M; ParticipantId = 3; MarketId = market }
    let! _ = controller.BuyABet { Amount = 6000M; ParticipantId = 4; MarketId = market }
    let! _ = controller.BuyABet { Amount = 6000M; ParticipantId = 5; MarketId = market }

    let check = checkBets controller

    do! [
        "Bet 1 - Market 1: Seller 1 must pay bettor 3 a payout of $3,300.00 ($3,000.00 invested at $1.10)"
        "Bet 2 - Market 1: Seller 1 must pay bettor 4 a payout of $1,100.00 ($1,000.00 invested at $1.10)"
        "Bet 3 - Market 1: Seller 2 must pay bettor 5 a payout of $5,250.00 ($5,000.00 invested at $1.05)"
        ] 
        |> check market 0
}

[<Fact>]
let ``11 - can get bets for one bettor in all markets`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market1 = controller.AddMarket()
    let! market2 = controller.AddMarket()
    let! market3 = controller.AddMarket()

    let! _ = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market3 }
    let! _ = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 2; MarketId = market1 }
    let! _ = controller.SellABet { Odds = 1.2M; Amount = 3000M; ParticipantId = 3; MarketId = market2 }

    let! _ = controller.BuyABet { Amount = 3000M; ParticipantId = 4; MarketId = market1 }
    let! _ = controller.BuyABet { Amount = 6000M; ParticipantId = 5; MarketId = market3 }
    let! _ = controller.BuyABet { Amount = 6000M; ParticipantId = 4; MarketId = market2 }

    let check = checkBets controller

    do! [
        "Bet 1 - Market 1: Seller 2 must pay bettor 4 a payout of $3,150.00 ($3,000.00 invested at $1.05)"
        "Bet 2 - Market 2: Seller 3 must pay bettor 4 a payout of $3,600.00 ($3,000.00 invested at $1.20)"
        ] 
        |> check 0 4
}

[<Fact>]
let ``12 - can get bets for all bettors in all markets`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market1 = controller.AddMarket()
    let! market2 = controller.AddMarket()
    let! market3 = controller.AddMarket()

    let! _ = controller.SellABet { Odds = 1.1M; Amount = 4000M; ParticipantId = 1; MarketId = market3 }
    let! _ = controller.SellABet { Odds = 1.05M; Amount = 5000M; ParticipantId = 2; MarketId = market1 }
    let! _ = controller.SellABet { Odds = 1.2M; Amount = 3000M; ParticipantId = 3; MarketId = market2 }

    let! _ = controller.BuyABet { Amount = 3000M; ParticipantId = 4; MarketId = market1 }
    let! _ = controller.BuyABet { Amount = 6000M; ParticipantId = 5; MarketId = market3 }
    let! _ = controller.BuyABet { Amount = 6000M; ParticipantId = 4; MarketId = market2 }

    let check = checkBets controller

    do! [
        "Bet 1 - Market 1: Seller 2 must pay bettor 4 a payout of $3,150.00 ($3,000.00 invested at $1.05)"
        "Bet 2 - Market 2: Seller 3 must pay bettor 4 a payout of $3,600.00 ($3,000.00 invested at $1.20)"
        "Bet 3 - Market 3: Seller 1 must pay bettor 5 a payout of $4,400.00 ($4,000.00 invested at $1.10)"
        ] 
        |> check 0 0
}

[<Fact>]
let ``13 - can account for seller exposure`` () = async {
    let engine = Engine()
    let controller = BmeController(engine)

    let! market = controller.AddMarket()

    let! _ = controller.SellABet { Odds = 1.1M; Amount = 1000M; ParticipantId = 1; MarketId = market }
    let! _ = controller.SellABet { Odds = 1.05M; Amount = 1000M; ParticipantId = 1; MarketId = market }

    let! _ = controller.SellABet { Odds = 1.2M; Amount = 1000M; ParticipantId = 2; MarketId = market }
    let! _ = controller.SellABet { Odds = 1.25M; Amount = 1000M; ParticipantId = 2; MarketId = market }

    let! _ = controller.BuyABet { Amount = 100M; ParticipantId = 3; MarketId = market }
    let! _ = controller.BuyABet { Amount = 200M; ParticipantId = 4; MarketId = market }
    let! _ = controller.BuyABet { Amount = 300M; ParticipantId = 5; MarketId = market }
    let! _ = controller.BuyABet { Amount = 400M; ParticipantId = 6; MarketId = market }
    let! _ = controller.BuyABet { Amount = 500M; ParticipantId = 7; MarketId = market }

    let res1 = controller.ShowExposure(1, 1)
    let res2 = controller.ShowExposure(1, 2)

    let check = checkExposure controller 1

    do! check 1 150M
    do! check 2 450M
}
