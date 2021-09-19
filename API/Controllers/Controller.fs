namespace Controllers

open Microsoft.AspNetCore.Mvc
open Engine
open Types

[<AutoOpen>]
module private Helpers = 
    let ok payload = payload |> OkObjectResult :> IActionResult
    let ko (payload: obj) = payload |> BadRequestObjectResult :> IActionResult
    let inline go send recv = async {
        let! result = send()
        return  
            match result with
            | Ok rsp -> recv rsp |> ok
            | Error err -> err |> ko
    }

[<ApiController>]
[<Route("[controller]")>]
type BmeController (engine : Engine) =
    inherit ControllerBase()
    
    [<HttpPost>][<Route("[action]")>]
    member x.AddMarket() = engine.AddMarket()

    [<HttpPost>][<Route("[action]")>]
    member x.SellABet([<FromQuery>] req) = 
        let send () = engine.Sell req
        let recv id = Sell.composeFullId req.MarketId id
        go send recv

    [<HttpPost>][<Route("[action]")>]
    member x.BuyABet([<FromQuery>] req) = 
        let send () = engine.Buy req
        let recv (filled, odds) = 
            {
                Status =
                    if filled >= req.Amount then "Fully Accepted"
                    elif filled > 0M then "Partially Accepted"
                    else "Rejected"
                Amount = filled
                Odds = odds
            }
        go send recv


    [<HttpGet>][<Route("[action]")>]
    member x.ShowOrders([<FromQuery>]marketId: int) = 
        let send () = engine.GetOrders marketId
        let recv (orders: (int * Sell) list) = 
            orders |> List.map (fun (id, sell) -> $"Order {Sell.composeFullId marketId id}, {sell.Amount - sell.Filled:C} remaining of {sell.Amount:C} at {sell.Odds:C2}")
        go send recv
    
    [<HttpDelete>][<Route("[action]")>]
    member x.RemoveSell([<FromQuery>]sellId: string) = 
        let send () = engine.RemoveSell sellId
        let recv response = 
            match response with
            | RemovalAccepted -> "Removal accepted"
            | UnableToRemove NotFound -> "Unable to remove (not found)"
            | UnableToRemove AlreadyFilled -> "Unable to remove (already filled)"
        go send recv

    [<HttpGet>][<Route("[action]")>]
    member x.ShowBets([<FromQuery>]req: ShowBetsRequest) = 
        let send () = engine.GetBets req.MarketId req.ParticipantId
        let recv (response: ShowBetsResponse list) = 
            response |> List.mapi (fun i rsp -> $"Bet {i + 1} - Market {rsp.MarketId}: Seller {rsp.SellerId} must pay bettor {rsp.BettorId} a payout of {rsp.Amount * rsp.Odds:C} ({rsp.Amount:C} invested at {rsp.Odds:C2})" )
        go send recv
