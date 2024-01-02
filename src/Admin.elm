module Admin exposing (..)

import AssocList
import Codec
import Element exposing (..)
import Element.Font
import Id exposing (Id)
import Lamdera
import Stripe.Codec
import Stripe.Stripe as Stripe exposing (Price, PriceData, PriceId, ProductId, StripeSessionId)
import Types exposing (..)


view : LoadedModel -> Element msg
view model =
    case model.backendModel of
        Just backendModel ->
            viewStripeData backendModel

        Nothing ->
            text "loading .."


viewStripeData : BackendModel -> Element msg
viewStripeData backendModel =
    -- type alias BackendModel =
    --     { orders : AssocList.Dict (Id StripeSessionId) Order
    --     , pendingOrder : AssocList.Dict (Id StripeSessionId) PendingOrder
    --     , expiredOrders : AssocList.Dict (Id StripeSessionId) PendingOrder
    --     , prices : AssocList.Dict (Id ProductId) Price2
    --     , time : Time.Posix
    --     , ticketsEnabled : TicketsEnabled
    --     }
    column
        [ width fill
        , spacing 40
        ]
        [ Element.el [ Element.Font.bold, Element.Font.size 18 ] (text "Stripe Data")
        , viewOrders backendModel.orders
        , viewPendingOrder backendModel.pendingOrder
        , viewExpiredOrders backendModel.expiredOrders
        , viewPrices backendModel.prices
        ]


viewPrices : AssocList.Dict (Id ProductId) Stripe.Codec.Price2 -> Element msg
viewPrices prices =
    column
        [ width fill
        ]
        [ text "Prices"
        , Codec.encodeToString 2 (Stripe.Codec.assocListCodec Stripe.Codec.price2Codec) prices |> text
        ]


viewOrders : AssocList.Dict (Id StripeSessionId) Stripe.Codec.Order -> Element msg
viewOrders orders =
    column
        [ width fill
        ]
        [ text "Orders"
        , Codec.encodeToString 2 (Stripe.Codec.assocListCodec Stripe.Codec.orderCodec) orders |> text
        ]


viewPendingOrder : AssocList.Dict (Id StripeSessionId) Stripe.Codec.PendingOrder -> Element msg
viewPendingOrder pendingOrders =
    column
        [ width fill
        ]
        [ text "Pending Orders"
        , Codec.encodeToString 2 (Stripe.Codec.assocListCodec Stripe.Codec.pendingOrderCodec) pendingOrders |> text
        ]


viewExpiredOrders : AssocList.Dict (Id StripeSessionId) Stripe.Codec.PendingOrder -> Element msg
viewExpiredOrders expiredOrders =
    column
        [ width fill
        ]
        [ text "Expired Orders"
        , Codec.encodeToString 2 (Stripe.Codec.assocListCodec Stripe.Codec.pendingOrderCodec) expiredOrders |> text
        ]


loadProdBackend : Cmd msg
loadProdBackend =
    let
        x =
            1

        -- pass =
        --     Env.adminPassword
    in
    Cmd.none
