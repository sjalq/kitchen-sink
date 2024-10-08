module Pages.Admin exposing (Window, view)

import AssocList
import Codec
import Dict
import Element exposing (Element, column, fill, height, px, row, scrollbarY, spacing, text, width)
import Element.Font
import Element.Input as Input
import EmailAddress
import Id exposing (Id)
import KeyValueStore
import MarkdownThemed
import Name
import Stripe.Codec
import Stripe.PurchaseForm
import Stripe.Stripe exposing (Price, PriceData, PriceId, ProductId, StripeSessionId)
import Stripe.View
import Theme
import Types exposing (AdminDisplay(..), BackendModel, FrontendMsg(..), LoadedModel)
import User
import View.Button
import View.Geometry
import View.Utility


type alias Window =
    { width : Int
    , height : Int
    }


view : LoadedModel -> Element FrontendMsg
view model =
    Element.column []
        [ Element.row [ Element.spacing 24, Element.paddingEach { left = 0, right = 0, top = 0, bottom = 24 } ]
            [ View.Button.setAdminDisplay model.adminDisplay ADUser "Users"
            , View.Button.setAdminDisplay model.adminDisplay ADSession "Sessions"
            , View.Button.setAdminDisplay model.adminDisplay ADKeyValues "Key-Value Store"
            , View.Button.setAdminDisplay model.adminDisplay ADStripe "Stripe Data"
            , View.Button.setAdminDisplay model.adminDisplay ADLoadBackend "Load Backend"
            ]
        , case model.backendModel of
            Nothing ->
                text ""

            Just backendModel ->
                case model.adminDisplay of
                    ADUser ->
                        viewUserData model.window backendModel

                    ADSession ->
                        viewSessions model.window backendModel

                    ADKeyValues ->
                        viewKeyValuePairs model.window backendModel

                    ADStripe ->
                        viewStripeData backendModel

                    ADLoadBackend ->
                        viewLoadBackend model
        , viewLoadBackend model
        ]


viewKeyValuePairs : Window -> BackendModel -> Element msg
viewKeyValuePairs window backendModel =
    column
        [ width fill
        , spacing 12
        , height (px <| window.height - 2 * View.Geometry.headerFooterHeight)
        ]
        ([ Element.column Theme.contentAttributes [ content ]
         , Element.el [ Element.Font.bold ] (text "Key-Value Store")
         ]
            ++ List.map viewPair (Dict.toList backendModel.keyValueStore)
        )


content =
    """
### RPC Example

Add key-value pairs to the key-value store by sending this
POST request:

```
curl -X POST -d '{ "key": "foo", "value": "1234" }' \\
   -H 'content-type: application/json' \\
   https://elm-kitchen-sink.lamdera.app/_r/putKeyValuePair
```

Retrieve key-value pairs from the key-value store by sending
the request

```
curl -X POST -d '{ "key": "foo" }' \\
-H 'content-type: application/json' \\
https://elm-kitchen-sink.lamdera.app/_r/getKeyValuePair
```
"""
        |> MarkdownThemed.renderFull


viewPair : ( String, KeyValueStore.KVDatum ) -> Element msg
viewPair ( key, value ) =
    row
        [ width fill
        , spacing 12
        ]
        [ text (key ++ ":")
        , text value.value
        ]


viewUserData : Window -> BackendModel -> Element msg
viewUserData window backendModel =
    column
        [ width fill
        , spacing 12
        ]
        [ viewUserDictionary window backendModel.users ]


viewUserDictionary : Window -> Dict.Dict String User.User -> Element msg
viewUserDictionary window users_ =
    let
        users : List User.User
        users =
            Dict.values users_
    in
    column
        [ width fill
        , Element.height (Element.px <| window.width - 2 * View.Geometry.headerFooterHeight)
        , Element.scrollbarY
        , Element.spacing 24
        ]
        (List.map viewUser users)


viewUser : User.User -> Element msg
viewUser =
    \user ->
        column
            [ width fill
            ]
            [ text ("realname: " ++ user.fullname)
            , text ("username: " ++ user.username)
            , text ("email: " ++ EmailAddress.toString user.email)
            , text ("id: " ++ user.id)
            ]


viewSessions : Window -> BackendModel -> Element msg
viewSessions window backendModel =
    column
        [ width fill
        , spacing 12
        ]
        (backendModel.sessionDict
            |> AssocList.toList
            |> List.map viewSession
        )


viewSession : ( String, String ) -> Element msg
viewSession ( key, value ) =
    column
        [ width fill
        ]
        [ text key
        , text value
        ]


viewStripeData : BackendModel -> Element msg
viewStripeData backendModel =
    column
        [ width fill
        , spacing 40
        ]
        [ viewOrders backendModel.orders
        , viewPendingOrder backendModel.pendingOrder
        , viewExpiredOrdersPretty backendModel.expiredOrders
        , viewPricesPretty backendModel.prices
        ]


viewPrices : AssocList.Dict (Id ProductId) Stripe.Codec.Price2 -> Element msg
viewPrices prices =
    column
        [ width fill
        ]
        [ text "Prices"
        , Codec.encodeToString 2 (Stripe.Codec.assocListCodec Stripe.Codec.price2Codec) prices |> text
        ]


viewPricesPretty : AssocList.Dict (Id ProductId) Stripe.Codec.Price2 -> Element msg
viewPricesPretty prices =
    column
        [ width fill
        ]
        (Element.el [ Element.Font.bold ] (text "Prices")
            :: List.map Stripe.View.viewEntry (prices |> AssocList.toList)
        )


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


viewExpiredOrdersPretty : AssocList.Dict (Id StripeSessionId) Stripe.Codec.PendingOrder -> Element msg
viewExpiredOrdersPretty expiredOrders =
    let
        orders : List ( Id StripeSessionId, Stripe.Codec.PendingOrder )
        orders =
            expiredOrders
                |> AssocList.toList

        viewOrder : ( Id StripeSessionId, Stripe.Codec.PendingOrder ) -> Element msg
        viewOrder ( id, order ) =
            column
                [ width fill
                ]
                [ text ("name: " ++ (order.form |> Stripe.PurchaseForm.getPurchaseData |> .billingName |> Name.nameToString))
                , text ("email: " ++ (order.form |> Stripe.PurchaseForm.getPurchaseData |> .billingEmail |> EmailAddress.toString))
                , text ("date-time: " ++ (order |> .submitTime |> View.Utility.toUtcString))
                , text ("id: " ++ Id.toString id)
                , text ("priceId: " ++ Id.toString order.priceId)
                , text ("sessionId: " ++ order.sessionId)
                ]
    in
    column
        [ width fill
        , Element.spacing 24
        ]
        (Element.el [ Element.Font.bold ] (text "Expired Orders") :: List.map viewOrder orders)


viewLoadBackend : LoadedModel -> Element FrontendMsg
viewLoadBackend loadedModel =
    column
        [ width fill
        , spacing 24
        ]
        [ Input.text []
            { onChange = UpdateRemoteUrl
            , text = loadedModel.modelUrl
            , placeholder = Just (Input.placeholder [] (text "Enter remote URL"))
            , label = Input.labelAbove [] (text "Remote URL")
            }
        , Input.text []
            { onChange = UpdateModelSecret
            , text = loadedModel.modelSecret
            , placeholder = Just (Input.placeholder [] (text "Enter model secret"))
            , label = Input.labelAbove [] (text "Model Secret")
            }
        , View.Button.button (FELoadBackendModel loadedModel.modelUrl loadedModel.modelSecret) "Load Backend Model"
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
