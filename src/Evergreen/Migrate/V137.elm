module Evergreen.Migrate.V137 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Evergreen.V136.EmailAddress
import Evergreen.V136.Id
import Evergreen.V136.Name
import Evergreen.V136.Stripe.PurchaseForm
import Evergreen.V136.Stripe.Stripe
import Evergreen.V136.Types
import Evergreen.V136.Untrusted
import Evergreen.V136.User
import Evergreen.V137.EmailAddress
import Evergreen.V137.Id
import Evergreen.V137.Name
import Evergreen.V137.Stripe.PurchaseForm
import Evergreen.V137.Stripe.Stripe
import Evergreen.V137.Types
import Evergreen.V137.Untrusted
import Evergreen.V137.User
import Lamdera.Migrations exposing (..)


frontendModel : Evergreen.V136.Types.FrontendModel -> ModelMigration Evergreen.V137.Types.FrontendModel Evergreen.V137.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Evergreen.V136.Types.BackendModel -> ModelMigration Evergreen.V137.Types.BackendModel Evergreen.V137.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V136.Types.FrontendMsg -> MsgMigration Evergreen.V137.Types.FrontendMsg Evergreen.V137.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V136.Types.ToBackend -> MsgMigration Evergreen.V137.Types.ToBackend Evergreen.V137.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V136.Types.BackendMsg -> MsgMigration Evergreen.V137.Types.BackendMsg Evergreen.V137.Types.BackendMsg
backendMsg old =
    MsgMigrated ( migrate_Types_BackendMsg old, Cmd.none )


toFrontend : Evergreen.V136.Types.ToFrontend -> MsgMigration Evergreen.V137.Types.ToFrontend Evergreen.V137.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_EmailAddress_EmailAddress : Evergreen.V136.EmailAddress.EmailAddress -> Evergreen.V137.EmailAddress.EmailAddress
migrate_EmailAddress_EmailAddress old =
    case old of
        Evergreen.V136.EmailAddress.EmailAddress p0 ->
            Evergreen.V137.EmailAddress.EmailAddress p0


migrate_Id_Id : (a_old -> a_new) -> Evergreen.V136.Id.Id a_old -> Evergreen.V137.Id.Id a_new
migrate_Id_Id migrate_a old =
    case old of
        Evergreen.V136.Id.Id p0 ->
            Evergreen.V137.Id.Id p0


migrate_Name_Name : Evergreen.V136.Name.Name -> Evergreen.V137.Name.Name
migrate_Name_Name old =
    case old of
        Evergreen.V136.Name.Name p0 ->
            Evergreen.V137.Name.Name p0


migrate_Stripe_PurchaseForm_PurchaseData : Evergreen.V136.Stripe.PurchaseForm.PurchaseData -> Evergreen.V137.Stripe.PurchaseForm.PurchaseData
migrate_Stripe_PurchaseForm_PurchaseData old =
    { billingName = old.billingName |> migrate_Name_Name
    , billingEmail = old.billingEmail |> migrate_EmailAddress_EmailAddress
    }


migrate_Stripe_PurchaseForm_PurchaseFormValidated : Evergreen.V136.Stripe.PurchaseForm.PurchaseFormValidated -> Evergreen.V137.Stripe.PurchaseForm.PurchaseFormValidated
migrate_Stripe_PurchaseForm_PurchaseFormValidated old =
    case old of
        Evergreen.V136.Stripe.PurchaseForm.ImageCreditPurchase p0 ->
            Evergreen.V137.Stripe.PurchaseForm.ImageCreditPurchase (p0 |> migrate_Stripe_PurchaseForm_PurchaseData)

        Evergreen.V136.Stripe.PurchaseForm.ImageLibraryPackagePurchase p0 ->
            Evergreen.V137.Stripe.PurchaseForm.ImageLibraryPackagePurchase (p0 |> migrate_Stripe_PurchaseForm_PurchaseData)


migrate_Stripe_Stripe_Price : Evergreen.V136.Stripe.Stripe.Price -> Evergreen.V137.Stripe.Stripe.Price
migrate_Stripe_Stripe_Price old =
    old


migrate_Stripe_Stripe_PriceData : Evergreen.V136.Stripe.Stripe.PriceData -> Evergreen.V137.Stripe.Stripe.PriceData
migrate_Stripe_Stripe_PriceData old =
    { priceId = old.priceId |> migrate_Id_Id migrate_Stripe_Stripe_PriceId
    , price = old.price |> migrate_Stripe_Stripe_Price
    , productId = old.productId |> migrate_Id_Id migrate_Stripe_Stripe_ProductId
    , isActive = old.isActive
    , createdAt = old.createdAt
    }


migrate_Stripe_Stripe_PriceId : Evergreen.V136.Stripe.Stripe.PriceId -> Evergreen.V137.Stripe.Stripe.PriceId
migrate_Stripe_Stripe_PriceId old =
    case old of
        Evergreen.V136.Stripe.Stripe.PriceId p0 ->
            Evergreen.V137.Stripe.Stripe.PriceId p0


migrate_Stripe_Stripe_ProductId : Evergreen.V136.Stripe.Stripe.ProductId -> Evergreen.V137.Stripe.Stripe.ProductId
migrate_Stripe_Stripe_ProductId old =
    case old of
        Evergreen.V136.Stripe.Stripe.ProductId p0 ->
            Evergreen.V137.Stripe.Stripe.ProductId p0


migrate_Stripe_Stripe_StripeSessionId : Evergreen.V136.Stripe.Stripe.StripeSessionId -> Evergreen.V137.Stripe.Stripe.StripeSessionId
migrate_Stripe_Stripe_StripeSessionId old =
    case old of
        Evergreen.V136.Stripe.Stripe.StripeSessionId p0 ->
            Evergreen.V137.Stripe.Stripe.StripeSessionId p0


migrate_Types_BackendMsg : Evergreen.V136.Types.BackendMsg -> Evergreen.V137.Types.BackendMsg
migrate_Types_BackendMsg old =
    case old of
        Evergreen.V136.Types.GotSlowTick p0 ->
            Evergreen.V137.Types.GotSlowTick p0

        Evergreen.V136.Types.GotFastTick p0 ->
            Evergreen.V137.Types.GotFastTick p0

        Evergreen.V136.Types.OnConnected p0 p1 ->
            Evergreen.V137.Types.OnConnected p0 p1

        Evergreen.V136.Types.GotAtmosphericRandomNumbers p0 ->
            Evergreen.V137.Types.GotAtmosphericRandomNumbers p0

        Evergreen.V136.Types.AutoLogin p0 p1 ->
            Evergreen.V137.Types.AutoLogin p0 (p1 |> migrate_User_LoginData)

        Evergreen.V136.Types.BackendGotTime p0 p1 p2 p3 ->
            Evergreen.V137.Types.BackendGotTime p0 p1 (p2 |> migrate_Types_ToBackend) p3

        Evergreen.V136.Types.SentLoginEmail p0 p1 p2 ->
            Evergreen.V137.Types.SentLoginEmail p0 (p1 |> migrate_EmailAddress_EmailAddress) p2

        Evergreen.V136.Types.AuthenticationConfirmationEmailSent p0 ->
            Evergreen.V137.Types.AuthenticationConfirmationEmailSent p0

        Evergreen.V136.Types.GotPrices p0 ->
            Evergreen.V137.Types.GotPrices (p0 |> Result.map (List.map migrate_Stripe_Stripe_PriceData))

        Evergreen.V136.Types.GotPrices2 p0 p1 ->
            Evergreen.V137.Types.GotPrices2 p0
                (p1 |> Result.map (List.map migrate_Stripe_Stripe_PriceData))

        Evergreen.V136.Types.CreatedCheckoutSession p0 p1 p2 p3 p4 ->
            Evergreen.V137.Types.CreatedCheckoutSession p0
                p1
                (p2 |> migrate_Id_Id migrate_Stripe_Stripe_PriceId)
                (p3 |> migrate_Stripe_PurchaseForm_PurchaseFormValidated)
                (p4 |> Result.map (Tuple.mapFirst (migrate_Id_Id migrate_Stripe_Stripe_StripeSessionId)))

        Evergreen.V136.Types.ExpiredStripeSession p0 p1 ->
            Evergreen.V137.Types.ExpiredStripeSession (p0 |> migrate_Id_Id migrate_Stripe_Stripe_StripeSessionId)
                p1

        Evergreen.V136.Types.ConfirmationEmailSent p0 p1 ->
            Evergreen.V137.Types.ConfirmationEmailSent (p0 |> migrate_Id_Id migrate_Stripe_Stripe_StripeSessionId)
                p1

        Evergreen.V136.Types.ErrorEmailSent p0 ->
            Evergreen.V137.Types.ErrorEmailSent p0

        Evergreen.V136.Types.GotWeatherData p0 p1 ->
            Evergreen.V137.Types.GotWeatherData p0 p1


migrate_Types_ToBackend : Evergreen.V136.Types.ToBackend -> Evergreen.V137.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V136.Types.ToBackendNoOp ->
            Evergreen.V137.Types.ToBackendNoOp

        Evergreen.V136.Types.SubmitFormRequest p0 p1 ->
            Evergreen.V137.Types.SubmitFormRequest (p0 |> migrate_Id_Id migrate_Stripe_Stripe_PriceId)
                (p1 |> migrate_Untrusted_Untrusted migrate_Stripe_PurchaseForm_PurchaseFormValidated)

        Evergreen.V136.Types.CancelPurchaseRequest ->
            Evergreen.V137.Types.CancelPurchaseRequest

        Evergreen.V136.Types.AdminInspect p0 ->
            Evergreen.V137.Types.AdminInspect (p0 |> Maybe.map migrate_User_User)

        Evergreen.V136.Types.GetBackendModel ->
            Evergreen.V137.Types.GetBackendModel

        Evergreen.V136.Types.CheckLoginRequest ->
            Evergreen.V137.Types.CheckLoginRequest

        Evergreen.V136.Types.LoginWithTokenRequest p0 ->
            Evergreen.V137.Types.LoginWithTokenRequest p0

        Evergreen.V136.Types.GetLoginTokenRequest p0 ->
            Evergreen.V137.Types.GetLoginTokenRequest (p0 |> migrate_EmailAddress_EmailAddress)

        Evergreen.V136.Types.LogOutRequest ->
            Evergreen.V137.Types.LogOutRequest Nothing

        Evergreen.V136.Types.RenewPrices ->
            Evergreen.V137.Types.RenewPrices

        Evergreen.V136.Types.AddUser p0 p1 p2 ->
            Evergreen.V137.Types.AddUser p0 p1 p2

        Evergreen.V136.Types.SignInRequest p0 p1 ->
            Evergreen.V137.Types.SignInRequest p0 p1

        Evergreen.V136.Types.SignOutRequest p0 ->
            Evergreen.V137.Types.SignOutRequest p0

        Evergreen.V136.Types.RequestSignup p0 p1 p2 p3 ->
            Evergreen.V137.Types.RequestSignup p0 p1 p2 p3

        Evergreen.V136.Types.GetWeatherData p0 ->
            Evergreen.V137.Types.GetWeatherData p0

        Evergreen.V136.Types.GetKeyValueStore ->
            Evergreen.V137.Types.GetKeyValueStore


migrate_Untrusted_Untrusted : (a_old -> a_new) -> Evergreen.V136.Untrusted.Untrusted a_old -> Evergreen.V137.Untrusted.Untrusted a_new
migrate_Untrusted_Untrusted migrate_a old =
    case old of
        Evergreen.V136.Untrusted.Untrusted p0 ->
            Evergreen.V137.Untrusted.Untrusted (p0 |> migrate_a)


migrate_User_LoginData : Evergreen.V136.User.LoginData -> Evergreen.V137.User.LoginData
migrate_User_LoginData old =
    { username = old.username
    , role = old.role |> migrate_User_Role
    }


migrate_User_Role : Evergreen.V136.User.Role -> Evergreen.V137.User.Role
migrate_User_Role old =
    case old of
        Evergreen.V136.User.AdminRole ->
            Evergreen.V137.User.AdminRole

        Evergreen.V136.User.UserRole ->
            Evergreen.V137.User.UserRole


migrate_User_User : Evergreen.V136.User.User -> Evergreen.V137.User.User
migrate_User_User old =
    { id = old.id
    , realname = old.realname
    , username = old.username
    , email = old.email |> migrate_EmailAddress_EmailAddress
    , created_at = old.created_at
    , updated_at = old.updated_at
    , role = old.role |> migrate_User_Role
    , recentLoginEmails = old.recentLoginEmails
    }
