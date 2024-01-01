module Evergreen.Migrate.V32 exposing (..)

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

import AssocList
import Evergreen.V30.EmailAddress
import Evergreen.V30.Id
import Evergreen.V30.Name
import Evergreen.V30.Postmark
import Evergreen.V30.Route
import Evergreen.V30.Stripe.PurchaseForm
import Evergreen.V30.Stripe.Stripe
import Evergreen.V30.Stripe.Tickets
import Evergreen.V30.Types
import Evergreen.V32.Email
import Evergreen.V32.EmailAddress
import Evergreen.V32.Id
import Evergreen.V32.Name
import Evergreen.V32.Route
import Evergreen.V32.Stripe.Codec
import Evergreen.V32.Stripe.PurchaseForm
import Evergreen.V32.Stripe.Stripe
import Evergreen.V32.Stripe.Tickets
import Evergreen.V32.Types
import Lamdera.Migrations exposing (..)
import Maybe


frontendModel : Evergreen.V30.Types.FrontendModel -> ModelMigration Evergreen.V32.Types.FrontendModel Evergreen.V32.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Evergreen.V30.Types.BackendModel -> ModelMigration Evergreen.V32.Types.BackendModel Evergreen.V32.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V30.Types.FrontendMsg -> MsgMigration Evergreen.V32.Types.FrontendMsg Evergreen.V32.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V30.Types.ToBackend -> MsgMigration Evergreen.V32.Types.ToBackend Evergreen.V32.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V30.Types.BackendMsg -> MsgMigration Evergreen.V32.Types.BackendMsg Evergreen.V32.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V30.Types.ToFrontend -> MsgMigration Evergreen.V32.Types.ToFrontend Evergreen.V32.Types.FrontendMsg
toFrontend old =
    MsgUnchanged
