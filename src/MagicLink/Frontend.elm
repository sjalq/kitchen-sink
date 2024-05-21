module MagicLink.Frontend exposing
    ( enterEmail
    , handleRegistrationError
    , handleSignInError
    , signInWithCode
    , signInWithTokenResponse
    , signOut
    , submitEmailForSignin
    , submitEmailForToken
    , submitSignUp
    , userRegistered
    )

import Dict
import EmailAddress
import Helper
import KeyValueStore
import Lamdera
import MagicLink.LoginForm
import MagicLink.Types exposing (LoginForm(..))
import Route exposing (Route(..))
import Stripe.PurchaseForm as PurchaseForm
    exposing
        ( PressedSubmit(..)
        , PurchaseForm
        , PurchaseFormValidated(..)
        , SubmitStatus(..)
        )
import Types
    exposing
        ( AdminDisplay(..)
        , BackendModel
        , FrontendModel(..)
        , FrontendMsg(..)
        , LoadedModel
        , LoadingModel
        , SignInState(..)
        , ToBackend(..)
        , ToFrontend(..)
        )
import User


submitEmailForSignin model =
    case model.loginForm of
        EnterEmail loginForm ->
            case EmailAddress.fromString loginForm.email of
                Just email ->
                    let
                        model2 =
                            { model | loginForm = EnterLoginCode { sentTo = email, loginCode = "", attempts = Dict.empty } }
                    in
                    ( model2, Helper.trigger <| AuthSigninRequested { methodId = "EmailMagicLink", email = Just loginForm.email } )

                Nothing ->
                    ( { model | loginForm = EnterEmail { loginForm | pressedSubmitEmail = True } }, Cmd.none )

        EnterLoginCode _ ->
            ( model, Cmd.none )


enterEmail model email =
    case model.loginForm of
        EnterEmail loginForm_ ->
            let
                loginForm =
                    { loginForm_ | email = email }
            in
            ( { model | loginForm = EnterEmail loginForm }, Cmd.none )

        EnterLoginCode loginCode_ ->
            -- TODO: complete this
            --  EnterLoginCode{ sentTo : EmailAddress, loginCode : String, attempts : Dict Int LoginCodeStatus }
            ( model, Cmd.none )


handleRegistrationError model str =
    ( { model | signInStatus = MagicLink.Types.ErrorNotRegistered str }, Cmd.none )


handleSignInError model message =
    ( { model | loginErrorMessage = Just message, signInStatus = MagicLink.Types.ErrorNotRegistered message }, Cmd.none )


signInWithTokenResponse model result =
    case result of
        Err _ ->
            ( { model | loginErrorMessage = Just "Invalid login code" }, Cmd.none )

        Ok signInData ->
            let
                adminCommand =
                    case signInData.role of
                        User.AdminRole ->
                            Lamdera.sendToBackend GetBackendModel

                        User.UserRole ->
                            Cmd.none
            in
            ( { model | currentUserData = Just signInData, route = HomepageRoute }, adminCommand )


signOut model =
    ( { model
        | showTooltip = False
        , form =
            { submitStatus = NotSubmitted NotPressedSubmit
            , name = ""
            , billingEmail = ""
            , country = ""
            }

        -- TOKEN
        , loginForm = MagicLink.LoginForm.init
        , loginErrorMessage = Nothing
        , signInStatus = MagicLink.Types.NotSignedIn

        -- USER
        , currentUserData = Nothing
        , currentUser = Nothing
        , realname = ""
        , username = ""
        , email = ""
        , password = ""
        , passwordConfirmation = ""
        , signInState = SignedOut

        -- ADMIN
        , adminDisplay = ADUser

        --
        , backendModel = Nothing
        , message = ""

        -- EXAMPLES
        , language = "en-US"
        , inputCity = ""
        , weatherData = Nothing

        -- DATA
        , currentKVPair = Nothing
        , inputKey = ""
        , inputValue = ""
        , inputFilterData = ""
        , kvViewType = KeyValueStore.KVVSummary
        , kvVerbosity = KeyValueStore.KVQuiet
      }
    , Lamdera.sendToBackend (SignOutRequest model.currentUserData)
    )


submitSignUp model =
    ( model, Lamdera.sendToBackend (AddUser model.realname model.username model.email) )


userRegistered model user =
    ( { model
        | currentUser = Just user
        , signInStatus = MagicLink.Types.SuccessfulRegistration user.username (EmailAddress.toString user.email)
      }
    , Cmd.none
    )


submitEmailForToken : LoadedModel -> ( LoadedModel, Cmd FrontendMsg )
submitEmailForToken model =
    case model.loginForm of
        EnterEmail loginForm ->
            case EmailAddress.fromString loginForm.email of
                Just email ->
                    ( { model | loginForm = EnterLoginCode { sentTo = email, loginCode = "", attempts = Dict.empty } }
                    , Lamdera.sendToBackend (RequestMagicToken email)
                    )

                Nothing ->
                    ( { model | loginForm = EnterEmail { loginForm | pressedSubmitEmail = True } }, Cmd.none )

        EnterLoginCode _ ->
            -- TODO: handle EnterLoginCode with parameter loginCode instead of _ ??
            ( model, Cmd.none )



-- HELPERS


signInWithCode model signInCode =
    case model.loginForm of
        MagicLink.Types.EnterEmail _ ->
            ( model, Cmd.none )

        EnterLoginCode enterLoginCode ->
            case MagicLink.LoginForm.validateLoginCode signInCode of
                Ok loginCode ->
                    if Dict.member loginCode enterLoginCode.attempts then
                        ( { model
                            | loginForm =
                                EnterLoginCode
                                    { enterLoginCode | loginCode = String.left MagicLink.LoginForm.loginCodeLength signInCode }
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | loginForm =
                                EnterLoginCode
                                    { enterLoginCode
                                        | loginCode = String.left MagicLink.LoginForm.loginCodeLength signInCode
                                        , attempts =
                                            Dict.insert loginCode MagicLink.Types.Checking enterLoginCode.attempts
                                    }
                          }
                        , Lamdera.sendToBackend (SigInWithTokenRequest loginCode)
                        )

                Err _ ->
                    ( { model | loginForm = EnterLoginCode { enterLoginCode | loginCode = String.left MagicLink.LoginForm.loginCodeLength signInCode } }
                    , Cmd.none
                    )
