module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (Html, text, div, h1, img, ul, li, a, b, p, table, th, td, tr)
import Html.Attributes exposing (src, href, style)
import Debug exposing (log)
import Routes exposing (fromUrl, Route(..))
import Url.Parser exposing (string)
import Array exposing (..)

---- MODEL ----

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentPage : Route
    , accounts : Array Account
    , accountState : AccountsState
    }

type AccountsState =
  AccountsNotLoaded
  | AccountsLoading
  | AccountsLoadNothing
  | AccountsLoadFailed
  | AccountsLoadSuccess

type AccountType = 
  DemandDeposit
  | AccountAnalysis

accountTypeToString : AccountType -> String
accountTypeToString accountType =
  case accountType of
    DemandDeposit -> "Demand Deposit"
    AccountAnalysis -> "Account Analysis"

type alias Account =
  { id : String
  , nickname : String
  , accountType: AccountType }


-- init : ( Model, Cmd Msg )
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        msg = log "init url" url
    in
    (Model key url Home (Array.fromList []) AccountsNotLoaded, Cmd.none)



---- UPDATE ----


-- type Msg
    -- = NoOp

type Msg =
    LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
            let
                mgs1 = log "url internal" url
                msg2 = log "parsed" (fromUrl url)
            in
            ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
            let
                mgs2 = log "href external" href
            in
            ( model, Nav.load href )

    UrlChanged url ->
        let
            msg3 = log "url changed" url.path
        in
        ( { model | currentPage = fromUrl url, url = url }
        , Cmd.none
        )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

---- VIEW ----


view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ text "The current URL is: "
      , b [] [ text (Url.toString model.url) ]
      , ul []
          [ viewLink "/"
          , viewLink "/statements"
          , viewLink "/downloads"
          , viewLink "/statements/"
          , viewLink "/downloads/"
          ]
      , viewFromRoute model
      ]
  }

viewFromRoute model =
  case model.currentPage of
    Routes.Home ->
      viewAccounts model
    Routes.Statements ->
      viewStatements model
    Routes.Downloads ->
      viewDownloads model
    Routes.NotFound ->
      viewNotFound model
      
viewNotFound model =
  div [] [text "Not found."]

viewAccounts model =
  div [] [
    b [] [text "Accounts"]
    , p [] [text "Bunch of accounts."]
    , accountsTable model
  ]

accountsTable model =
  table [
    style "width" "100%"
   ][
    tr [] [
      th [] [text "ID"]
      , th [] [text "Account Nickname"]
      , th [] [text "Account Type"]
    ]
  ]

accountsLoading =
  div [] [text "Loading accounts..."]

accountsFailedToLoad =
  div [] [text "Accounts failed to load."]

accountsNoneToShow =
  div [] [text "No accounts to show."]

accountTableRow : Account -> Html Msg
accountTableRow account =
  tr [] [
    td [] [text account.id ] 
    , td [] [ text account.nickname ]
    , td [] [ text (accountTypeToString account.accountType) ]
  ]

viewStatements model =
  div [] [
    b [] [text "Statements"]
    , p [] [text "This is the statements page."]
  ]

viewDownloads model =
  div [] [
    b [] [text "Downloads"]
    , p [] [text "Downloads shown here."]
  ]


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]


---- PROGRAM ----


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }