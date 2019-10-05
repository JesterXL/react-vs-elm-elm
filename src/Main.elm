module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (Html, text, div, h1, img, ul, li, a, b, p, table, th, td, tr, button, select, input, br)
import Html.Attributes exposing (src, href, style, type_, align)
import Html.Events exposing (onClick)
import Debug exposing (log)
import Routes exposing (fromUrl, Route(..))
import Url.Parser exposing (string)
import Array exposing (..)
import Http
import Json.Decode exposing (Decoder, map3, field, string, int, list)
import Chunk exposing (chunk)

---- MODEL ----



type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentPage : Route
    , accountState : AccountsState
    }

type AccountsState
  = AccountsNotLoaded
  | AccountsLoading
  | AccountsLoadNothing
  | AccountsLoadFailed String
  | AccountsLoadSuccess AccountView

type alias AccountView =
  { accounts : List Account
    , currentPage : Int
    , pageSize : Int
    , totalPages : Int
  }

nextPage : AccountView -> AccountView
nextPage accountView =
  if accountView.currentPage < accountView.pageSize - 1 then
    { accountView | currentPage = accountView.currentPage + 1}
  else
    accountView



previousPage : AccountView -> AccountView
previousPage accountView =
  if accountView.currentPage > 0 then
    { accountView | currentPage = accountView.currentPage - 1}
  else
    accountView

type AccountType
  = Checking
  | Savings
  | JamesBond
  | MutualMoo

accountTypeToString : AccountType -> String
accountTypeToString accountType =
  case accountType of
    Checking -> "Checking"
    Savings -> "Savings"
    JamesBond -> "Bond... James Bond"
    MutualMoo -> "Mutual Cow"

type alias Account =
  { id : String
  , nickname : String
  , accountType: AccountType }

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        msg = log "init url" url
    in
    (Model key url Home AccountsNotLoaded, Cmd.none)


loadAccounts =
  Http.get
    { url = "http://localhost:8001/accounts"
    , expect = Http.expectJson FetchAccountsResult accountsDecoder
    }

accountsDecoder : Decoder (List AccountJSON)
accountsDecoder =
  list accountDecoder

type alias AccountJSON = 
 { id : String
 , nickname : String 
 , typeString : String }

accountDecoder : Decoder AccountJSON
accountDecoder =
  map3 AccountJSON
    (field "id" string)
    (field "nickname" string)
    (field "type" string)


accountTypeDecode : String -> AccountType
accountTypeDecode accountTypeString =
  if String.toLower accountTypeString == "checking" then
    Checking
  else if String.toLower accountTypeString == "savings" then
    Savings
  else if String.toLower accountTypeString == "bond... james bond" then
    JamesBond
  else if String.toLower accountTypeString == "mutual cow" then
    MutualMoo
  else
    Checking

accountJSONToAccount : AccountJSON -> Account
accountJSONToAccount accountJSON = 
  Account accountJSON.id accountJSON.nickname (accountTypeDecode accountJSON.typeString)

accountJSONToAccounts : List AccountJSON -> List Account
accountJSONToAccounts accountsList =
  List.map accountJSONToAccount accountsList

---- UPDATE ----


type Msg =
    LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | FetchAccounts
    | FetchAccountsResult (Result Http.Error (List AccountJSON))
    | PreviousAccountsPage
    | NextAccountsPage

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

    FetchAccounts ->
      ( model, loadAccounts)

    FetchAccountsResult result ->
      case result of
        Ok accountJSONs ->
          (let
              desiredPageSize = 10
              chunkedAccounts = chunk desiredPageSize accountJSONs
              msg1 = log "accountJSONs is" chunkedAccounts
              accounts = accountJSONToAccounts accountJSONs
              accountView = AccountView accounts 0 desiredPageSize (List.length chunkedAccounts)
          in
          
          ( { model | accountState = AccountsLoadSuccess accountView }
          , Cmd.none
          ))
        Err datError ->
          let
              msg2 = log "err is" datError
          in
          
          ( { model | accountState = AccountsLoadFailed (httpErrorToString datError) }
            , Cmd.none
          )

    PreviousAccountsPage ->
      case model.accountState of
        AccountsNotLoaded ->
          ( model, Cmd.none )
        AccountsLoading ->
          ( model, Cmd.none )
        AccountsLoadNothing ->
          ( model, Cmd.none )
        AccountsLoadFailed _ ->
          ( model, Cmd.none )
        AccountsLoadSuccess accountView ->
          let
            updatedView = previousPage accountView
          in
            (
              { model | accountState = AccountsLoadSuccess updatedView }
              , Cmd.none 
            )
    NextAccountsPage ->
      case model.accountState of
        AccountsNotLoaded ->
          ( model, Cmd.none )
        AccountsLoading ->
          ( model, Cmd.none )
        AccountsLoadNothing ->
          ( model, Cmd.none )
        AccountsLoadFailed _ ->
          ( model, Cmd.none )
        AccountsLoadSuccess accountView ->
          let
            updatedView =  nextPage accountView
          in
            (
              { model | accountState = AccountsLoadSuccess updatedView }
              , Cmd.none 
            )

httpErrorToString : Http.Error -> String
httpErrorToString error =
  case error of
    Http.BadUrl reason ->
      "BadUrl, reason: " ++ reason
    Http.Timeout ->
      "Timeout"
    Http.NetworkError ->
      "NetworkError"
    Http.BadStatus status ->
      "BadStatus, statusCode :" ++ (String.fromInt status)
    Http.BadBody reason ->
      "BadBody, reason: " ++ reason

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

---- VIEW ----

view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ viewFromRoute model ]
  }

viewFromRoute : Model -> Html Msg
viewFromRoute model =
  let
      msg1 = log "viewFromRoute current page" model.currentPage
  in
  case model.currentPage of
    Routes.Home ->
      viewAccounts model
    Routes.NotFound ->
      viewNotFound model
      
viewNotFound : Model -> Html Msg
viewNotFound model =
  div [] [text "Not found."]

getCurrentPage : Int -> Int -> List Account -> Array Account 
getCurrentPage pageSize currentPage accounts =
  let
      wat = chunk pageSize accounts
      msg1 = log "what lol" wat
  in
  
  chunk pageSize accounts
  |> Array.fromList
  |> Array.get currentPage
  |> Maybe.withDefault []
  |> Array.fromList


viewAccounts : Model -> Html Msg
viewAccounts model =
  div [ style "margin" "8px"] [
    h1 [style "text-align" "left"] [text "Accounts"]
    , div [style "display" "flex"] [
      button [ onClick FetchAccounts, style "padding" "8px", style "margin-bottom" "8px" ] [ text "Fetch Accounts"]
    ]
    , case model.accountState of
        AccountsNotLoaded ->
          div [] [ text "Accounts not fetched yet."]
        AccountsLoading ->
          div [] [ text "Loading accounts..."]
        AccountsLoadNothing ->
          div [] [ text "No accounts." ]
        AccountsLoadFailed errorString ->
          div [] [ text ("Failed to load accounts:" ++ errorString) ]
        AccountsLoadSuccess accountView ->
          accountsTable accountView.totalPages accountView.currentPage (getCurrentPage accountView.pageSize accountView.currentPage accountView.accounts)
  ]

accountToRow : Account -> Html Msg
accountToRow account =
  tr [] [
    td [align "left"] [text account.id]
      , td [align "left"] [text account.nickname]
      , td [align "left"] [text (accountTypeToString account.accountType)]
  ]

getDisabledTrue : Bool -> String
getDisabledTrue bool =
  case bool of
    True -> "true"
    False -> "false"

accountsTable : Int -> Int -> Array Account -> Html Msg
accountsTable totalPages currentPage accounts =
  div [] [
    table [
      style "width" "100%"
    ] 
      (
          [
           tr [] [
            th [align "left"] [text "ID"]
            , th [align "left"] [text "Account Nickname"]
            , th [align "left"] [text "Account Type"]
          ]
        ] ++ (Array.map accountToRow accounts |> Array.toList)
      )
      , br [][]
    , div [style "display" "flex", style "height" "300px"][
        div [style "display" "flex", style "width" "300px", style "height" "100px", style "margin" "auto"][
            button [
              onClick PreviousAccountsPage
              , style "disabled" (getDisabledTrue (currentPage  == totalPages))
              , style "flex-grow" "2"
            ] [text "<"]
          , div [style "padding" "8px", style "width" "60px"][ text (String.fromInt(currentPage + 1) ++ " of " ++ (String.fromInt totalPages) )]
          , button [
            onClick NextAccountsPage
            , style "flex-grow" "2"
          ] [text ">"]
        ]
    ]
  ]

accountsLoading =
  div [] [text "Loading accounts..."]

accountsFailedToLoad =
  div [] [text "Accounts failed to load."]

accountsNoneToShow =
  div [] [text "No accounts to show."]

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