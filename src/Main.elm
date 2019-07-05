module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (Html, text, div, h1, img, ul, li, a, b, p, table, th, td, tr, button, select, input)
import Html.Attributes exposing (src, href, style, type_)
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

type AccountsState =
  AccountsNotLoaded
  | AccountsLoading
  | AccountsLoadNothing
  | AccountsLoadFailed
  | AccountsLoadSuccess AccountView

type SortDirection =
  Ascending
  | Descending
  | NoSort

type alias AccountView =
  { accounts : List Account
    , currentPage : Int
    , filteredAndSortedAccounts : List Account
    , pageSize : Int
    , filterText : String
    , sortDirection : SortDirection
  }

filterItems : String -> List Account -> List Account
filterItems filterText accounts =
  List.filter (\account -> String.contains filterText account.nickname) accounts

-- sortItems : (a -> comparable) -> List Account -> List Account
-- sortItems field accounts =
--   List.sortWith (\a b =
    -- )

filterAndSortItems : String -> List Account -> List Account
filterAndSortItems filterText accounts =
  filterItems filterText accounts
  -- |> sortItems field

nextPage : AccountView -> AccountView
nextPage accountView =
  if accountView.currentPage < accountView.pageSize then
    { accountView | currentPage = accountView.currentPage + 1}
  else
    accountView

previousPage : AccountView -> AccountView
previousPage accountView =
  if accountView.currentPage > 0 then
    { accountView | currentPage = accountView.currentPage - 1}
  else
    accountView

-- reFilterAndSortItems : AccountView -> AccountView
-- reFilterAndSortItems accountView =



type AccountType = 
  DemandDeposit
  | AccountAnalysis

accountTypeToString : AccountType -> String
accountTypeToString accountType =
  case accountType of
    DemandDeposit -> "Demand Deposit"
    AccountAnalysis -> "Account Analysis"

type alias Account =
  { id : Int
  , nickname : String
  , accountType: AccountType }

-- init : ( Model, Cmd Msg )
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        msg = log "init url" url
    in
    (Model key url Home AccountsNotLoaded, Cmd.none)


loadAccounts =
  Http.get
    { url = "http://localhost:8001/accounts/dda"
    , expect = Http.expectJson FetchAccountsResult accountsDecoder
    }

accountsDecoder : Decoder (List AccountJSON)
accountsDecoder =
  list accountDecoder


type alias AccountJSON = 
 { id : Int
 , nickname : String 
 , typeString : String }

accountDecoder : Decoder AccountJSON
accountDecoder =
  map3 AccountJSON
    (field "id" int)
    (field "nickname" string)
    (field "type" string)


accountTypeDecode : String -> AccountType
accountTypeDecode accountTypeString =
  if String.toLower accountTypeString == "dda" then
    DemandDeposit
  else if String.toLower accountTypeString == "aa" then
    AccountAnalysis
  else
    DemandDeposit

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
          let
              msg1 = log "accountJSONs is" (chunk 10 accountJSONs)
              accounts = accountJSONToAccounts accountJSONs
              filteredAndSortedAccounts = filterAndSortItems "" accounts
              accountView = AccountView accounts 0 filteredAndSortedAccounts 10 "" NoSort
          in
          
          ( { model | accountState = AccountsLoadSuccess accountView }
          , Cmd.none
          )
        Err datError ->
          let
              msg2 = log "err is" datError
          in
          
          ( { model | accountState = AccountsLoadFailed }
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
        AccountsLoadFailed ->
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
        AccountsLoadFailed ->
          ( model, Cmd.none )
        AccountsLoadSuccess accountView ->
          let
            updatedView =  nextPage accountView
          in
            (
              { model | accountState = AccountsLoadSuccess updatedView }
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

viewFromRoute : Model -> Html Msg
viewFromRoute model =
  -- let
  --     msg1 = log "viewFromRoute current page" model.currentPage
  -- in
  case model.currentPage of
    Routes.Home ->
      viewAccounts model
    Routes.Statements ->
      viewStatements model
    Routes.Downloads ->
      viewDownloads model
    Routes.NotFound ->
      viewNotFound model
      
viewNotFound : Model -> Html Msg
viewNotFound model =
  div [] [text "Not found."]

getCurrentPage : Int -> Int -> List Account -> Array Account 
getCurrentPage pageSize currentPage accounts =
  chunk pageSize accounts
  |> Array.fromList
  |> Array.get currentPage
  |> Maybe.withDefault []
  |> Array.fromList

viewAccounts : Model -> Html Msg
viewAccounts model =
  div [] [
    b [] [text "Accounts"]
    , p [] [text "Bunch of accounts."]
    , button [ onClick FetchAccounts ] [ text "Fetch Accounts"]
    , case model.accountState of
        AccountsNotLoaded ->
          div [] [ text "Accounts not fetched yet."]
        AccountsLoading ->
          div [] [ text "Loading accounts..."]
        AccountsLoadNothing ->
          div [] [ text "No accounts." ]
        AccountsLoadFailed ->
          div [] [ text "Failed to load accounts." ]
        AccountsLoadSuccess accountView ->
          accountsTable accountView.currentPage (getCurrentPage accountView.pageSize accountView.currentPage accountView.filteredAndSortedAccounts)
  ]

accountToRow : Account -> Html Msg
accountToRow account =
  tr [] [
    td [] [input [type_ "checkbox"][]]
    , td [] [text (String.fromInt account.id)]
      , td [] [text account.nickname]
      , td [] [text (accountTypeToString account.accountType)]
  ]

accountsTable : Int -> Array Account -> Html Msg
accountsTable currentPage accounts =
  div [] [
    table [
      style "width" "100%"
    ] 
      (
          [
          tr [] [
            th [] [input [ type_ "checkbox"] [] ]
            , th [] [text "ID"]
            , th [] [text "Account Nickname"]
            , th [] [text "Account Type"]
          ]
        ] ++ (Array.map accountToRow accounts |> Array.toList)
      )
    , button [onClick PreviousAccountsPage ] [text "<"]
    , div [][ text ("Current Page: " ++ String.fromInt(currentPage))]
    , button [onClick NextAccountsPage ] [text ">"]
  ]

   
  

accountsLoading =
  div [] [text "Loading accounts..."]

accountsFailedToLoad =
  div [] [text "Accounts failed to load."]

accountsNoneToShow =
  div [] [text "No accounts to show."]

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