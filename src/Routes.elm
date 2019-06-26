module Routes exposing (fromUrl, Route)

import Url exposing (Url)
import Url.Parser exposing (Parser, parse, (</>), int, map, oneOf, s, string)


type Route
  = Home
  | Statements
  | Download

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map Home   (s "")
    , map Statements    (s "statements")
    , map Download    (s "downloads")
    ]

fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> parse routeParser