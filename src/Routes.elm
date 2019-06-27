module Routes exposing (fromUrl, Route(..))

import Url exposing (Url)
import Url.Parser exposing (Parser, parse, (</>), top, int, map, oneOf, s, string)

type Route
  = Home
  | Statements
  | Downloads
  | NotFound

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map Home   top
    , map Statements    (s "statements")
    , map Downloads     (s "downloads")
    ]

fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (parse routeParser url)