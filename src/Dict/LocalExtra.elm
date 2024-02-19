module Dict.LocalExtra exposing (intersectionWith)

import Dict exposing (Dict)


intersectionWith : (( a, b ) -> c) -> Dict comparable a -> Dict comparable b -> Dict comparable c
intersectionWith combine firstDict secondDict =
    firstDict
        |> Dict.foldl
            (\key firstValue ->
                case secondDict |> Dict.get key of
                    Nothing ->
                        identity

                    Just secondValue ->
                        Dict.insert key (combine ( firstValue, secondValue ))
            )
            Dict.empty
