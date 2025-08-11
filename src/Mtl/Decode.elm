module Mtl.Decode exposing
    ( decodeString
    , MaterialCreator, MaterialLibrary
    )

{-|


# Decoding

@docs decodeString


# Material

@docs MaterialCreator, MaterialLibrary

-}

import Dict exposing (Dict)


{-| A MaterialCreator is a function that takes a name, a list of words from the
current Mtl line, and maybe the material that was already in the material
library under the name. It outputs a material if successfully created from the
given information, but it can also decide to not return a material even though
it could, leaving the library unchanged.

This is subject to change as the
rationale for it is not entirely clear, suspecting the mechanism can be simpler.

-}
type alias MaterialCreator material =
    String -> List String -> Maybe material -> Maybe material


{-| A library of materials, a dictionary mapping names to materials.
-}
type alias MaterialLibrary material =
    Dict String material


type DecodeMode material
    = Initial
    | NewMtl String (Maybe material)


{-| Decode an mtl data string into a material library. This can fail so a
Result is returned which can hold an error message if parsing fails.
-}
decodeString : { createMaterial : MaterialCreator material } -> String -> Result String (MaterialLibrary material)
decodeString { createMaterial } content =
    decodeHelp (String.lines content) 1 Initial createMaterial Dict.empty


decodeHelp :
    List String
    -> Int
    -> DecodeMode material
    -> MaterialCreator material
    -> MaterialLibrary material
    -> Result String (MaterialLibrary material)
decodeHelp lines lineno mode createMaterial materials =
    case lines of
        [] ->
            Ok materials

        line :: remainingLines ->
            let
                words =
                    String.words line

                startsWith =
                    case words of
                        firstWord :: _ ->
                            firstWord

                        _ ->
                            ""

                remainingWords =
                    List.drop 1 words
            in
            -- conditions are sorted based on their frequency
            if startsWith == "newmtl" then
                let
                    name =
                        String.join " " remainingWords
                in
                if String.isEmpty name then
                    formatError lineno "Nameless material"

                else
                    decodeHelp remainingLines (lineno + 1) (NewMtl name Nothing) createMaterial materials

            else if List.any ((==) startsWith) materialSpecCommands then
                case mode of
                    NewMtl name material ->
                        materials
                            |> Dict.get name
                            |> createMaterial name (startsWith :: remainingWords)
                            |> Maybe.map (\m -> Dict.insert name m materials)
                            |> Maybe.withDefault materials
                            |> decodeHelp remainingLines (lineno + 1) mode createMaterial

                    _ ->
                        formatError lineno
                            ("Invalid MTL syntax, parameters outside material definition '"
                                ++ (if String.length line > 20 then
                                        String.left 20 line ++ "...'"

                                    else
                                        line ++ "'"
                                   )
                            )

            else if startsWith == "" then
                let
                    newMaterials =
                        case mode of
                            NewMtl name (Just material) ->
                                Dict.insert name material materials

                            _ ->
                                materials
                in
                decodeHelp remainingLines (lineno + 1) Initial createMaterial newMaterials

            else if String.left 1 startsWith == "#" || List.any ((==) startsWith) skipCommands then
                -- Skip empty lines, comments and unsupported commands
                decodeHelp remainingLines (lineno + 1) mode createMaterial materials

            else
                formatError lineno
                    ("Invalid MTL syntax '"
                        ++ (if String.length line > 20 then
                                String.left 20 line ++ "...'"

                            else
                                line ++ "'"
                           )
                    )


formatError : Int -> String -> Result String material
formatError lineno error =
    Err ("Line " ++ String.fromInt lineno ++ ": " ++ error)


materialSpecCommands : List String
materialSpecCommands =
    [ "Kd", "D" ]


skipCommands : List String
skipCommands =
    []
