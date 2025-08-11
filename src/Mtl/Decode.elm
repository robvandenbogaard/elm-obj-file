module Mtl.Decode exposing (decodeString)

import Dict exposing (Dict)


type DecodeMode a
    = Initial
    | NewMtl String (Maybe a)


decodeString : { createMaterial : String -> List String -> Maybe a -> Maybe a } -> String -> Result String (Dict String a)
decodeString { createMaterial } content =
    decodeHelp (String.lines content) 1 Initial createMaterial Dict.empty


decodeHelp :
    List String
    -> Int
    -> DecodeMode a
    -> (String -> List String -> Maybe a -> Maybe a)
    -> Dict String a
    -> Result String (Dict String a)
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


formatError : Int -> String -> Result String a
formatError lineno error =
    Err ("Line " ++ String.fromInt lineno ++ ": " ++ error)


materialSpecCommands : List String
materialSpecCommands =
    [ "Kd", "D" ]


skipCommands : List String
skipCommands =
    []
