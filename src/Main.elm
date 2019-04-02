module Main exposing (Key(..), Model, Msg(..), ToShow(..), TodoPoint, applyAt, dispatchWiew, init, isLastEmpty, keyDecoder, main, removeA, subscriptions, toggleStatus, update, updateAdd, updateStatus, updateTask, view, viewTodoPoint)

import Array exposing (Array)
import Browser
import Browser.Events
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D



-- https://stackoverflow.com/questions/33099945/how-to-remove-an-item-at-a-given-index-from-array-list-in-elm/33101419


{-| Split an array into two arrays, the first ending at and the second starting at the given index
-}
removeA : Int -> Array a -> Array a
removeA i a =
    let
        a1 =
            Array.slice 0 i a

        a2 =
            Array.slice (i + 1) (Array.length a) a
    in
    Array.append a1 a2



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias TodoPoint =
    { task : String
    , status : Bool
    }


type ToShow
    = DoneOnly
    | TodoOnly
    | Both


type alias Model =
    { show : ToShow
    , todos : Array TodoPoint
    , active : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { show = Both
      , todos = Array.fromList [ { task = "Barre de faire", status = False } ]
      , active = 0
      }
    , Cmd.none
    )



-- UPDATE$
-- ajouter ( ... , Cmd.none) à tout les retours de update ?


type Msg
    = Add
    | Keypress String
    | Drop Int
    | Write Int String
    | Toggle Int
    | GetOnlyDone
    | GetOnlyTodo
    | GetBoth


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Keypress key ->
            if key == "+" then
                updateAdd model

            else
                ( model, Cmd.none )

        GetOnlyDone ->
            ( { show = DoneOnly
              , todos = .todos <| model
              , active = .active <| model
              }
            , Cmd.none
            )

        GetOnlyTodo ->
            ( { show = TodoOnly
              , todos = .todos <| model
              , active = .active <| model
              }
            , Cmd.none
            )

        GetBoth ->
            ( { show = Both
              , todos = .todos <| model
              , active = .active <| model
              }
            , Cmd.none
            )

        Add ->
            -- Ajouter un TodoPoint à la liste dans le model
            updateAdd model

        Drop nb ->
            {- Supprimer le TodoPoint cible de la liste
               Il faut transmettre un n° de TodoPoint pour supprimer la
               bonne div
               Handling user input
            -}
            ( { show = .show <| model
              , todos = removeA nb <| .todos <| model
              , active = .active <| model
              }
            , Cmd.none
            )

        Write nb txt ->
            -- Modfier le text de task
            -- Function partial: (updateTask txt)
            ( { show = .show <| model
              , todos = applyAt nb (updateTask txt) <| .todos <| model
              , active = nb
              }
            , Cmd.none
            )

        Toggle nb ->
            -- Toggle le status
            -- En plus de mettre à jour le record TodoPoint cible avec updateStatus
            -- faut changer la liste "model", il faut la changer en utilisant updateStatus
            -- pb: .status à besoin d'un arg de type TodoPoint et on lui passe du Maybe TodoPoint
            -- https://dennisreimann.de/articles/elm-maybe.html
            ( { show = .show <| model
              , todos = applyAt nb toggleStatus <| .todos <| model
              , active = .active <| model
              }
            , Cmd.none
            )


updateAdd : Model -> ( Model, Cmd Msg )
updateAdd model =
    ( { show = .show <| model
      , todos = model |> .todos |> Array.push (TodoPoint "" False)
      , active = .active <| model
      }
    , Cmd.none
    )



-- update général devient gros -> le découper en petit update
-- Pour résoudre le pb générer par " { (getAt msg.numTodo model) | task = msg.text } "
-- J'utilise du code boillerplate dont se plaint @skosch dans :
-- https://discourse.elm-lang.org/t/dynamic-record-updates/1198/2


updateTask : String -> TodoPoint -> TodoPoint
updateTask text todoPoint =
    { todoPoint | task = text }


toggleStatus : TodoPoint -> TodoPoint
toggleStatus todoPoint =
    let
        newStatus =
            not todoPoint.status
    in
    { todoPoint | status = newStatus }


updateStatus : Bool -> TodoPoint -> TodoPoint
updateStatus status todoPoint =
    { todoPoint | status = status }



-- https://codereview.stackexchange.com/questions/123129/maybe-handling-in-matrix-manipulation-in-elm


applyAt : Int -> (a -> a) -> Array a -> Array a
applyAt i f array =
    case Array.get i array of
        Nothing ->
            array

        Just a ->
            Array.set i (f a) array



-- SUBSCRIPTIONS
-- https://guide.elm-lang.org/effects/http.html
-- https://github.com/chrisbuttery/elm-subscriptions/blob/master/KeyboardArrows.elm


type Key
    = PressedLetter Char
    | Control String


subscriptions : Model -> Sub Msg
subscriptions model =
    -- https://github.com/elm/browser/blob/master/examples/wasd.elm
    Browser.Events.onKeyPress (D.map Keypress keyDecoder)


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string



-- VIEW
-- Parcourrir une liste en passant à la function de mappage l'index de l'élément:
-- https://package.elm-lang.org/packages/elm-lang/core/3.0.0/List#indexedMap
-- Passer les deux éléments du tuple de indexedMap à viewTodoPoint qui accepte
-- deux arguments :
-- https://stackoverflow.com/questions/40855019/is-there-a-way-to-pipe-two-arguments-to-a-function-in-elm
-- -> nb: la fonction uncurry a été sortie des fonctions de base


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "Value of model: " model
    in
    div [ class "content" ]
        [ div [ id "todoPoints" ] (Array.toList (Array.indexedMap (dispatchWiew model.show) model.todos))
        , button [ onClick Add, disabled (isLastEmpty <| Array.toList <| model.todos) ] [ text "Add" ]
        , div [ id "filters" ]
            [ button [ onClick GetOnlyDone ] [ text "Only done" ]
            , button [ onClick GetOnlyTodo ] [ text "Only to do" ]
            , button [ onClick GetBoth ] [ text "done & to do" ]
            ]
        ]


isLastEmpty : List TodoPoint -> Bool
isLastEmpty listTodo =
    let
        last =
            List.head <| List.reverse <| listTodo
    in
    case last of
        Just todo ->
            todo |> .task |> String.isEmpty

        Nothing ->
            False



-- Faut-il implémenter un functor (ou applicative functor ?) pour savoir si on display le TodoPoint
-- en fonction de l'état "show" du programme ?
-- et/ou
-- voir Signals: https://en.wikibooks.org/wiki/Elm_programming_language
-- Créer un function dispatchWiew ?


dispatchWiew : ToShow -> Int -> TodoPoint -> Html Msg
dispatchWiew show nb todoPoint =
    case show of
        DoneOnly ->
            if todoPoint.status then
                viewTodoPoint nb todoPoint

            else
                div [] []

        TodoOnly ->
            if not todoPoint.status then
                viewTodoPoint nb todoPoint

            else
                div [] []

        Both ->
            viewTodoPoint nb todoPoint


viewTodoPoint : Int -> TodoPoint -> Html Msg
viewTodoPoint nb todoPoint =
    div []
        [ input [ type_ "checkbox", onClick (Toggle nb), checked todoPoint.status ] []
        , input
            [ type_ "text"
            , value todoPoint.task
            , readonly todoPoint.status
            , onInput (Write nb)
            ]
            []
        , button [ onClick (Drop nb) ] [ text "supprimer" ]
        ]



-- Utiliser la donnée active todo pour changer la couleur
