module Time.Timestamp exposing ( .. )

{-| This module lets you wrap your component such, that incoming messages will
be transparently decorated with timestamps.

@docs Model, Message, update, view

-}

import Html exposing ( Html )
import Html.App as App
import Task
import Time exposing ( Time )


-- MODEL


{-| Wraps the essential parts of another component.

-}
type alias Model msg model =
    { model : model
    , update : (msg, Time) -> model -> (model, Cmd msg)
    , view : model -> Html msg
    }


-- UPDATE


{-| A wrapper for messages that might carry a timestamp. If it doesn't a command
is issued to get one that does and carries the same inner message.

-}
type Message msg = Timed Time msg | Untimed msg


{-| Updates the inner model given a wrapped message.

-}
update : Message msg -> Model msg model -> (Model msg model, Cmd (Message msg))
update msg model =
    case msg of

        Timed t msg ->
            let
                ( newModel, cmd ) =
                    model.update (msg, t) model.model
            in
                ( { model | model = newModel }
                , Cmd.map Untimed cmd
                )

        Untimed msg ->
            ( model
            , Task.perform
                (\_ -> Debug.crash "couldn't time a message")
                (\t -> Timed t msg)
                Time.now
            )


-- VIEW


{-| Render the component.

-}
view : Model msg model -> Html (Message msg)
view { model, view } =
    App.map Untimed <| view model