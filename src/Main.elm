module Main exposing (Model, Msg(..), getFollowers, init, main, qiitaUserDecoder, subscriptions, update, view, viewGif)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, maybe, string)
import Url.Builder



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { userId : UserId
    , status : Status
    , pageNumber : PageNumber
    }


type Status
    = Failure Http.Error
    | Loading
    | Success QiitaUserList


initialUserId : UserId
initialUserId =
    "Yametaro"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { userId = initialUserId
      , status = Loading
      , pageNumber = 1
      }
    , getFollowers initialUserId 1
    )



-- UPDATE


type Msg
    = MorePlease
    | GotData (Result Http.Error QiitaUserList)
    | ChangeUserId String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUserId userId ->
            ( { model | userId = userId }, Cmd.none )

        MorePlease ->
            ( { model | status = Loading }, getFollowers model.userId model.pageNumber )

        GotData result ->
            case result of
                Ok qiitaUserList ->
                    ( { model | status = Success qiitaUserList }, Cmd.none )

                Err error ->
                    ( { model | status = Failure error }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Following Only You" ]
        , viewGif model
        ]


viewGif : Model -> Html Msg
viewGif model =
    case model.status of
        Failure error ->
            let
                _ =
                    Debug.log "Httpエラー：" error
            in
            div []
                [ text "見つかりませんでした"
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success qiitaUserList ->
            let
                filteredQiitaUserList =
                    List.filter (\qiitaUser -> qiitaUser.followees_count == 1) qiitaUserList
            in
            div []
                [ div []
                    [ input [ value model.userId, onInput ChangeUserId ] []
                    , button [ onClick MorePlease ] [ text "検索" ]
                    ]
                , p []
                    [ text <| "あなただけをフォローしているQiitaユーザーは"
                    , b [] [ text <| String.fromInt <| List.length filteredQiitaUserList ]
                    , text "名です"
                    ]
                , ul [] <| List.map viewQiitaUser filteredQiitaUserList
                ]


viewQiitaUser : QiitaUser -> Html Msg
viewQiitaUser qiitaUser =
    let
        description =
            Maybe.withDefault "" qiitaUser.description
    in
    li []
        [ figure []
            [ div [ class "profile_image_url" ] [ img [ src qiitaUser.profile_image_url, style "width" "100px" ] [] ]
            , figcaption []
                [ p [] [ text <| "ユーザーID：" ++ qiitaUser.userId ]
                , if qiitaUser.name == "" then
                    text ""

                  else
                    p [] [ text <| "名前：" ++ qiitaUser.name ]
                , if qiitaUser.name == "" then
                    text ""

                  else
                    p [] [ text <| "プロフィール：" ++ description ]
                , p [] [ text <| "フォロワー数：" ++ String.fromInt qiitaUser.followees_count ++ "名" ]
                ]
            ]
        ]



-- HTTP


type alias PageNumber =
    Int


type alias UserId =
    String


type alias QiitaUser =
    { name : String
    , userId : UserId
    , profile_image_url : String
    , description : Maybe String
    , followees_count : Int
    }


type alias QiitaUserList =
    List QiitaUser


getFollowers : UserId -> PageNumber -> Cmd Msg
getFollowers userId pageNumber =
    let
        url =
            Url.Builder.crossOrigin
                "https://qiita.com"
                [ "api", "v2", "users", userId, "followers" ]
                [ Url.Builder.string "page" (String.fromInt pageNumber)
                , Url.Builder.string "per_page" "100"
                ]
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotData qiitaUserListDecoder
        }


qiitaUserListDecoder : Decoder QiitaUserList
qiitaUserListDecoder =
    Json.Decode.list qiitaUserDecoder


qiitaUserDecoder : Decoder QiitaUser
qiitaUserDecoder =
    Json.Decode.map5 QiitaUser
        (field "name" string)
        (field "id" string)
        (field "profile_image_url" string)
        (field "description" (maybe string))
        (field "followees_count" int)
