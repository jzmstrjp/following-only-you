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
    , totalPages : Int
    , followersList : QiitaUserList
    }


type Status
    = Failure Http.Error
    | Loading
    | GetListSuccess
    | GetUserSuccess User


initialUserId : UserId
initialUserId =
    "Yametaro"


init : () -> ( Model, Cmd Msg )
init _ =
    ( { userId = initialUserId
      , status = Loading
      , pageNumber = 1
      , totalPages = 0
      , followersList = []
      }
    , getFollowers initialUserId 1
    )



-- UPDATE


type Msg
    = GetUser
    | MorePlease
    | GotUser (Result Http.Error User)
    | GotData (Result Http.Error QiitaUserList)
    | ChangeUserId String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUserId userId ->
            ( { model | userId = userId }, Cmd.none )

        GetUser ->
            ( { model | status = Loading }, getUser model.userId )

        MorePlease ->
            ( { model | status = Loading }, getFollowers model.userId model.pageNumber )

        GotUser result ->
            case result of
                Ok user ->
                    ( { model
                        | status = GetUserSuccess user
                        , totalPages = culculateTotalPages user.followers_count
                      }
                    , getFollowers model.userId 1
                    )

                Err error ->
                    ( { model | status = Failure error }, Cmd.none )

        GotData result ->
            case result of
                Ok qiitaUserList ->
                    if model.pageNumber < model.totalPages then
                        ( { model
                            | status = Loading
                            , pageNumber = model.pageNumber + 1
                            , followersList = model.followersList ++ qiitaUserList
                          }
                        , getFollowers model.userId (model.pageNumber + 1)
                        )

                    else
                        ( { model | status = GetListSuccess }, Cmd.none )

                Err error ->
                    ( { model | status = Failure error }, Cmd.none )


culculateTotalPages : Int -> Int
culculateTotalPages followers_count =
    followers_count // 100 + 1



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
                , button [ onClick GetUser ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        GetUserSuccess user ->
            div []
                [ text <| String.fromInt user.followers_count
                , br [] []
                , text <| String.fromInt model.totalPages
                ]

        GetListSuccess ->
            let
                filteredFollowersList =
                    List.filter (\qiitaUser -> qiitaUser.followees_count == 1) model.followersList
            in
            div []
                [ div []
                    [ input [ value model.userId, onInput ChangeUserId ] []
                    , button [ onClick GetUser ] [ text "検索" ]
                    ]
                , p []
                    [ text <| "あなただけをフォローしているQiitaユーザーは"
                    , b [] [ text <| String.fromInt <| List.length filteredFollowersList ]
                    , text "名です"
                    ]
                , ul [] <| List.map viewQiitaUser filteredFollowersList
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


type alias User =
    { followers_count : Int }


type alias QiitaUser =
    { name : String
    , userId : UserId
    , profile_image_url : String
    , description : Maybe String
    , followees_count : Int
    }


type alias QiitaUserList =
    List QiitaUser


getUser : UserId -> Cmd Msg
getUser userId =
    Http.get
        { url = "https://qiita.com/api/v2/users/" ++ userId
        , expect = Http.expectJson GotUser userDecoder
        }


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


userDecoder : Decoder User
userDecoder =
    Json.Decode.map User
        (field "followers_count" int)


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
