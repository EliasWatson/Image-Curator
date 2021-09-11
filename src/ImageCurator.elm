module ImageCurator exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array)
import Array exposing (get)

type Msg
    = SetCurrentImageStr String
    | PrevImage
    | NextImage
    | ToggleApproved
    | SetCropLeft String
    | SetCropTop String
    | SetCropSize String

type alias CropRegion =
    { top : Int
    , left : Int
    , size : Int
    }

type alias Image =
    { filename : String
    , approved : Bool
    , crop : CropRegion
    }

type alias Model =
    { images : Array Image
    , currentImageIndex : Int
    }

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewNavigationBar model
        , viewWorkspace model
        ]

viewNavigationBar : Model -> Html Msg
viewNavigationBar model =
    div [ class "navbar" ] 
        [ viewButton [] PrevImage (i [ class "fas fa-backward" ] [])
        , input
            [ type_ "number", placeholder "Image Index"
            , value (String.fromInt (model.currentImageIndex + 1))
            , onInput SetCurrentImageStr
            ] []
        , viewButton [] NextImage (i [ class "fas fa-forward" ] [])
        ]

viewWorkspace : Model -> Html Msg
viewWorkspace model =
    div [ class "workspace" ]
        [ viewProperties model
        , viewImageViewer model
        ]

viewProperties : Model -> Html Msg
viewProperties model =
    let
        currentImage = getCurrentImage model
    in
    div [ class "properties" ]
        [ div [ class "filename" ] [ text (getCurrentImage model).filename ]
        , viewCheckbox ToggleApproved (getCurrentImage model).approved "Approved"
        , table [ class "crop-table" ]
            [ tr []
                [ td [] [ text "Crop Left" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.crop.left)
                    , onInput SetCropLeft
                    ] [] ]
                ]
            , tr []
                [ td [] [ text "Crop Top" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.crop.top)
                    , onInput SetCropTop
                    ] [] ]
                ]
            , tr []
                [ td [] [ text "Crop Size" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.crop.size)
                    , onInput SetCropSize
                    ] [] ]
                ]
            ]
        ]

viewImageViewer : Model -> Html Msg
viewImageViewer model =
    let
        currentImage = getCurrentImage model
    in
    div [ class "image-viewer" ]
        [ img [ src currentImage.filename ] []
        ]

viewButton : List String -> Msg -> Html Msg -> Html Msg
viewButton classes msg content =
    div [ classList (( "button", True ) :: (List.map (\class_ -> ( class_, True )) classes))
        , onClick msg
        ] [ content ]

viewCheckbox : Msg -> Bool -> String -> Html Msg
viewCheckbox msg checked_ name =
    label []
        [ input [ type_ "checkbox", checked checked_, onClick msg ] []
        , text name
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentImage = getCurrentImage model
        currentCrop = currentImage.crop
    in
    case msg of
        SetCurrentImageStr indexString ->
            ( updateCurrentImage
                model
                ((Maybe.withDefault (model.currentImageIndex + 1) (String.toInt indexString)) - 1)
            , Cmd.none
            )
        PrevImage -> ( updateCurrentImage model (model.currentImageIndex - 1), Cmd.none )
        NextImage -> ( updateCurrentImage model (model.currentImageIndex + 1), Cmd.none )
        ToggleApproved ->
            ( updateCurrentImageProperties model { currentImage | approved = not currentImage.approved }
            , Cmd.none
            )
        SetCropLeft numString ->
            ( updateCurrentImageProperties
                model
                { currentImage | crop =
                    { currentCrop
                    | left = (Maybe.withDefault currentCrop.left (String.toInt numString)) }
                    }
            , Cmd.none
            )
        SetCropTop numString ->
            ( updateCurrentImageProperties
                model
                { currentImage | crop =
                    { currentCrop
                    | top = (Maybe.withDefault currentCrop.top (String.toInt numString)) }
                    }
            , Cmd.none
            )
        SetCropSize numString ->
            ( updateCurrentImageProperties
                model
                { currentImage | crop =
                    { currentCrop
                    | size = (Maybe.withDefault currentCrop.size (String.toInt numString)) }
                    }
            , Cmd.none
            )

updateCurrentImage : Model -> Int -> Model
updateCurrentImage model index =
    { model
    | currentImageIndex = if Array.isEmpty model.images then 0 else clamp 0 ((Array.length model.images) - 1) index
    }

updateCurrentImageProperties : Model -> Image -> Model 
updateCurrentImageProperties model image =
    { model | images = Array.set model.currentImageIndex image model.images }

getCurrentImage : Model -> Image
getCurrentImage model =
    Maybe.withDefault emptyImage
        <| Array.get model.currentImageIndex model.images

emptyImage : Image
emptyImage =
    { filename = ""
    , approved = False
    , crop = { top = 0, left = 0, size = 0 }
    }

initialModel : Model
initialModel =
    { images = Array.fromList [{filename="test.png", approved=False, crop={top=0, left=0, size=0}}]
    , currentImageIndex = 0
    }

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
