module ImageCurator exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Http

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, title, type_, placeholder, value, checked)
import Html.Events exposing (onClick, onInput)

import Array exposing (Array)

import Json.Decode exposing (Decoder, int, array, string, bool, succeed)
import Json.Decode.Pipeline exposing (optional, required)

import Canvas exposing (rect, shapes, texture)
import Canvas.Texture as Texture exposing (Texture)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate, scale)
import Color

apiUrl : String
apiUrl = "http://192.168.0.152:8080"

canvasSize : number
canvasSize = 768

type Msg
    = GotImages (Result Http.Error (Array Image))
    | GotTexture (Maybe Texture)
    | UpdatedDatabase (Result Http.Error ())
    | SetCurrentImageStr String
    | PrevImage
    | NextImage
    | ToggleApproved
    | SetCropLeft String
    | SetCropTop String
    | SetCropSize String
    | AnimationFrame Float

type Load a
    = Loading
    | Loaded a
    | Errored String

type alias Image =
    { filename : String
    , approved : Bool
    , crop_left : Int
    , crop_top : Int
    , crop_size : Int
    }

type alias Model =
    { status : Load (Array Image)
    , currentImageIndex : Int
    , currentTexture : Load Texture
    , currentTextureSource : Texture.Source Msg
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
                    , value (String.fromInt currentImage.crop_left)
                    , onInput SetCropLeft
                    ] [] ]
                ]
            , tr []
                [ td [] [ text "Crop Top" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.crop_top)
                    , onInput SetCropTop
                    ] [] ]
                ]
            , tr []
                [ td [] [ text "Crop Size" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.crop_size)
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
        [ Canvas.toHtmlWith
            { width = canvasSize
            , height = canvasSize
            , textures = [ model.currentTextureSource ]
            }
            []
            [ canvasClearScreen
            , canvasRender model
            ]
        ]

canvasClearScreen : Canvas.Renderable
canvasClearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) canvasSize canvasSize ]

canvasRender : Model -> Canvas.Renderable
canvasRender model =
    case model.currentTexture of
        Loaded texture_ ->
            let
                dimensions = Texture.dimensions texture_
                scale_ = canvasSize / (max dimensions.width dimensions.height)
                leftShift = (canvasSize - (dimensions.width * scale_)) / 2
                topShift = (canvasSize - (dimensions.height * scale_)) / 2
            in
                texture
                    [ transform
                        [ translate leftShift topShift
                        , scale scale_ scale_
                        ]
                    ]
                    ( 0, 0 )
                    texture_
        Loading -> shapes [] []
        Errored _ -> shapes [] []

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
    in
    case msg of
        GotImages (Ok images) ->
            (   { model
                | status = Loaded images
                , currentTextureSource = Texture.loadFromImageUrl
                    ( apiUrl
                    ++ "/get_image/"
                    ++ (Maybe.withDefault emptyImage <| Array.get 0 images).filename
                    )
                    GotTexture
                }
            , Cmd.none
            )
        GotImages (Err _) ->
            (   { model
                | status = Errored "Failed to load images"
                , currentTextureSource = Texture.loadFromImageUrl
                    "img/error.png"
                    GotTexture
                }
            , Cmd.none
            )
        GotTexture Nothing -> ( { model | currentTexture = Errored "Failed to load texture" }, Cmd.none )
        GotTexture (Just texture) -> ( { model | currentTexture = Loaded texture }, Cmd.none )
        UpdatedDatabase _ -> ( model, Cmd.none )
        SetCurrentImageStr indexString ->
            ( updateCurrentImage
                model
                ((Maybe.withDefault
                    (model.currentImageIndex + 1)
                    (String.toInt indexString)) - 1
                )
            , Cmd.none
            )
        PrevImage -> ( updateCurrentImage model (model.currentImageIndex - 1), Cmd.none )
        NextImage -> ( updateCurrentImage model (model.currentImageIndex + 1), Cmd.none )
        ToggleApproved ->
            ( updateCurrentImageProperties
                model
                { currentImage | approved = not currentImage.approved }
            , Http.get
                { url = apiUrl
                    ++ "/set_image_approved/"
                    ++ currentImage.filename
                    ++ "/"
                    ++ (if currentImage.approved then "false" else "true")
                , expect = Http.expectWhatever UpdatedDatabase
                }
            )
        SetCropLeft numString ->
            let
                newImage = { currentImage | crop_left = (Maybe.withDefault currentImage.crop_left (String.toInt numString)) }
            in
            ( updateCurrentImageProperties model newImage, databaseUpdateCrop newImage )
        SetCropTop numString ->
            let
                newImage = { currentImage | crop_top = (Maybe.withDefault currentImage.crop_top (String.toInt numString)) }
            in
            ( updateCurrentImageProperties model newImage, databaseUpdateCrop newImage )
        SetCropSize numString ->
            let
                newImage = { currentImage | crop_size = (Maybe.withDefault currentImage.crop_size (String.toInt numString)) }
            in
            ( updateCurrentImageProperties model newImage, databaseUpdateCrop newImage )
        AnimationFrame dt -> ( model, Cmd.none )

databaseUpdateCrop : Image -> Cmd Msg
databaseUpdateCrop image =
    Http.get
        { url = apiUrl
            ++ "/set_image_crop/"
            ++ image.filename
            ++ "/" ++ (String.fromInt image.crop_left)
            ++ "/" ++ (String.fromInt image.crop_top)
            ++ "/" ++ (String.fromInt image.crop_size)
        , expect = Http.expectWhatever UpdatedDatabase
        }

imageDecoder : Decoder Image
imageDecoder =
    succeed Image
        |> required "filename" string
        |> required "activated" bool
        |> required "crop_left" int
        |> required "crop_top" int
        |> required "crop_size" int

updateCurrentTextureSource : Model -> Model
updateCurrentTextureSource model =
    { model
    | currentTextureSource = Texture.loadFromImageUrl
        ( apiUrl
        ++ "/get_image/"
        ++ (getCurrentImage model).filename
        )
        GotTexture
    }

updateCurrentImage : Model -> Int -> Model
updateCurrentImage model index =
    case model.status of
        Loaded images ->
            updateCurrentTextureSource
                { model
                | currentImageIndex =
                    if Array.isEmpty images
                    then 0
                    else clamp 0 ((Array.length images) - 1) index
                }
        Loading -> model
        Errored _ -> model

updateCurrentImageProperties : Model -> Image -> Model 
updateCurrentImageProperties model image =
    case model.status of
        Loaded images ->
            { model
            | status = Loaded (Array.set model.currentImageIndex image images)
            }
        Loading -> model
        Errored _ -> model

getCurrentImage : Model -> Image
getCurrentImage model =
    case model.status of
        Loaded images ->
            Maybe.withDefault emptyImage
                <| Array.get model.currentImageIndex images
        Loading -> emptyImage
        Errored _ -> emptyImage

emptyImage : Image
emptyImage =
    { filename = ""
    , approved = False
    , crop_left = 0
    , crop_top = 0
    , crop_size = 0
    }

initialModel : Model
initialModel =
    { status = Loading
    , currentImageIndex = 0
    , currentTexture = Loading
    , currentTextureSource = Texture.loadFromImageUrl "img/loading.png" GotTexture
    }

initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = apiUrl ++ "/get_images"
        , expect = Http.expectJson GotImages (array imageDecoder)
        }

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \model -> onAnimationFrameDelta AnimationFrame
        }
