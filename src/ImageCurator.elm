module ImageCurator exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Http

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, title, type_, placeholder, value, checked)
import Html.Events exposing (onClick, onInput)

import Array exposing (Array)

import Json.Encode
import Json.Decode exposing (Decoder, int, array, string, bool, succeed)
import Json.Decode.Pipeline exposing (optional, required)

import Canvas exposing (rect, shapes, texture)
import Canvas.Texture as Texture exposing (Texture)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced exposing (rotate, transform, translate, scale)
import Color

apiUrl : String
apiUrl = "http://192.168.0.152:8080"

canvasSize : number
canvasSize = 1024

type Msg
    = GotImages (Result Http.Error (Array Image))
    | GotTexture (Maybe Texture)
    | UpdatedDatabase (Result Http.Error ())
    | SetCurrentImageStr String
    | PrevImage
    | NextImage
    | ToggleProcessed
    | ToggleApproved
    | SetCropLeft String
    | SetCropTop String
    | SetCropSize String
    | CropCenter
    | CropExtend
    | SaveProperties
    | AnimationFrame Float

type Load a
    = Loading
    | Loaded a
    | Errored String

type alias Image =
    { filename : String
    , processed : Bool
    , approved : Bool
    , cropLeft : Int
    , cropTop : Int
    , cropSize : Int
    }

type alias TextureProperties =
    { scale : Float
    , width : Int
    , height : Int
    , leftOffset : Int
    , topOffset : Int
    }

type alias Model =
    { status : Load (Array Image)
    , currentImageIndex : Int
    , currentTexture : Load Texture
    , currentTextureSource : Texture.Source Msg
    , currentTextureProperties : TextureProperties
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
        , viewCheckbox ToggleProcessed (getCurrentImage model).processed "Processed"
        , viewCheckbox ToggleApproved (getCurrentImage model).approved "Approved"
        , table [ class "crop-table" ]
            [ tr []
                [ td [] [ text "Crop Left" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.cropLeft)
                    , onInput SetCropLeft
                    ] [] ]
                ]
            , tr []
                [ td [] [ text "Crop Top" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.cropTop)
                    , onInput SetCropTop
                    ] [] ]
                ]
            , tr []
                [ td [] [ text "Crop Size" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.cropSize)
                    , onInput SetCropSize
                    ] [] ]
                ]
            ]
        , div [ class "crop-presets" ]
            [ viewButton [] CropCenter ( text "Center" ) 
            , viewButton [] CropExtend ( text "Extend" )
            ]
        , viewButton [] SaveProperties ( text "Save" )
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
            , canvasRenderImage model
            , canvasRenderCrop model
            ]
        ]

canvasClearScreen : Canvas.Renderable
canvasClearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) canvasSize canvasSize ]

canvasRenderImage : Model -> Canvas.Renderable
canvasRenderImage model =
    case model.currentTexture of
        Loaded texture_ ->
            let
                scale_ = model.currentTextureProperties.scale
                leftShift = (toFloat model.currentTextureProperties.leftOffset) * scale_
                topShift = (toFloat model.currentTextureProperties.topOffset) * scale_
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

canvasRenderCrop : Model -> Canvas.Renderable
canvasRenderCrop model =
    let
        currentImage = getCurrentImage model
        cropLeft =
            toFloat
                ( model.currentTextureProperties.leftOffset
                + currentImage.cropLeft
                )
            * model.currentTextureProperties.scale
        cropTop =
            toFloat
                ( model.currentTextureProperties.topOffset
                + currentImage.cropTop
                )
            * model.currentTextureProperties.scale
        cropSize = toFloat currentImage.cropSize * model.currentTextureProperties.scale
    in
    shapes [ stroke Color.red ] [ rect ( cropLeft, cropTop ) cropSize cropSize ]

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
        GotTexture (Just texture) ->
            let
                dimensions = Texture.dimensions texture
                maxDim = max dimensions.width dimensions.height
                scale = canvasSize / maxDim
            in
            (   { model
                | currentTexture = Loaded texture
                , currentTextureProperties =
                    { scale = scale
                    , width = round dimensions.width
                    , height = round dimensions.height
                    , leftOffset = floor ((maxDim - dimensions.width) / 2)
                    , topOffset = floor ((maxDim - dimensions.height) / 2)
                    }
                }
            , Cmd.none
            )
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
            , Cmd.none
            )
        ToggleProcessed ->
            ( updateCurrentImageProperties
                model
                { currentImage | processed = not currentImage.processed }
            , Cmd.none
            )
        SetCropLeft numString ->
            let
                newImage = { currentImage | cropLeft = (Maybe.withDefault currentImage.cropLeft (String.toInt numString)) }
            in
            ( updateCurrentImageProperties model newImage, Cmd.none )
        SetCropTop numString ->
            let
                newImage = { currentImage | cropTop = (Maybe.withDefault currentImage.cropTop (String.toInt numString)) }
            in
            ( updateCurrentImageProperties model newImage, Cmd.none )
        SetCropSize numString ->
            let
                newImage = { currentImage | cropSize = (Maybe.withDefault currentImage.cropSize (String.toInt numString)) }
            in
            ( updateCurrentImageProperties model newImage, Cmd.none )
        CropCenter ->
            let
                minDim = min
                    model.currentTextureProperties.width
                    model.currentTextureProperties.height
            in
            ( updateCurrentImageProperties model
                { currentImage
                | cropLeft = round (toFloat (model.currentTextureProperties.width - minDim) / 2)
                , cropTop = round (toFloat (model.currentTextureProperties.height - minDim) / 2)
                , cropSize = minDim
                }
            , Cmd.none
            )
        CropExtend ->
            let
                maxDim = max
                    model.currentTextureProperties.width
                    model.currentTextureProperties.height
            in
            ( updateCurrentImageProperties model
                { currentImage
                | cropLeft = negate model.currentTextureProperties.leftOffset
                , cropTop = negate model.currentTextureProperties.topOffset
                , cropSize = maxDim
                }
            , Cmd.none
            )
        SaveProperties -> ( model, updateDatabase <| getCurrentImage model )
        AnimationFrame dt -> ( model, Cmd.none )

updateDatabase : Image -> Cmd Msg
updateDatabase image =
    Http.post
        { url = apiUrl ++ "/update_properties"
        , body = Http.jsonBody <| imageEncode image
        , expect = Http.expectWhatever UpdatedDatabase
        }

imageEncode : Image -> Json.Encode.Value
imageEncode image =
    Json.Encode.object
        [ ( "filename", Json.Encode.string image.filename )
        , ( "processed", Json.Encode.bool image.processed )
        , ( "approved", Json.Encode.bool image.approved )
        , ( "crop_left", Json.Encode.int image.cropLeft )
        , ( "crop_top", Json.Encode.int image.cropTop )
        , ( "crop_size", Json.Encode.int image.cropSize )
        ]

imageDecoder : Decoder Image
imageDecoder =
    succeed Image
        |> required "filename" string
        |> required "processed" bool
        |> required "approved" bool
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
    , processed = False
    , approved = False
    , cropLeft = 0
    , cropTop = 0
    , cropSize = 0
    }

initialModel : Model
initialModel =
    { status = Loading
    , currentImageIndex = 0
    , currentTexture = Loading
    , currentTextureSource = Texture.loadFromImageUrl "img/loading.png" GotTexture
    , currentTextureProperties =
        { scale = 1.0
        , width = 0
        , height = 0
        , leftOffset = 0
        , topOffset = 0
        }
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
