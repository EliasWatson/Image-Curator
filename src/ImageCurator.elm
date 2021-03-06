module ImageCurator exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)

import Http
import Task

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, title, type_, placeholder, value, checked)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Dropdown

import Array exposing (Array)

import Json.Encode
import Json.Decode exposing (Decoder, int, array, string, bool, succeed)
import Json.Decode.Pipeline exposing (optional, required)

import Canvas exposing (rect, shapes, texture, path, lineTo)
import Canvas.Texture as Texture exposing (Texture)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineWidth)
import Canvas.Settings.Advanced exposing (rotate, transform, translate, scale, alpha)
import Color

apiUrl : String
apiUrl = "http://192.168.0.152:8080"

canvasSize : number
canvasSize = 1024

previewSize : number
previewSize = 256

type Msg
    = NoOp
    | GotImages (Result Http.Error (Array Image))
    | GotTexture (Maybe Texture)
    | UpdatedDatabase (Result Http.Error ())
    | SetCurrentImageStr String
    | PrevImage
    | NextImage
    | PrevUnprocessedImage
    | NextUnprocessedImage
    | FirstImage
    | LastImage
    | ToggleProcessed
    | ToggleApproved
    | SetCropLeft String
    | SetCropTop String
    | SetCropSize String
    | CropCenter
    | CropExtend
    | SetCropExtendMode ( Maybe String )
    | SaveProperties
    | AnimationFrame Float
    | KeyDown String

type Load a
    = Loading
    | Loaded a
    | Errored String

type ExtendMode
    = MirrorExtend
    | StretchExtend
    -- | SmearExtend
    | WhiteExtend
    | BlackExtend

type alias Image =
    { filename : String
    , processed : Bool
    , approved : Bool
    , cropLeft : Int
    , cropTop : Int
    , cropSize : Int
    , cropExtend : ExtendMode
    }

type ImageExtendAxis
    = None
    | Vertical
    | Horizontal

type alias TextureProperties =
    { scale : Float
    , width : Int
    , height : Int
    , leftOffset : Int
    , topOffset : Int
    , extendAxis : ImageExtendAxis
    , canvasPercent : Int
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
    div [ class "content" ] [ viewWorkspace model ]

viewNavigationBar : Model -> Html Msg
viewNavigationBar model =
    div [ class "navbar" ] 
        [ viewButton [] FirstImage (i [ class "fas fa-step-backward" ] [])
        , viewButton [] PrevUnprocessedImage (i [ class "fas fa-backward" ] [])
        , viewButton [] PrevImage (i [ class "fas fa-chevron-left" ] [])
        , input
            [ type_ "number", placeholder "Image Index"
            , value (String.fromInt (model.currentImageIndex + 1))
            , onInput SetCurrentImageStr
            , stopKeyPropagation
            ] []
        , viewButton [] NextImage (i [ class "fas fa-chevron-right" ] [])
        , viewButton [] NextUnprocessedImage (i [ class "fas fa-forward" ] [])
        , viewButton [] LastImage (i [ class "fas fa-step-forward" ] [])
        ]

viewWorkspace : Model -> Html Msg
viewWorkspace model =
    div [ class "workspace" ]
        [ div [ class "left-panel" ]
            [ viewProperties model
            , viewCropPreview model
            ]
        , viewImageViewer model
        ]

viewProperties : Model -> Html Msg
viewProperties model =
    let
        currentImage = getCurrentImage model
    in
    div [ class "panel properties" ]
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
                    , stopKeyPropagation
                    ] [] ]
                ]
            , tr []
                [ td [] [ text "Crop Top" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.cropTop)
                    , onInput SetCropTop
                    , stopKeyPropagation
                    ] [] ]
                ]
            , tr []
                [ td [] [ text "Crop Size" ]
                , td [] [ input
                    [ type_ "number"
                    , value (String.fromInt currentImage.cropSize)
                    , onInput SetCropSize
                    , stopKeyPropagation
                    ] [] ]
                ]
            ]
        , Dropdown.dropdown
            extendModeDropdownOptions
            [ class "crop-extend-mode" ]
            ( Just ( extendModeEncoder currentImage.cropExtend ) )
        , div [ class "crop-presets" ]
            [ viewButton [] CropCenter ( text "Center" ) 
            , viewButton [] CropExtend ( text "Extend" )
            ]
        , viewButton [] SaveProperties ( text "Save" )
        , viewNavigationBar model
        ]

viewImageViewer : Model -> Html Msg
viewImageViewer model =
    let
        currentImage = getCurrentImage model
    in
    div [ class "panel image-viewer" ]
        [ Canvas.toHtmlWith
            { width = canvasSize
            , height = canvasSize
            , textures = [ model.currentTextureSource ]
            }
            []
            <| canvasRenderFullSize model
        ]

viewCropPreview : Model -> Html Msg
viewCropPreview model =
    let
        currentImage = getCurrentImage model
    in
    div [ class "panel crop-preview" ]
        [ Canvas.toHtmlWith
            { width = previewSize
            , height = previewSize
            , textures = [ model.currentTextureSource ]
            }
            []
            <| canvasRenderPreview model
        ]

canvasRenderFullSize : Model -> List Canvas.Renderable
canvasRenderFullSize model =
    let
        leftShift = model.currentTextureProperties.leftOffset
        topShift = model.currentTextureProperties.topOffset
        size = max model.currentTextureProperties.width model.currentTextureProperties.height
    in 
    (( canvasClearScreen
    :: canvasRenderExtend 0.5 canvasSize leftShift topShift size model )
    ++ [ canvasRenderImage canvasSize leftShift topShift size model ]
    ++ [ canvasRenderCrop model ]
    )

canvasRenderPreview : Model -> List Canvas.Renderable
canvasRenderPreview model =
    let
        currentImage = getCurrentImage model
        leftShift = negate currentImage.cropLeft
        topShift = negate currentImage.cropTop
        size = currentImage.cropSize
    in 
    (( canvasClearScreen
    :: canvasRenderExtend 1.0 previewSize leftShift topShift size model )
    ++ [ canvasRenderImage previewSize leftShift topShift size model ]
    ++ ( if currentImage.approved then [] else [ canvasRenderX previewSize ] )
    )

canvasClearScreen : Canvas.Renderable
canvasClearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) canvasSize canvasSize ]

canvasRenderExtend : Float -> Int -> Int -> Int -> Int -> Model -> List Canvas.Renderable
canvasRenderExtend alpha_ canvasSize_ left top size model =
    let
        currentImage = getCurrentImage model
        scale_ = toFloat canvasSize_ / toFloat size
        leftShift = (toFloat left) * scale_
        topShift = (toFloat top) * scale_
        width = model.currentTextureProperties.width
        height = model.currentTextureProperties.height
    in 
    case model.currentTexture of
        Loaded texture_ ->
            case currentImage.cropExtend of
                MirrorExtend ->
                    case model.currentTextureProperties.extendAxis of
                        Horizontal ->
                            [ texture
                                [ transform
                                    [ translate leftShift topShift
                                    , scale ( negate scale_ ) scale_
                                    ]
                                , alpha alpha_
                                ]
                                ( 0, 0 )
                                texture_
                            , texture
                                [ transform
                                    [ translate ( leftShift + ( (toFloat width * 2) * scale_ ) ) topShift
                                    , scale ( negate scale_ ) scale_
                                    ]
                                , alpha alpha_
                                ]
                                ( 0, 0 )
                                texture_
                            ]
                        Vertical ->
                            [ texture
                                [ transform
                                    [ translate leftShift topShift
                                    , scale scale_ ( negate scale_ )
                                    ]
                                , alpha alpha_
                                ]
                                ( 0, 0 )
                                texture_
                            , texture
                                [ transform
                                    [ translate leftShift ( topShift + ( (toFloat height * 2) * scale_ ) )
                                    , scale scale_ ( negate scale_ )
                                    ]
                                , alpha alpha_
                                ]
                                ( 0, 0 )
                                texture_
                            ]
                        None -> []
                StretchExtend ->
                    [ texture
                        [ transform
                            [ scale
                                ( canvasSize / toFloat width )
                                ( canvasSize / toFloat height )
                            ]
                        , alpha alpha_
                        ]
                        ( 0, 0 )
                        texture_
                    ]
                WhiteExtend -> [ shapes [ fill Color.white ] [ rect ( 0, 0 ) ( toFloat canvasSize_ ) ( toFloat canvasSize_ ) ] ]
                BlackExtend -> [ shapes [ fill Color.black ] [ rect ( 0, 0 ) ( toFloat canvasSize_ ) ( toFloat canvasSize_ ) ] ]
        Loading -> []
        Errored _ -> []

canvasRenderImage : Int -> Int -> Int -> Int -> Model -> Canvas.Renderable
canvasRenderImage canvasSize_ left top size model =
    let
        scale_ = toFloat canvasSize_ / toFloat size
        leftShift = (toFloat left) * scale_
        topShift = (toFloat top) * scale_
        width = model.currentTextureProperties.width
        height = model.currentTextureProperties.height
    in 
    case model.currentTexture of
        Loaded texture_ ->
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
    shapes
        [ stroke Color.red
        , lineWidth 2
        ]
        [ rect ( cropLeft, cropTop ) cropSize cropSize ]

canvasRenderX : Int -> Canvas.Renderable
canvasRenderX canvasSize_ =
    let
        canvasSizeFloat = toFloat canvasSize_
    in
    shapes
        [ stroke Color.red
        , lineWidth 4
        ]
        [ path ( 0, 0 ) [ lineTo ( canvasSizeFloat, canvasSizeFloat ) ]
        , path ( canvasSizeFloat, 0 ) [ lineTo ( 0, canvasSizeFloat ) ]
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
        canvasPercent = model.currentTextureProperties.canvasPercent
    in
    case msg of
        NoOp -> ( model, Cmd.none )
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
                    , extendAxis =
                        if dimensions.width < dimensions.height then Horizontal
                        else if dimensions.height < dimensions.width then Vertical
                        else None
                    , canvasPercent = max 1 <| round ((canvasSize * 0.01) / scale)
                    }
                }
            , Cmd.none
            )
        UpdatedDatabase _ -> ( model, Cmd.none )
        SetCurrentImageStr indexString ->
            ( updateCurrentImageProcessed <| updateCurrentImage
                model
                ((Maybe.withDefault
                    (model.currentImageIndex + 1)
                    (String.toInt indexString)) - 1
                )
            , updateDatabase currentImage
            )
        PrevImage ->
            ( updateCurrentImageProcessed
                <| updateCurrentImage model (model.currentImageIndex - 1)
            , updateDatabase currentImage
            )
        NextImage ->
            ( updateCurrentImageProcessed
                <| updateCurrentImage model (model.currentImageIndex + 1)
            , updateDatabase currentImage
            )
        PrevUnprocessedImage ->
            case model.status of
                Loaded images ->
                    ( updateCurrentImageProcessed
                        <| updateCurrentImage model (getUnprocessedImage images -1 model.currentImageIndex)
                    , updateDatabase currentImage
                    )
                Loading -> ( model, Cmd.none )
                Errored _ -> ( model, Cmd.none )
        NextUnprocessedImage ->
            case model.status of
                Loaded images ->
                    ( updateCurrentImageProcessed
                        <| updateCurrentImage model (getUnprocessedImage images 1 model.currentImageIndex)
                    , updateDatabase currentImage
                    )
                Loading -> ( model, Cmd.none )
                Errored _ -> ( model, Cmd.none )
        FirstImage ->
            ( updateCurrentImageProcessed
                <| updateCurrentImage model 0
            , updateDatabase currentImage
            )
        LastImage ->
            case model.status of
                Loaded images -> ( updateCurrentImageProcessed <| updateCurrentImage model ((Array.length images) - 1), updateDatabase currentImage )
                Loading -> ( model, Cmd.none )
                Errored _ -> ( model, Cmd.none )
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
        SetCropExtendMode ( Just str ) ->
            ( updateCurrentImageProperties model
                { currentImage | cropExtend = extendModeDecoderString str }
            , Cmd.none
            )
        SetCropExtendMode Nothing -> ( model, Cmd.none )
        SaveProperties -> ( model, updateDatabase currentImage )
        AnimationFrame dt -> ( model, Cmd.none )
        KeyDown "ArrowLeft" -> ( updateCurrentImageProperties model { currentImage | cropLeft = currentImage.cropLeft - canvasPercent }, Cmd.none )
        KeyDown "ArrowRight" -> ( updateCurrentImageProperties model { currentImage | cropLeft = currentImage.cropLeft + canvasPercent }, Cmd.none )
        KeyDown "ArrowUp" -> ( updateCurrentImageProperties model { currentImage | cropTop = currentImage.cropTop - canvasPercent }, Cmd.none )
        KeyDown "ArrowDown" -> ( updateCurrentImageProperties model { currentImage | cropTop = currentImage.cropTop + canvasPercent }, Cmd.none )
        KeyDown "-" -> ( updateCurrentImageProperties model { currentImage | cropSize = currentImage.cropSize - canvasPercent }, Cmd.none )
        KeyDown "+" -> ( updateCurrentImageProperties model { currentImage | cropSize = currentImage.cropSize + canvasPercent }, Cmd.none )
        KeyDown "a" -> ( updateCurrentImageProcessed <| updateCurrentImage model (model.currentImageIndex - 1), updateDatabase currentImage )
        KeyDown "d" -> ( updateCurrentImageProcessed <| updateCurrentImage model (model.currentImageIndex + 1), updateDatabase currentImage )
        KeyDown "w" -> ( updateCurrentImageProperties model { currentImage | cropExtend = getPrevExtendMode currentImage.cropExtend }, Cmd.none )
        KeyDown "s" -> ( updateCurrentImageProperties model { currentImage | cropExtend = getNextExtendMode currentImage.cropExtend }, Cmd.none )
        KeyDown " " -> ( updateCurrentImageProperties model { currentImage | processed = not currentImage.processed }, Cmd.none )
        KeyDown "q" -> ( updateCurrentImageProperties model { currentImage | approved = not currentImage.approved }, Cmd.none )
        KeyDown "e" -> ( model, sendMsg CropExtend )
        KeyDown "r" -> ( model, sendMsg CropCenter )
        KeyDown "Enter" -> ( model, updateDatabase currentImage )
        KeyDown _ -> ( model, Cmd.none )

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
        , ( "crop_extend", Json.Encode.string <| extendModeEncoder image.cropExtend )
        ]

extendModeEncoder : ExtendMode -> String
extendModeEncoder mode =
    case mode of
        MirrorExtend -> "mirror"
        StretchExtend -> "stretch"
        WhiteExtend -> "white"
        BlackExtend -> "black"

imageDecoder : Decoder Image
imageDecoder =
    succeed Image
        |> required "filename" string
        |> optional "processed" bool False
        |> optional "approved" bool False
        |> optional "crop_left" int 0
        |> optional "crop_top" int 0
        |> optional "crop_size" int 0
        |> optional "crop_extend" extendModeDecoder MirrorExtend

extendModeDecoder : Decoder ExtendMode
extendModeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str -> Json.Decode.succeed <| extendModeDecoderString str)

extendModeDecoderString : String -> ExtendMode
extendModeDecoderString str =
    case str of
        "mirror" -> MirrorExtend
        "stretch" -> StretchExtend
        "white" -> WhiteExtend
        "black" -> BlackExtend
        _ -> MirrorExtend

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

updateCurrentImageProcessed : Model -> Model
updateCurrentImageProcessed model =
    let
        currentImage = getCurrentImage model
    in
    updateCurrentImageProperties model { currentImage | processed = True }

getCurrentImage : Model -> Image
getCurrentImage model =
    case model.status of
        Loaded images ->
            Maybe.withDefault emptyImage
                <| Array.get model.currentImageIndex images
        Loading -> emptyImage
        Errored _ -> emptyImage

getUnprocessedImage : Array Image -> Int -> Int -> Int
getUnprocessedImage images dir pos =
    case Array.get pos images of
        Just image ->
            if not image.processed
            then pos
            else getUnprocessedImage images dir (pos + dir)
        Nothing -> pos - dir    --This isn't the best solution. It could cause issues with 0-length arrays

getNextExtendMode : ExtendMode -> ExtendMode
getNextExtendMode mode =
    case mode of
        MirrorExtend -> StretchExtend
        StretchExtend -> WhiteExtend
        WhiteExtend -> BlackExtend
        BlackExtend -> MirrorExtend

getPrevExtendMode : ExtendMode -> ExtendMode
getPrevExtendMode mode =
    case mode of
        MirrorExtend -> BlackExtend
        StretchExtend -> MirrorExtend
        WhiteExtend -> StretchExtend
        BlackExtend -> WhiteExtend

sendMsg : msg -> Cmd msg
sendMsg msg =
    Task.succeed msg
    |> Task.perform identity

extendModeDropdownOptions : Dropdown.Options Msg
extendModeDropdownOptions =
    let
        defaultOptions = Dropdown.defaultOptions SetCropExtendMode
    in
    { defaultOptions
    | items = 
        [ { value = "mirror", text = "Mirror", enabled = True }
        , { value = "stretch", text = "Stretch", enabled = True }
        , { value = "white", text = "White", enabled = True }
        , { value = "black", text = "Black", enabled = True }
        ]
    }

stopKeyPropagation : Attribute Msg
stopKeyPropagation =
    stopPropagationOn "keydown" ( Json.Decode.succeed ( NoOp, True ) )

emptyImage : Image
emptyImage =
    { filename = ""
    , processed = False
    , approved = False
    , cropLeft = 0
    , cropTop = 0
    , cropSize = 0
    , cropExtend = BlackExtend
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
        , extendAxis = None
        , canvasPercent = 1
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
        , subscriptions = \model -> Sub.batch
            [ onAnimationFrameDelta AnimationFrame
            , Browser.Events.onKeyDown ( Json.Decode.field "key" Json.Decode.string |> Json.Decode.map KeyDown )
            ]
        }
