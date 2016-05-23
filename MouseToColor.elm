module MouseToColor exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes
import Mouse
import Window
import Task


type alias Position = { x: Int, y: Int }
type alias Size = { height: Int, width: Int }
type alias Distance = Float
type alias Angle = Float


type alias Model =
  {
    position: Position,
    center: Position,
    color: String,
    textColor: String,
    text: String
  }

type Msg =
  Move Position | Click | Resize Size


main: Program Never
main =
  Html.App.program {
          init = init,
          subscriptions = subscriptions,
          update = update,
          view = view
        }



decimalToHex: Int -> String
decimalToHex number =
  if number < 10
  then toString(number)
  else case number of
         10 ->
           "a"
         11 ->
           "b"
         12 ->
           "c"
         13 ->
           "d"
         14 ->
           "e"
         15 ->
           "f"
         _ ->
           "_"


byteToHex: Int -> String
byteToHex byte =
  (decimalToHex (byte // 16)) ++ (decimalToHex (byte % 16))


rgbToHexString: (Int, Int, Int) -> String
rgbToHexString (r, g, b) =
  (byteToHex r) ++ (byteToHex g) ++ (byteToHex b)


-- H (0 - 360), S (0 - 1) V (1)
-- http://www.splinter.com.au/converting-hsv-to-rgb-colour-using-c/
hsvToRgb: Float -> Float -> Float -> (Int, Int, Int)
hsvToRgb hue saturation value =
  let
    hf = hue / 60
    i = floor hf
    f = hf - toFloat(i)
    pv = value * (1 - saturation)
    qv = value * (1 - saturation * f)
    tv = value * (1 - saturation * (1 - f))
    (r, g, b) = case i of
                  0 ->
                    (value, tv, pv)
                  1 ->
                    (qv, value, pv)
                  2 ->
                    (pv, value, tv)
                  3 ->
                    (pv, qv, value)
                  4 ->
                    (tv, pv, value)
                  5 ->
                    (value, pv, qv)
                  6 ->
                    (value, tv, pv)
                  _ ->
                    (0, 0, 0)
    clampToByte = round << clamp 0 255 << (*) 255
  in
    (clampToByte r, clampToByte g, clampToByte b)


getCenter: Size -> Position
getCenter size =
  Position (size.width // 2) (size.height // 2)


fromCenter: Position -> Position -> (Distance, Angle)
fromCenter center position =
  let
    y = toFloat(position.y - center.y)
    x = toFloat(position.x - center.x)
    distance = sqrt (x ^ 2 + y ^ 2)
    angle = atan2 y x
    makePositive = (\a -> if a < 0 then 360 + a else a)
    degrees = makePositive (angle * (180 / pi))
  in
    (distance, degrees)


-- Use Task.perform to convert Window.size task into a Cmd
init: (Model, Cmd Msg)
init =
  let
    initialSizeCmd = Task.perform Resize Resize Window.size
  in
    ({position = Position 0 0,
      center = Position 0 0,
      color = "#fff",
      textColor = "#000",
      text = "test"
    }, initialSizeCmd)


subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch [
        Mouse.moves Move,
        Window.resizes Resize
       ]


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move position ->
      let
        (distance, angle) = fromCenter model.center position
        relativeDistance = min (distance / toFloat(model.center.x)) 1.0
        color = rgbToHexString (hsvToRgb angle relativeDistance 1.0)
        textColor = rgbToHexString (hsvToRgb angle relativeDistance 0.8)
      in
        ({model |
            position = position,
            color = "#" ++ color,
            textColor = "#" ++ textColor,
            text = toString(angle) ++ "     Dist:" ++ toString(relativeDistance) ++ "  COLOR: " ++ color
         }, Cmd.none)

    Click ->
      (model, Cmd.none)

    Resize size ->
      ({model |
          center = getCenter size
       }, Cmd.none)


view: Model -> Html Msg
view model =
  let
    styles = Html.Attributes.style
             [("font-size", "50px")
             ,("color", model.textColor)]
    divText = (\t -> div [styles] [text t])
  in
    div
      [Html.Attributes.style
         [("background", model.color)
         ,("position", "absolute")
         ,("top", "0")
         ,("left", "0")
         ,("bottom", "0")
         ,("right", "0")
         ,("display", "flex")
         ,("flex-direction", "column")
         ,("justify-content", "center")
         ,("align-items", "center")
         ,("font-size", "14px")]]
      [
       divText model.color]
