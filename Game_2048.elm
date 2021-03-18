port module Game_2048 exposing (..)

-- Add/modify imports if you'd like. ---------------------------------

import Browser
import Browser.Events
import Json.Decode as Decode
import List
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Random exposing (Generator)
import Time
import Debug
import Svg exposing (Svg)
import Svg.Attributes

----------------------------------------------------------------------

main : Program Flags Model Msg
main = 
 Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- ports to implement high score feature --
port saveScore : {- forall msg. -} Int -> Cmd msg
port loadScoreRequest : {- forall msg. -} () -> Cmd msg
port loadScoreReceive : {- forall msg. -} (Int -> msg) -> Sub msg    

-----------------------------      MODEL       ------------------------

type Model = Play CurrentInfo 
  | GameOver Result CurrentInfo

type alias CurrentInfo =
 {
    score : Int,
    tiles : List Tile,
    high : Int
 }   

type Result = Win | Lose

type alias Flags = () 

type alias Tile = 
 {
   row : Int,
   col : Int,
   value : Int
 } 

init : Flags -> (Model, Cmd Msg)
init currentTime = (initModel currentTime 0, Cmd.batch initCmds)

initModel : Flags -> Int -> Model 
initModel currentTime high = 
 Play (newInfo high)

initCmds = [ loadScoreRequest(), genNewBoard ]


newInfo : Int -> CurrentInfo
newInfo best = 
 { score = 0, tiles = [], high = best} 

type Msg = NewGame | Grid (List Tile) | KeyDown String | LoadScoreReceive Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
 case model of 
  Play info -> playUpdate msg info 
  GameOver res info ->
   end msg res info


playUpdate : Msg -> CurrentInfo -> (Model, Cmd Msg)
playUpdate msg model = 
 case msg of 
  LoadScoreReceive num -> 
   let 
    newModel = { model | score = 0, high = num }
   in 
    (Play newModel, Cmd.none) 
  KeyDown key ->
   case key of 
    "ArrowUp" -> 
     if checkMoves Up model.tiles then 
      let 
       (newTiles, newScore) = move Up model.tiles model.score
      --  newScore = scoreCol newTiles model.score
       newModel = { model | score = newScore, tiles = newTiles }
      in 
       (Play newModel, genNextBoard newTiles) 
     else
      case findEmpty model.tiles of 
       [] -> updateScore model.score model.high model    
       _ -> (Play model, Cmd.none) 
    "ArrowDown" ->
     if checkMoves Down model.tiles then 
      let 
       (newTiles, newScore) = move Down model.tiles model.score
      --  newScore = scoreCol newTiles model.score
       newModel = { model | score = newScore, tiles = newTiles }
      in (Play newModel, genNextBoard newTiles)  
     else 
      case findEmpty model.tiles of 
       [] -> updateScore model.score model.high model    
       _ -> (Play model, Cmd.none)      
    "ArrowLeft" ->
     if checkMoves Left model.tiles then 
      let 
       (newTiles, newScore) = move Left model.tiles model.score
      --  newScore = scoreRow newTiles model.score
       newModel = { model | score = newScore, tiles = newTiles }
      in 
       (Play newModel, genNextBoard newTiles)
     else
      case findEmpty model.tiles of 
       [] -> 
        updateScore model.score model.high model    
       _ -> (Play model, Cmd.none)     
    "ArrowRight" ->
     if checkMoves Right model.tiles then 
      let 
       (newTiles, newScore) = move Right model.tiles model.score
       newModel = { model | score = newScore, tiles = newTiles }
      in
       (Play newModel, genNextBoard newTiles)   
     else 
      case findEmpty model.tiles of 
       [] -> updateScore model.score model.high model   
       _ -> (Play model, Cmd.none)
    s -> 
     case findEmpty model.tiles of 
      [] -> updateScore model.score model.high model    
      _ -> (Play model, Cmd.none)
  NewGame -> 
   let 
    newModel = {model | score = 0}
   in 
    (Play newModel, Cmd.batch initCmds)
  Grid tiles -> 
   let 
    h_score = model.high 
    new = model.score
    newModel = {model | tiles = tiles}
   in 
    if canMove tiles then (Play newModel, Cmd.none) 
    else updateScore new h_score model

end : Msg -> Result -> CurrentInfo -> (Model, Cmd Msg)
end msg outcome info =
  case msg of
    NewGame ->
     let 
      updateInfo = { info | score = 0}
     in 
      (Play updateInfo, genNewBoard)
    _ ->
      (GameOver outcome info, Cmd.none)     

-- if True, win game --
contains2048 : List Tile -> Bool
contains2048 tiles = 
 List.any (\t -> t.value == 2048) tiles       
    

-----------------------------      VIEW       -------------------------

view : Model -> Html Msg
view model =
 case model of 
  Play info -> viewPlay info 
  GameOver res info -> viewGameOver res info

viewPlay : CurrentInfo -> Html Msg 
viewPlay model = 
 let 
  gameboard = boardLayout model.tiles
 in  
  Html.div 
  [ Html.Attributes.style "text-align" "center"
  , Html.Attributes.style "align-content" "center"
  , Html.Attributes.style "justify-content" "center"
  , Html.Attributes.style "font-family" "Helvetica"
  , Html.Attributes.style "color" "#717171"]
  [ h1 [Html.Attributes.class "title"] [ Html.text "2048" ]
  , Html.div [Html.Attributes.class "score-info"] 
    [ Html.p [] [ Html.text ("Score: " ++ (String.fromInt model.score)) ]
    , Html.p [] [ Html.text ("High Score: " ++ (String.fromInt model.high)) ]
    ] 
  , gameboard
  , Html.div [Html.Attributes.class "info"] 
    [ Html.p [] [ Html.text ("A beachy remake of the class game 2048 by Ellen Chen")] ]
  ]

viewGameOver : Result -> CurrentInfo -> Html Msg 
viewGameOver res model = 
 case res of 
  Win ->
   Html.div 
   [ Html.Attributes.style "text-align" "center"
   , Html.Attributes.style "align-content" "center"
   , Html.Attributes.style "justify-content" "center"
   , Html.Attributes.style "font-family" "Helvetica"]
   [ Html.div [Html.Attributes.class "endscreen"]
     [Html.h1 [Html.Attributes.class "endscreen-win"] [ Html.text ("YOU WON! :)")]
     , Html.p [] [ Html.text ("Score: " ++ (String.fromInt model.score))]
     , Html.div []
       [Html.button 
        [ Html.Events.onClick NewGame
        , Html.Attributes.class "playagain-btn" ] 
        [ text "Play Again" ]]]
   ]  
  Lose -> 
   Html.div
   [ Html.Attributes.style "text-align" "center"
   , Html.Attributes.style "align-content" "center"
   , Html.Attributes.style "justify-content" "center"
   , Html.Attributes.style "font-family" "Helvetica"]
   [ Html.div [Html.Attributes.class "endscreen"]
     [Html.h1 [Html.Attributes.class "endscreen-lose"] [ Html.text ("YOU LOST! :(")]
     , Html.p [] [ Html.text ("Score: " ++ (String.fromInt model.score))]
     , Html.div []
       [Html.button 
        [ Html.Events.onClick NewGame
        , Html.Attributes.class "playagain-btn" ] 
        [ text "Play Again" ]]]
   ]  

-----------------------------      GRID       -------------------------
boardLayout : List Tile  -> Html Msg 
boardLayout tiles = 
 Svg.svg 
  [ Svg.Attributes.width "500"
  , Svg.Attributes.height "500"
  , Svg.Attributes.viewBox ("0 0 635 635")
  , Svg.Attributes.style ("background: " ++ "#aaaaaa")]
  [
    gridLayout,
    tileRow tiles
  ]

gridLayout : Svg msg
gridLayout =
 let 
  xs = [0, 1, 2, 3]
  ys = [0, 1, 2, 3]
  xy_cart = cartesian xs ys
  cells = List.map (\(x, y) -> cellLayout x y) xy_cart
 in 
  Svg.g [Svg.Attributes.fill "#E6E3E3"] cells

cellLayout : Int -> Int -> Svg msg
cellLayout row col = 
 let 
  x = cellDistance row
  y = cellDistance col
 in 
  -- Debug.log(Debug.toString row)
  Svg.rect 
  [ Svg.Attributes.x (String.fromInt x)
  , Svg.Attributes.y (String.fromInt y)
  ,  Svg.Attributes.width "140"
  , Svg.Attributes.height "140"]
  []  

-- cell size of 140 px, spacing of 10 px --
cellDistance : Int -> Int 
cellDistance n = 155 * n + 15  

-- helper function in building grid --
cartesian : List a -> List b -> List (a, b)
cartesian xs ys =
 List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs
  
tileLayout : Tile -> Html Msg 
tileLayout tile =
 let 
  num = tile.value
 in 
 Html.div 
  [Html.Attributes.class ("Cell TileNum" ++ Debug.toString num)] 
  [tileLayoutSVG tile]   

tileLayoutSVG : Tile -> Svg msg
tileLayoutSVG tile = 
 let 
  x = cellDistance tile.col
  y = cellDistance tile.row
 in 
  Svg.g [] [
   Svg.rect 
    [ Svg.Attributes.x (String.fromInt x)
    , Svg.Attributes.y (String.fromInt y)
    , Svg.Attributes.width "140"
    , Svg.Attributes.height "140"
    , Svg.Attributes.fill (tileColor tile.value)]
    []
  , Svg.text_
   [ Svg.Attributes.x (String.fromInt (x + textDistance tile.value))
   , Svg.Attributes.y (String.fromInt (y + 85))
   , Svg.Attributes.fontSize "40"
   , Svg.Attributes.fontWeight "bold"
   , Svg.Attributes.fill "#717171"
   , Svg.Attributes.fontFamily "Helvetica"]
   [ Svg.text (String.fromInt tile.value)]
  ]

tileRow : List Tile -> Svg msg 
tileRow tiles = 
 let 
  row = List.map tileLayoutSVG tiles 
 in 
  Svg.g [] row

-- each tile is a different color --
tileColor : Int -> String
tileColor tile =
 case tile of
  0 -> "#f4f9f9"
  2 -> "#E0F9F9"
  4 -> "#D3FBFB"
  8 -> "#ccf2f4"
  16 -> "#BBF3F5"
  32 -> "#a4ebf3"
  64 -> "#9EEFFC"
  128 -> "#94EAF7"
  256 -> "#7FE0EF"
  512 -> "#77DCF8"
  1024 -> "#50C8F9"
  2048 -> "#38BFF6"
  4096 -> "#FEF7A9"
  _ -> "white" -- should never occur --


-- formatting for numbers in tiles -- 
textDistance : Int -> Int
textDistance tile =
 if tile < 10 then 60
 else if tile < 100 then 50
 else if tile < 1000 then 38
 else 28

-----------------------------      SETUP       -------------------------
---  derived inspiration from https://github.com/dwayne/elm-2048    ---  

-- generate new tile -- 
newVal : Random.Generator Int 
newVal = Random.weighted (80, 2) [(20, 4)]

-- find empty tiles --
findEmpty : List Tile -> List (Int, Int)
findEmpty tiles = 
 let 
  x = [0, 1, 2, 3]
  y = [0, 1, 2, 3]
  all = cartesian x y
 in 
  List.filter 
   (\(row, col) -> List.all (\t -> t.row /= row || t.col /= col) tiles) 
   all

-- check if (row, col) is empty --
isEmpty : Int -> Int -> List Tile -> Bool    
isEmpty row col tiles = 
 List.all (\t -> t.row /= row || t.col /= col) tiles

getInd : Int -> List (Int, Int) -> (Int, Int)
getInd ind empties =
 case (ind, empties) of 
  (_, []) -> (0, 0)
  (0, head :: _) -> head
  (_, _ :: rest) -> getInd (ind-1) rest

-- generate one tile --
genOne : List Tile -> Generator Tile 
genOne tiles = 
 let 
  empties = findEmpty tiles 
  len = List.length empties - 1
 in 
  Random.map (\(i, num) ->
   let 
    (x, y) = getInd i empties 
   in { row = x, col = y, value = num }) 
    (Random.pair (Random.int 0 len) newVal)

-- new Board will generate two tiles --
newBoard : List Tile -> Generator (List Tile)
newBoard tiles = 
 genOne tiles 
  |> Random.andThen 
   (\t -> genOne (t :: tiles)
    |> Random.map (\t2 -> [t, t2]))

genNewBoard : Cmd Msg
genNewBoard = Random.generate Grid (newBoard [])

genNextBoard : List Tile -> (Cmd Msg)
genNextBoard tiles = 
 case findEmpty tiles of 
  [] -> Cmd.none
  _ -> Random.generate Grid (nextBoard tiles)

-- Board will generate a new tile with each play --
nextBoard : List Tile -> Generator (List Tile)
nextBoard tiles = 
 genOne tiles |> Random.map (\t -> t :: tiles)

--------------------------------      MOVES       -------------------------

type Direction = Up | Down | Left | Right

move : Direction -> List Tile -> Int -> (List Tile, Int)
move dir tiles score = 
   case dir of 
    Up -> up tiles score
    Down -> down tiles score 
    Left -> left tiles score 
    Right -> right tiles score

-- group Tiles by row --
groupRow : Int -> List Tile -> List Tile
groupRow row tiles =
 List.sortBy .col (List.filter (\t -> t.row == row) tiles) 
 
-- group Tiles by col -- 
groupCol : Int -> List Tile -> List Tile
groupCol row tiles =
 List.sortBy .row (List.filter (\t -> t.col == row) tiles)  

-- combine tiles to increment values --
merge : List Tile -> (List Tile, Int)
merge tiles = 
 case tiles of 
  [] -> ([], 0)
  (t1 :: t2 :: rest) ->
   if (t1.value == t2.value) then 
    let 
     tile = 
      { row = t1.row
      , col = t1.col
      , value = 2 * t1.value }
     (merged, score) = merge rest  
    in (tile :: merged, tile.value + score) 
   else 
    let 
     (merged, score) = merge (t2 :: rest)
    in    
    (t1 :: merged, score) 
  (t1 :: rest) -> 
   let 
     (merged, score) = merge rest
    in    
    (t1 :: merged, score)

-- shift tiles if there are empty spaces --

shiftLeft : List Tile -> Int -> Int -> List Tile 
shiftLeft tiles row y = 
 case tiles of 
  [] -> []
  (t1 :: rest) ->
   ({t1 | col = y }) :: shiftLeft rest row (y+1)

shiftRight : List Tile -> Int -> Int -> List Tile 
shiftRight tiles row y = 
 case tiles of 
  [] -> []
  (t1 :: rest) ->
   ({t1 | col = y }) :: shiftRight rest row (y-1)   

shiftUp : List Tile -> Int -> Int -> List Tile 
shiftUp tiles x col = 
 case tiles of 
  [] -> []
  (t1 :: rest) ->
   ({t1 | row = x }) :: shiftUp rest (x+1) col   

shiftDown : List Tile -> Int -> Int -> List Tile 
shiftDown tiles x col = 
 case tiles of 
  [] -> []
  (t1 :: rest) ->
   ({t1 | row = x }) :: shiftDown rest (x-1) col    

sort : List Tile -> List Tile 
sort tiles = 
 let 
  r0 = List.sortBy .col (groupRow 0 tiles)     
  r1 = List.sortBy .col (groupRow 1 tiles)  
  r2 = List.sortBy .col (groupRow 2 tiles)  
  r3 = List.sortBy .col (groupRow 3 tiles)  
 in r0 ++ r1 ++ r2 ++ r3  

left : List Tile -> Int -> (List Tile, Int) 
left tiles score = 
 let 
  (row0, s0) = merge (groupRow 0 tiles)
  (row1, s1) = merge (groupRow 1 tiles)
  (row2, s2) = merge (groupRow 2 tiles)
  (row3, s3) = merge (groupRow 3 tiles)
  sum = s0 + s1 + s2 + s3 + score
  shifted0 = List.sortBy .col (shiftLeft row0 0 0)
  shifted1 = List.sortBy .col (shiftLeft row1 0 0)
  shifted2 = List.sortBy .col (shiftLeft row2 0 0)
  shifted3 = List.sortBy .col (shiftLeft row3 0 0)
 in (shifted0 ++ shifted1 ++ shifted2 ++ shifted3, sum)

right : List Tile -> Int -> (List Tile, Int) 
right tiles score = 
 let
  (row0, s0) = merge (List.reverse (groupRow 0 tiles))
  (row1, s1) = merge (List.reverse (groupRow 1 tiles))
  (row2, s2) = merge (List.reverse (groupRow 2 tiles))
  (row3, s3) = merge (List.reverse (groupRow 3 tiles))
  sum = s0 + s1 + s2 + s3 + score
  shifted0 = List.sortBy .col (shiftRight row0 0 3)
  shifted1 = List.sortBy .col (shiftRight row1 1 3)
  shifted2 = List.sortBy .col (shiftRight row2 2 3)
  shifted3 = List.sortBy .col (shiftRight row3 0 3)
 in (shifted0 ++ shifted1 ++ shifted2 ++ shifted3, sum)


up : List Tile -> Int -> (List Tile, Int) 
up tiles score = 
 let 
  (col0, s0) = merge (groupCol 0 tiles)
  (col1, s1) = merge (groupCol 1 tiles)
  (col2, s2) = merge (groupCol 2 tiles)
  (col3, s3) = merge (groupCol 3 tiles)
  sum = s0 + s1 + s2 + s3 + score
  shifted0 = List.sortBy .row (shiftUp col0 0 0)
  shifted1 = List.sortBy .row (shiftUp col1 0 1)
  shifted2 = List.sortBy .row (shiftUp col2 0 2)
  shifted3 = List.sortBy .row (shiftUp col3 0 3)
  overall = sort (shifted0 ++ shifted1 ++ shifted2 ++ shifted3)
 in (overall, sum)

down : List Tile -> Int -> (List Tile, Int)
down tiles score = 
 let
  (col0, s0) = merge (List.reverse (groupCol 0 tiles))
  (col1, s1) = merge (List.reverse (groupCol 1 tiles))
  (col2, s2) = merge (List.reverse (groupCol 2 tiles))
  (col3, s3) = merge (List.reverse (groupCol 3 tiles))
  sum = s0 + s1 + s2 + s3 + score
  shifted0 = List.sortBy .row (shiftDown col0 3 0)
  shifted1 = List.sortBy .row (shiftDown col1 3 1)
  shifted2 = List.sortBy .row (shiftDown col2 3 2)
  shifted3 = List.sortBy .row (shiftDown col3 3 3)
  overall = sort (shifted0 ++ shifted1 ++ shifted2 ++ shifted3)
 in (overall, sum)

checkMoves : Direction -> List Tile -> Bool
checkMoves dir tiles = 
 let 
  (moved, _) = move dir tiles 0
 in 
  moved /= tiles 

-- check for valid moves --
canMove : List Tile -> Bool 
canMove tiles = 
 let 
  dirs = [Up, Down, Left, Right]
 in
  List.any (\x -> x == True) (List.map (\d -> checkMoves d tiles) dirs)

-- updates high score if necessary and calls end screen --
updateScore : Int -> Int -> CurrentInfo -> (Model, Cmd Msg) 
updateScore new h_score info =
  if (new > h_score) then 
    if contains2048 info.tiles then 
      (GameOver Win info, saveScore new)
    else (GameOver Lose info, saveScore new)
  else
    if contains2048 info.tiles then 
      (GameOver Win info, saveScore h_score)
    else (GameOver Lose info, saveScore h_score)   
 
-----------------------------      SUBS       -------------------------  

keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
 Sub.batch
  [ Browser.Events.onKeyDown
     (Decode.map (\key -> KeyDown key) keyDecoder)
    , loadScoreReceive LoadScoreReceive
  ]   