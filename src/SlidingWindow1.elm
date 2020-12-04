module SlidingWindow1 exposing (main, init, update, view)
import Browser
import Html exposing (Html, Attribute, table, tr, td, div, button, text)
import Html.Attributes exposing (style, title, disabled)
import Html.Events exposing (onClick)
import Array exposing (Array)
import List exposing (map, range, length)



-- MAIN
main: Program () Model Msg
main = 
  Browser.sandbox {init = init, update = update, view = view}



-- MODEL
type alias Model = {
    p: Process Int,  -- process P
    q: Process Int,  -- process Q
    pq: Channel Int, -- channel P->Q
    qp: Channel Int  -- channel Q->P
  }

type alias Process t = {
    send: Array t,   -- packets to send
    recv: Array t,   -- packets recieved
    s:    Array Int, -- packet sent?
    r:    Array Int, -- packet recieved?
    w:    Int        -- window size (constant)
  }

type alias Channel t = {
    both: Array t,   -- packets recieved/to send
    s:    Array Int, -- packet sent?
    r:    Array Int, -- packet recieved?
    l:    Int        -- send limit (constant)
  }

type alias Packet t = {
    i: Int, -- number
    m: t    -- message
  }

init: Model
init =
  let data = [7, 5, 9, 4, 6, 3]
      n = length data
      p = initProcess 2 0 data
      q = initProcess 3 0 (map negate data)
      pq = initChannel 2 0 n
      qp = initChannel 2 0 n in
  Model p q pq qp

initProcess: Int -> t -> List t -> Process t
initProcess w d data =
  let n = length data
      send = Array.fromList data
      recv = Array.repeat n d
      z = Array.repeat n 0 in
  Process send recv z z w

initChannel: Int -> t -> Int -> Channel t
initChannel l d n =
  let b = Array.repeat n d
      z = Array.repeat n 0 in
  Channel b z z l



-- UPDATE
type Msg =
  SendP Int |
  SendQ Int |
  RecvP Int |
  RecvQ Int |
  Ignore Int

update: Msg -> Model -> Model
update msg model =
  let {p, q, pq, qp} = model in
  case msg of
    SendP i ->
      let (p1, x) = processSend 0 i p
          pq1 = channelRecv x pq in
      if processCanSend i p then {model | p = p1, pq = pq1} else model
    SendQ i ->
      let (q1, x) = processSend 0 i q
          qp1 = channelRecv x qp in
      if processCanSend i q then {model | q = q1, qp = qp1} else model
    RecvP i ->
      let (qp1, x) = channelSend 0 i qp
          p1 = processRecv x p in
      if channelCanSend i qp then {model | p = p1, qp = qp1} else model
    RecvQ i ->
      let (pq1, x) = channelSend 0 i pq
          q1 = processRecv x q in
      if channelCanSend i pq then {model | q = q1, pq = pq1} else model
    Ignore _ ->
      model

isDone: Model -> Bool
isDone model =
  let {p, q} = model in
  processIsDone p && processIsDone q

processIsDone: Process t -> Bool
processIsDone p =
  let {r} = p
      n = Array.length r
      rp = arraySearch 0 r in
  rp == n

processCanSend: Int -> Process t -> Bool
processCanSend i p =
  let (a, b) = processWindow p in
  i >= a && i < b

processWindow: Process t -> (Int, Int)
processWindow p =
  let {s, r, w} = p
      sp = arraySearch 0 s
      rp = arraySearch 0 r
      a = min sp rp in
  (a, a+w)

processSend: t -> Int -> Process t -> (Process t, Packet t)
processSend d i p =
  let {send, s} = p
      si = arrayGet 0 i s
      m = arrayGet d i send in
  ({p | s = Array.set i (si+1) s}, Packet i m)

processRecv: Packet t -> Process t -> Process t
processRecv x p =
  let {i, m} = x
      {recv, r} = p
      ri = arrayGet 0 i r in
  {p | recv = Array.set i m recv, r = Array.set i (ri+1) r}

channelCanSend: Int -> Channel t -> Bool
channelCanSend i c =
  let {s, r, l} = c
      si = arrayGet 0 i s
      ri = arrayGet 0 i r in
  ri > 0 && si < l

channelSend: t -> Int -> Channel t -> (Channel t, Packet t)
channelSend d i c =
  let {both, s} = c
      si = arrayGet 0 i s
      m = arrayGet d i both in
  ({c | s = Array.set i (si+1) s}, Packet i m)

channelRecv: Packet t -> Channel t -> Channel t
channelRecv x c =
  let {i, m} = x
      {both, r} = c
      ri = arrayGet 0 i r in
  {c | both = Array.set i m both, r = Array.set i (ri+1) r}



-- VIEW
view: Model -> Html Msg
view model =
  let {p, q, pq, qp} = model in
  table styleBody [
    tr [] [
      td styleP [text "P: send", viewProcess SendP p True],
      td styleC [text "PQ", viewChannel RecvQ pq],
      td styleQ [text "Q: recv", viewProcess Ignore q False]
    ],
    tr [] [
      td styleP [text "P: recv", viewProcess Ignore p False],
      td styleC [text "QP", viewChannel RecvP qp],
      td styleQ [text "Q: send", viewProcess SendQ q True]
    ],
    div styleSubmit [
      viewStatus (isDone model)
    ]
  ]

viewProcess: (Int -> Msg) -> Process Int -> Bool -> Html Msg
viewProcess msg p vs =
  let {send, recv, s, r} = p
      n = Array.length send
      (a, b) = processWindow p
      d = if vs then send else recv
      di i = arrayGet 0 i d
      si i = if vs then arrayGet 0 i s else 0
      ri i = if vs then 0 else arrayGet 0 i r
      ai i = vs && (i>=a && i<b)
      msgi i = if vs then msg i else Ignore i
      vmap i = viewPacket (di i) (si i) (ri i) (ai i) (msgi i) in
  div [] (map vmap (range 0 (n-1)))

viewChannel: (Int -> Msg) -> Channel Int -> Html Msg
viewChannel msg c =
  let {both, s, r} = c
      n = Array.length both
      di i = arrayGet 0 i both
      si i = arrayGet 0 i s
      ri i = arrayGet 0 i r
      ai i = channelCanSend i c
      msgi i = msg i
      vmap i = viewPacket (di i) (si i) (ri i) (ai i) (msgi i) in
  div [] (map vmap (range 0 (n-1)))

viewPacket: Int -> Int -> Int -> Bool -> msg -> Html msg
viewPacket m s r a msg = 
  button (stylePacket s r ++ [
    title ("Sent: "++String.fromInt s++" Recv: "++String.fromInt r),
    disabled (not a),
    onClick msg
  ]) [text (String.fromInt m)]

viewStatus: Bool -> Html Msg
viewStatus s =
  let m = if s then "All packets recieved." else "Some packets not yet recieved." in
  div (styleStatus s) [text m]



-- STYLES
stylePacket: Int -> Int -> List (Attribute m)
stylePacket s r = [
    style "outline" (String.fromInt (min r 2) ++ "px solid green"),
    style "outline" (String.fromInt (min s 2) ++ "px solid orange"),
    style "font-size" "1.5em",
    style "width" "2em"
  ]

styleP: List (Attribute m)
styleP = [style "background-color" "yellow"]

styleQ: List (Attribute m)
styleQ = [style "background-color" "yellow"]

styleC: List (Attribute m)
styleC = [style "background-color" "yellow"]

styleStatus: Bool -> List (Attribute m)
styleStatus s = [
    style "background-color" (if s then "green" else "yellow"),
    style "margin-top" "1em"
  ]

styleSubmit: List (Attribute m)
styleSubmit = [
    style "margin-top" "1em"
  ]

styleBody: List (Attribute m)
styleBody = [
    style "text-align" "center",
    style "margin" "2em",
    style "border-spacing" "1em"
  ]



-- UTIL
arrayGet: d -> Int -> Array d -> d
arrayGet d i x =
  Maybe.withDefault d (Array.get i x)

arraySearch: d -> Array d -> Int
arraySearch v x =
  let match u (i, f) = (i-1, if v==u then i-1 else f) in
  Tuple.second (Array.foldr match (Array.length x, Array.length x) x)
