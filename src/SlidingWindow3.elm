module SlidingWindow3 exposing (main, init, update, view)
import Browser
import Html exposing (Html, Attribute, node, text, div, button, table, tr, td)
import Html.Attributes exposing (rel, href, style, class, title, disabled)
import Html.Events exposing (onClick)
import Array exposing (Array)
import List exposing (length, map, filter, foldl, range)
import Round



-- MAIN
main: Program () Model Msg
main = 
  Browser.sandbox {init = init, update = update, view = view}



-- MODEL
type alias Model = {
    -- components
    p:  Process Int, -- process P
    q:  Process Int, -- process Q
    pq: Channel Int, -- channel P->Q
    qp: Channel Int, -- channel Q->P
    -- operation
    who: Array Int,  -- whose turn
    t:  Int          -- turn
  }

type alias Process t = {
    -- components
    send: Array t, -- packets to send
    recv: Array t, -- packets recieved
    st: Array Int, -- packet send turn
    rt: Array Int, -- packet received turn
    s: Array Int,  -- packet sent?
    r: Array Int,  -- packet recieved?
    -- operation
    mode: String,  -- acknowldgement process?
    w: Int,        -- window size (constant)
    t: Int         -- turn
  }

type alias Channel t = {
    -- components
    both: Array t, -- packets recieved/to send
    st: Array Int, -- packet send turn
    rt: Array Int, -- packet received turn
    s: Array Int,  -- packet sent?
    r: Array Int,  -- packet recieved?
    -- operation
    sb: Int,       -- send start
    se: Int,       -- send limit
    t: Int         -- turn
  }

type alias Packet t = {
    i: Int, -- number
    m: t    -- message
  }

init: Model
init =
  let data = range 1 54
      n = length data
      p = initProcess 0 "def" 9 data
      q = initProcess 0 "ack" 9 (map negate data)
      pq = initChannel 0 2 5 n
      qp = initChannel 0 2 5 n
      who = Array.fromList [3, 2, 1, 0] in
  Model p q pq qp who 0

initProcess: t -> String -> Int -> List t -> Process t
initProcess d mode win data =
  let n = length data
      send = Array.fromList data
      recv = Array.repeat n d
      z = Array.repeat n 0 in
  Process send recv z z z z mode win 0

initChannel: t -> Int -> Int -> Int -> Channel t
initChannel d sb se n =
  let b = Array.repeat n d
      z = Array.repeat n 0 in
  Channel b z z z z sb se 0



-- UPDATE
type Msg =
  SendP Int |
  SendQ Int |
  RecvP Int |
  RecvQ Int |
  Next Int

update: Msg -> Model -> Model
update msg model =
  let {p, q, pq, qp, t} = model in
  case msg of
    SendP i ->
      let (p1, x) = processSend 0 t i p
          pq1     = channelRecv t x pq in
      if processCanSend i p then {model | p = p1, pq = pq1, t = t+1} else {model | t = t+1}
    SendQ i ->
      let (q1, x) = processSend 0 t i q
          qp1     = channelRecv t x qp in
      if processCanSend i q then {model | q = q1, qp = qp1, t = t+1} else {model | t = t+1}
    RecvP i ->
      let (qp1, x) = channelSend 0 t i qp
          p1       = processRecv t x p in
      if channelCanSend i qp then {model | p = p1, qp = qp1, t = t+1} else {model | t = t+1}
    RecvQ i ->
      let (pq1, x) = channelSend 0 t i pq
          q1       = processRecv t x q in
      if channelCanSend i pq then {model | q = q1, pq = pq1, t = t+1} else {model | t = t+1}
    Next i ->
      if i == 1 then
      case whoseTurn model of
        0 -> update (SendP (processToSend p))  model
        2 -> update (SendQ (processToSend q))  model
        3 -> update (RecvP (channelToSend qp)) model
        _ -> update (RecvQ (channelToSend pq)) model
      else if isDone model then model
      else update (Next (i-1)) (update (Next 1) model)

whoseTurn: Model -> Int
whoseTurn model =
  let {who, t} = model
      i = modBy (Array.length who) t in
  arrayGet 0 i who

isDone: Model -> Bool
isDone model =
  let {p, q} = model in
  processIsDone p && processIsDone q



processSize: Process t -> Int
processSize p =
  let {send} = p in
  Array.length send

processSent: Process t -> Int
processSent p =
  let {s} = p in
  arraySum (Array.map sign s)

processSentAll: Process t -> Int
processSentAll p =
  let {s} = p in
  arraySum s

processRcvd: Process t -> Int
processRcvd p =
  let {r} = p in
  arraySum (Array.map sign r)

processRcvdAll: Process t -> Int
processRcvdAll p =
  let {r} = p in
  arraySum r

processDeltaT: Process t -> Int
processDeltaT p =
  let {st, rt} = p in
  arrayMax st - arrayMin rt

processLatencies: Process t -> Array Int
processLatencies p =
  let {mode, st, rt} = p in
  if mode=="ack" then arraysPosDelta st rt
  else arraysPosDelta rt st

processSentLatencies: Process t -> Array Int
processSentLatencies p =
  let {st} = p in
  arrayPosDelta st

processRcvdLatencies: Process t -> Array Int
processRcvdLatencies p =
  let {rt} = p in
  arrayPosDelta rt

processIsDone: Process t -> Bool
processIsDone p =
  let {r} = p
      n  = processSize p
      rp = arraySearch 0 r in
  rp == n

processToSend: Process t -> Int
processToSend p =
  let (a, b) = processWindow p
      {t} = p in
  if a==b then -1 else a + modBy (b-a) t

processCanSend: Int -> Process t -> Bool
processCanSend i p =
  let (a, b) = processWindow p in
  i >= a && i < b

processWindow: Process t -> (Int, Int)
processWindow p =
  let {mode, s, r, w} = p
      n  = processSize p
      sp = arraySearch 0 s
      rp = arraySearch 0 r
      a = min sp rp in
  if mode == "ack" then (max (a-w) 0, rp)
  else (a, min (a+w) n)

processSend: t -> Int -> Int -> Process t -> (Process t, Packet t)
processSend d ct i p =
  let {send, st, s, t} = p
      sti = arrayGet 0 i st
      si  = arrayGet 0 i s
      m   = arrayGet d i send in
  ({p | st = Array.set i (minp ct sti) st, s = Array.set i (si+1) s, t = t+1}, Packet i m)

processRecv: Int -> Packet t -> Process t -> Process t
processRecv ct x p =
  let {i, m} = x
      {recv, rt, r} = p
      rti = arrayGet 0 i rt
      ri  = arrayGet 0 i r in
  {p | recv = Array.set i m recv, rt = Array.set i (minp ct rti) rt, r = Array.set i (ri+1) r}



channelSize: Channel t -> Int
channelSize c =
  let {both} = c in
  Array.length both

channelSent: Channel t -> Int
channelSent c =
  let {s} = c in
  arraySum (Array.map sign s)

channelSentAll: Channel t -> Int
channelSentAll c =
  let {s} = c in
  arraySum s

channelRcvd: Channel t -> Int
channelRcvd c =
  let {r} = c in
  arraySum (Array.map sign r)

channelRcvdAll: Channel t -> Int
channelRcvdAll c =
  let {r} = c in
  arraySum r

channelDeltaT: Channel t -> Int
channelDeltaT c =
  let {st, rt} = c in
  arrayMax st - arrayMin rt

channelLatencies: Channel t -> Array Int
channelLatencies c =
  let {st, rt} = c in
  arraysPosDelta st rt

channelSentLatencies: Channel t -> Array Int
channelSentLatencies c =
  let {st} = c in
  arrayPosDelta st

channelRcvdLatencies: Channel t -> Array Int
channelRcvdLatencies c =
  let {rt} = c in
  arrayPosDelta rt

channelToSend: Channel t -> Int
channelToSend c =
  let {t} = c
      n = channelSize c
      canSend j = channelCanSend j c
      is = Array.fromList (filter canSend (range 0 (n-1)))
      ni = Array.length is in
  if ni>0 then arrayGet 0 (modBy ni t) is else -1

channelCanSend: Int -> Channel t -> Bool
channelCanSend i c =
  let {s, r, sb, se} = c
      si = arrayGet 0 i s
      ri = arrayGet 0 i r in
  ri >= sb && si < se

channelSend: t -> Int -> Int -> Channel t -> (Channel t, Packet t)
channelSend d ct i c =
  let {both, st, s, t} = c
      sti = arrayGet 0 i st
      si  = arrayGet 0 i s
      m   = arrayGet d i both in
  ({c | st = Array.set i (minp ct sti) st, s = Array.set i (si+1) s, t = t+1}, Packet i m)

channelRecv: Int -> Packet t -> Channel t -> Channel t
channelRecv ct x c =
  let {i, m} = x
      {both, rt, r} = c
      rti = arrayGet 0 i rt
      ri  = arrayGet 0 i r in
  {c | both = Array.set i m both, rt = Array.set i (minp ct rti) rt,r = Array.set i (ri+1) r}



-- VIEW
view: Model -> Html Msg
view model =
  let {p, q, pq, qp} = model in
  div [] [
    node "link" [
      rel "stylesheet",
      href "SlidingWindow.css"
    ] [],
    table [] [
      tr [] [
        td (class "psend" :: styleTurn 0 model) [viewProcess p  "P.send" SendP True],
        td (class "pq"    :: styleTurn 1 model) [viewChannel pq "P -> Q" RecvQ],
        td (class "qrecv" :: styleTurn 4 model) [viewProcess q  "Q.recv" Next False]
      ],
      tr [] [
        td (class "precv" :: styleTurn 5 model) [viewProcess p  "P.recv" Next False],
        td (class "qp"    :: styleTurn 3 model) [viewChannel qp "Q -> P" RecvP],
        td (class "qsend" :: styleTurn 2 model) [viewProcess q  "Q.send" SendQ True]
      ]
    ],
    div [class "submit"] [
      viewNext (not (isDone model)),
      viewController model
    ]
  ]

viewProcess: Process Int -> String -> (Int -> Msg) -> Bool -> Html Msg
viewProcess p id msg vs =
  let {send, recv, st, s, rt, r, mode, t} = p
      n = processSize p
      k = if vs then processToSend p else -1
      (a, b) = processWindow p
      both = if vs then send else recv
      data i = arrayGet 0 i both
      strn i = if vs then arrayGet 0 i st else 0
      sent i = if vs then arrayGet 0 i s else 0
      rtrn i = if vs then 0 else arrayGet 0 i rt
      rcvd i = if vs then 0 else arrayGet 0 i r
      actv i = vs && (i>=a && i<b)
      msgi i = if vs then msg i else Next i
      vpkt i = viewPacket (data i) (strn i) (sent i) (rtrn i) (rcvd i) (k==i) (actv i) (msgi i)
      sent1  = processSent p
      sentn  = processSentAll p
      sdelt  = arrayMax st - arrayMin st
      slatn  = processSentLatencies p
      rcvd1  = processRcvd p
      rcvdn  = processRcvdAll p
      rdelt  = arrayMax rt - arrayMin rt
      rlatn  = processRcvdLatencies p in
  div [] [
    text ("Process: " ++ id ++ " [" ++ mode ++ "]" ++ if vs then " (" ++ intStr t ++ ")" else ""),
    div [] (map vpkt (range 0 (n-1))),
    if vs then div [] [text (
      "Sent: " ++ intStr sent1 ++ "/" ++ intStr n ++     " (" ++ perStr sent1 n ++ "), " ++
      "Effc: " ++ intStr sent1 ++ "/" ++ intStr sentn ++ " (" ++ perStr sent1 sentn ++ ")"
    )]
    else div [] [text (
      "Rcvd: " ++ intStr rcvd1 ++ "/" ++ intStr n     ++ " (" ++ perStr rcvd1 n ++ "), " ++
      "Effc: " ++ intStr rcvd1 ++ "/" ++ intStr rcvdn ++ " (" ++ perStr rcvd1 rcvdn ++ ")"
    )],
    if vs then div [] [text (
      "Thpt: " ++ wrtStr 2 sent1 sdelt ++ "/" ++ wrtStr 2 sentn sdelt ++ " (" ++ intStr sdelt ++ "), " ++
      "Latn: " ++ wrtStr 2 (arraySum slatn) sent1 ++ " (" ++ intStr (arraySum slatn) ++ ")"
    )]
    else div [] [text (
      "Thpt: " ++ wrtStr 2 rcvd1 rdelt ++ "/" ++ wrtStr 2 rcvdn rdelt ++ " (" ++ intStr rdelt ++ "), " ++
      "Latn: " ++ wrtStr 2 (arraySum rlatn) sent1 ++ " (" ++ intStr (arraySum rlatn) ++ ")"
    )]
  ]

viewChannel: Channel Int -> String -> (Int -> Msg) -> Html Msg
viewChannel c id msg =
  let {both, st, s, rt, r, t} = c
      n = channelSize c
      k = channelToSend c
      data i = arrayGet 0 i both
      strn i = arrayGet 0 i st
      sent i = arrayGet 0 i s
      rtrn i = arrayGet 0 i rt
      rcvd i = arrayGet 0 i r
      actv i = channelCanSend i c
      msgi i = msg i
      vpkt i = viewPacket (data i) (strn i) (sent i) (rtrn i) (rcvd i) (k==i) (actv i) (msgi i)
      sent1  = channelSent c
      sentn  = channelSentAll c
      rcvd1  = channelRcvd c
      rcvdn  = channelRcvdAll c
      delt   = channelDeltaT c
      latn   = channelLatencies c in
  div [] [
    text ("Channel: " ++ id ++ " (" ++ intStr t ++ ")"),
    div [] (map vpkt (range 0 (n-1))),
    div [] [text (
      "Txfr: " ++ intStr sent1 ++ "/" ++ intStr rcvd1 ++ " (" ++ perStr sent1 rcvd1 ++ "), " ++
      "Effc: " ++ intStr sent1 ++ "/" ++ intStr rcvdn ++ " (" ++ perStr sent1 rcvdn ++ ")"
    )],
    div [] [text (
      "Thpt: " ++ wrtStr 2 sent1 delt ++ "/" ++ wrtStr 2 sentn delt ++ " (" ++ intStr delt ++ "), " ++
      "Latn: " ++ wrtStr 2 (arraySum latn) sent1 ++ " (" ++ intStr (arraySum latn) ++ ")"
    )]
  ]

viewPacket: Int -> Int -> Int -> Int -> Int -> Bool -> Bool -> msg -> Html msg
viewPacket data strn sent rtrn rcvd selc actv msg = 
  button (class "packet" :: stylePacket sent rcvd selc ++ [
    title (
      "Sent: " ++ intStr sent ++ " [" ++ intStr strn ++ "], " ++
      "Rcvd: " ++ intStr rcvd ++ " [" ++ intStr rtrn ++ "]"),
    disabled (not actv),
    onClick msg
  ]) [text (intStr data)]

viewController: Model -> Html Msg
viewController model =
  let {p, t} = model
      sent1 = processSent p
      sentn = processSentAll p
      rcvd1 = processRcvd p
      rcvdn = processRcvdAll p
      delt  = processDeltaT p
      latn  = processLatencies p
      end = isDone model
      msg = if end then "All packets recieved." else "Some packets not yet recieved." in
  div [class "controller"] [
    div [] [text ("Controller (" ++ intStr t ++ ")")],
    div (class "status" :: styleStatus end) [text msg],
    div [] [text (
      "Txfr: " ++ intStr rcvd1 ++ "/" ++ intStr sent1 ++ " (" ++ perStr rcvd1 sent1 ++ "), " ++
      "Effc: " ++ intStr rcvd1 ++ "/" ++ intStr sentn ++ " (" ++ perStr rcvd1 sentn ++ ")"
    )],
    div [] [text (
      "Thpt: " ++ wrtStr 2 rcvd1 delt ++ "/" ++ wrtStr 2 rcvdn delt ++ " (" ++ intStr delt ++ "), " ++
      "Latn: " ++ wrtStr 2 (arraySum latn) rcvd1 ++ " (" ++ intStr (arraySum latn) ++ ")"
    )]
  ]

viewNext: Bool -> Html Msg
viewNext actv =
  div [] [
    button [
      class "next",
      disabled (not actv),
      onClick (Next 1)
    ] [text "Next"],
    button [
      class "next",
      disabled (not actv),
      onClick (Next 20)
    ] [text "Next 20"],
    button [
      class "next",
      disabled (not actv),
      onClick (Next 100)
    ] [text "Next 100"]
  ]



-- STYLES
styleTurn: Int -> Model -> List (Attribute m)
styleTurn i model = [
    style "background-color" (if whoseTurn model == i then "lightgreen" else "yellow")
  ]

stylePacket: Int -> Int -> Bool -> List (Attribute m)
stylePacket sent rcvd selc =
  let c = if selc then "black" else if sent >= rcvd then "orange" else "green"
      t = if selc then 1 else min (max sent rcvd) 2 in [
    style "outline" (intStr t ++ "px solid " ++ c)
  ]

styleStatus: Bool -> List (Attribute m)
styleStatus s = [
    style "background-color" (if s then "green" else "yellow")
  ]


-- UTIL
sign: Int -> Int
sign x =
  if x == 0 then 0 else if x > 0 then 1 else -1

minp: Int -> Int -> Int
minp x y =
  if x == 0 then y else if y == 0 then x else min x y

intStr: Int -> String
intStr x =
  String.fromInt x

floatDiv: Int -> Int -> Float
floatDiv x y =
  if y == 0 then 0 else toFloat x / toFloat y

wrt: Int -> Int -> Int -> Int
wrt x y f =
  if y > 0 then round (floatDiv x y * toFloat f) else 0

percent: Int -> Int -> Int
percent x y =
  wrt x y 100

wrtStr: Int -> Int -> Int -> String
wrtStr a x y =
  Round.round a (floatDiv x y)

perStr: Int -> Int -> String
perStr x y =
  wrtStr 0 (x * 100) y ++ "%"

arrayGet: d -> Int -> Array d -> d
arrayGet d i x =
  Maybe.withDefault d (Array.get i x)

arrayMin: Array number -> number
arrayMin x =
  Array.foldl min 0 x

arrayMax: Array number -> number
arrayMax x =
  Array.foldl max 0 x

arraySum: Array number -> number
arraySum x =
  Array.foldl (+) 0 x

arrayPosDelta: Array number -> Array number
arrayPosDelta x =
  let f v (l,p) = (max (v-p) 0 :: l, v) in
  Array.fromList (Tuple.first (Array.foldl f ([], 0) x))

arraysPosDelta: Array number -> Array number -> Array number
arraysPosDelta x y =
  let f i = max (arrayGet 0 i x - arrayGet 0 i y) 0 in
  Array.fromList (map f (range 0 (Array.length x)))

arraySearch: d -> Array d -> Int
arraySearch v x =
  let match u (i, f) = (i-1, if v==u then i-1 else f) in
  Tuple.second (Array.foldr match (Array.length x, Array.length x) x)
