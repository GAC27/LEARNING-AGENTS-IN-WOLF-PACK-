breed [sheep a-sheep]
breed [wolves wolf]
globals[ SizeOfMap wolves-in-position]

turtles-own [prev-xcor prev-ycor]

;;;
;;; Wolves own this characteristics
;;;
wolves-own [
  last-action
  sheep-pos
  desire
  intention
  plan
  otherWolvesObjective
  communication-done
]

;;
;; last-action:      It contains the robot's action in the previous robot-loop
;;                   Its values range between: "move-up", "move-down", "move-left", "move-right" and "stop"
;;
;; desire:           It indentifies the robot's current desire, according to the desire definition in Chap.4 of [Wooldridge02].
;;                   Its values range between: "search", "pursuit" and "stop"
;;
;; intention:        It identifies the robot's current intention, according to the intention definition in Chap.4 of [Wooldridge02].
;;                   It uses the internal abstract type 'intention'
;;
;; plan:             It identifies the robot's current plan to achieve its intention
;;                   It uses the internal abstract type 'plan'
;;


patches-own [cost came-from cost-so-far heuristic fvalue]

to set-globals
  set SizeOfMap (max-pxcor + 1)
  set wolves-in-position 0
end

to setup
  clear-all
  reset-ticks
  set-globals
   ask patches with [pxcor >= (- SizeOfMap) and pxcor < (SizeOfMap) and pycor >= (- SizeOfMap) and pycor < (SizeOfMap )]
    [ set pcolor scale-color green ((random 500) + 5000) 0 9000 ]

  set-default-shape sheep "sheep"

  create-sheep 1  ;; create the sheep
  [
    set color white
    set size 1  ;; easier to see
    setxy (round random-xcor) (round random-ycor)
    set heading 0
  ]

  set-default-shape wolves "wolf"


  create-wolves 4  ;; create the wolves
  [
    set color black
    set size 1  ;; easier to see
    set-random-position
    set heading 0
    init-wolf
  ]


end

;;;
;;;  Sets the turtle in a random, empty position
;;;
to set-random-position
  setxy random-pxcor random-pycor
  while [any? other turtles-here] [
    setxy random-pxcor random-pycor
  ]
end



;;;
;;; Initialization of the wolf
;;;
to init-wolf
  set desire "search"
  set intention build-empty-intention
  set plan build-empty-plan
  set last-action ""
  set communication-done false
end


;;;
;;;  Step up the simulation
;;;
to go
  ;Stoping condition test
  ask wolves [
    if around-sheep?[
      set wolves-in-position (wolves-in-position + 1)
    ]
  ]
  if wolves-in-position = 4[
    stop
  ]
  set wolves-in-position 0

  let collisions true
  tick
  ;;

  ;;; Wolves update beliefs
  ask wolves[
    update-beliefs
  ]

  ;; the wolves think
  ask wolves [
      wolf-think-loop
  ]
  ;; the wolves act
  ask wolves [
      set prev-xcor xcor
      set prev-ycor ycor
 ;     wolf-act-loop
  ]
  ;; the sheep act
  ask sheep [
    sheep-loop
  ]
  ;;correct colisions
  while [collisions] [
    set collisions false
    ask turtles [
      if (count (turtles-on patch-here)) > 1[
        set collisions true
        ask (turtles-on patch-here)[
          backtrace-movements
        ]
      ]
    ]
  ]
end


to sheep-loop
  ifelse( reactive-sheep )[
    let threat (wolves-on (patches in-radius Sheep_depth_of_field))
    set threat agentset-to-list threat

    ifelse length threat = 0[
      random-loop
    ]
    [
      let degrees-of-threat []
      foreach threat[
        face ?
        set degrees-of-threat lput heading degrees-of-threat
      ]

      let avg-degree-of-threat 0
      foreach degrees-of-threat[
        set avg-degree-of-threat (avg-degree-of-threat + ?)
      ]
      set avg-degree-of-threat ( avg-degree-of-threat / (length threat) )

      set avg-degree-of-threat (avg-degree-of-threat + 180 )

      set heading avg-degree-of-threat

      set heading ((floor (heading / 90) ) * 90)

      fd 1

    ]
  ]
  [
    random-loop
  ]

end

to random-loop
  set prev-xcor xcor
    set prev-ycor ycor
    set heading ((random 4) * 90)
    if (random 100) > 25 [
      fd 1
    ]
end



;;;
;;;  wolf's updating procedure, which defines the rules of its behaviors
;;;
to wolf-think-loop
  set communication-done false
  ;; Check the wolf's options
  set desire BDI-options
  set intention BDI-filter
  set plan build-plan-for-intention intention
end

;;;
;;;Wolf acting procedure
;;;
to wolf-act-loop
  if not empty-plan? plan [
    execute-plan-action
  ]
end

;
;Returns the turtle to its previous position
;

to backtrace-movements
  set xcor prev-xcor
  set ycor prev-ycor
end

;
;Reports if an agent is next to a sheep
;
to-report around-sheep?
  report any? sheep-on neighbors4
end


;
;Reports the sheep if it is in the wolf's field of vision or nobody
;
to-report position-sheep
  report one-of sheep-on visible-patches
end


to-report visible-patches
  report patches in-radius Wolf_depth_of_field
end


;
;Reports an agentset with all the seen wolves that are in the wolf's field of vision
;
to-report visible-wolves
  report (wolves-on visible-patches) with [ myself != self ]
end




;;;
;;;
;;; BDI PROCEDURES
;;;
;;;


;;;
;;; According to the current beliefs, it selects the wolf's desires
;;;
;;; Reference: Chap.4 de [Wooldridge02]
;;;
to-report BDI-options
  ifelse around-sheep? [
   report "stop"
  ]
  [
   ifelse sheep-pos != nobody[
     report "pursuit"
   ]
   [
     report "search"
   ]
  ]

end


;;;
;;; It selects a desire and coverts it into an intention
;;; Reference: Chap.4 de [Wooldridge02]
;;;
to-report BDI-filter
  let pos-xcor xcor
  let pos-ycor ycor

  ifelse desire = "stop"
  [
    report build-intention desire (list xcor ycor)
  ]
  [
   ifelse desire = "pursuit" [
     let pos get-position-to-attack
     let px (item 0 pos)
     let py (item 1 pos)

     report build-intention desire (list px py)
   ]
   [
     if desire = "search" [
      if (random 100) >= 25[
      ifelse(diagonal-movement)
      [
       ask one-of neighbors [
         set pos-xcor pxcor
         set pos-ycor pycor
       ]
      ]
      [
        ask one-of neighbors4 [
         set pos-xcor pxcor
         set pos-ycor pycor
       ]
      ]
      ]
       report build-intention desire (list pos-xcor pos-ycor)
     ]
   ]

  ]
  report build-empty-intention
end


;;;
;;;  Create a plan for a given intention
;;;
to-report build-plan-for-intention [iintention]
  let new-plan build-empty-plan

  if not empty-intention? iintention
  [
    set new-plan build-path-plan (list xcor ycor) item 1 iintention
  ]

  report new-plan
end



;;;
;;;  Update the wolf's beliefs based on its perceptions
;;;  Reference: Chap.4 of [Wooldridge02]
;;;
to update-beliefs
  update-state
end

;;;
;;;  Check if the wolf's intention has been achieved
;;;
to-report intention-succeeded? [iintention]
  let ddesire 0

  if (empty-intention? iintention)
    [ report false ]

  set ddesire get-intention-desire iintention

  ifelse (ddesire = "stop")
  [ report true ]
  [ report false ]
end


;;;  Check if an intention cannot be achieved anymore
;;;
;;;
to-report impossible-intention? [iintention]
  report false
end


;;;
;;;  Reactive agent control loop
;;;
to reactive-agent-loop
   let the-sheep position-sheep
  if not around-sheep?[
    if the-sheep != nobody[
      face the-sheep
      set heading ( floor (heading / 90)) * 90
    ]
    fd 1
  ]

end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                            ;;;
;;;           INTERNAL ABSTRACT TYPES          ;;;
;;;                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; -------------------------
;;; Intention
;;;
;;; Chap.4 of [Wooldridge02]
;;; An intention is a list such as [desire position heading]
;;; -------------------------
;;;
to-report build-empty-intention
  report []
end

to-report build-intention [ddesire pposition]
  let aux 0

  set aux list ddesire pposition
  report aux
end

to-report get-intention-desire [iintention]
  report item 0 iintention
end

to-report get-intention-position [iintention]
  report item 1 iintention
end


to-report empty-intention? [iintention]
  report empty? iintention
end

;;;
;;; -------------------------
;;;    Plans
;;; -------------------------
;;;

to-report add-instruction-to-plan [pplan iinstruction]
  report lput iinstruction pplan
end

to-report remove-plan-first-instruction [pplan]
  report butfirst pplan
end

to-report get-plan-first-instruction [pplan]
  report first pplan
end

;;;
;;; Build a pan to move the agent from posi to posf
;;;
to-report build-path-plan [posi posf]
  let newPlan 0
  let path 0
  let lastpos (patch (item 0 posi) (item 1 posi))


  set newPlan build-empty-plan
  set path (find-path posi posf)
  foreach path
    [ set newPlan add-instruction-to-plan newPlan (build-instruction-find-adjacent-position ? lastpos)
      set lastpos ? ]

  report newPlan
end


to-report build-empty-plan
  report []
end

to-report empty-plan? [pplan]
  report empty? pplan
end




;;;
;;; -------------------------
;;;    Plan Intructions
;;; -------------------------
;;;

to-report build-instruction [ttype vvalue]
  report list ttype vvalue
end

to-report get-instruction-type [iinstruction]
  report first iinstruction
end

to-report get-instruction-value [iinstruction]
  report last iinstruction
end

to-report build-instruction-find-adjacent-position [aadjacent-position lastpos]
  let posxDifference ([pxcor] of aadjacent-position) - ([pxcor] of lastpos)
  let posyDifference ([pycor] of aadjacent-position) - ([pycor] of lastpos)
  let realSizeOM (-1 + SizeOfMap)
  ifelse (posxDifference > 0 and posyDifference = 0 and posxDifference != realSizeOM) or (posxDifference = (0 - realSizeOM) and posyDifference = 0)[
     report build-instruction "moveright" aadjacent-position
  ]
  [
     ifelse (posxDifference < 0 and posyDifference = 0 and posxDifference != (0 - realSizeOM)) or (posxDifference = realSizeOM and posyDifference = 0)[
        report build-instruction "moveleft" aadjacent-position
     ]
     [
       ifelse (posxDifference = 0 and posyDifference > 0 and posyDifference != realSizeOM) or (posxDifference = 0 and posyDifference = (0 - realSizeOM))[
         report build-instruction "moveup" aadjacent-position
       ]
       [
         report build-instruction "movedown" aadjacent-position
       ]
     ]
  ]

end




;;;
;;; ----------------------------
;;;  Plan execution procedures
;;; ----------------------------
;;;

;;;
;;;  Execute the next action of the current plan
;;;
to execute-plan-action
  let current-instruction get-plan-first-instruction plan

  setxy ([pxcor] of (item 1 current-instruction)) ([pycor] of (item 1 current-instruction))

  set plan remove-plan-first-instruction plan
end

;;;
;;; ----------------------------------------
;;;    Internal state updating procedures
;;; ----------------------------------------
;;;

;;;
;;;  Update the wolf's state using perceptions and comunications
;;;
to update-state
  let the-sheep position-sheep
  if not communication-done[
    ifelse (the-sheep != nobody )[
      set sheep-pos the-sheep
      send-sheep-pos the-sheep
    ]
    [      set sheep-pos nobody
    ]
  ]
end



;;;
;;; ----------------------------
;;;    Communication procedures
;;; ----------------------------
;;;

;;;send sheep pos to
;;;wolves visible
to send-sheep-pos [the-sheep]
  if(not communication-done)
  [
    set communication-done true
    ask visible-wolves[
      set sheep-pos the-sheep
      send-sheep-pos the-sheep
    ]
  ]
end


;;;Choose a free position to attack
to-report get-position-to-attack
  let wolves-intention-patches []
  let px 0
  let py 0
  ask visible-wolves [
      if (not empty-intention? intention) [
        set px (first item 1 intention)
        set py (last item 1 intention)
        set wolves-intention-patches lput (patch px py) wolves-intention-patches
      ]
     ]
     let objective-patches 0
     ask sheep-pos [
       set objective-patches filter [not member? ? wolves-intention-patches] (agentset-to-list neighbors4)
     ]
     ask first objective-patches [
      set px pxcor
      set py pycor
     ]
     report (list px py)
end


;;;
;;; -------------------------
;;;    Map
;;; -------------------------
;;;



;;;
;;;  Return a list of positions from initialPos to FinalPos
;;;  The returning list excludes the initialPos
;;;  If no path is found, the returning list is empty
;;;
;;;  path= caminho -> reachedGoal-> frontier->Goal-pos -> Start_pos
to-report find-path [initialPos FinalPos]
  setup-patches

  let START_POS patch (item 0 initialPos) (item 1 initialPos)

  ask START_POS [ set came-from START_POS ]

  let path (list [] false (list START_POS) (patch (item 0 FinalPos) (item 1 FinalPos)) START_POS)

  while [(item 1 path ) = false][
    set path pathfinding-iteration path
  ]
  report item 0 path
end


to-report pathfinding-iteration [path]
 ; let current pop-frontier (item 2 path)


  ;pop-frontier
  let current first (item 2 path)
  set path replace-item 2 path (but-first (item 2 path))

  if current = (item 3 path)
  [
    set path (reconstruct-path path)
    report replace-item 1 path true
  ]


  let current-neighbors 0
  ifelse (diagonal-movement)
  [
    set current-neighbors (agentset-to-list ([neighbors] of current))
  ]
  [
    set current-neighbors (agentset-to-list ([neighbors4] of current))
  ]
  (foreach current-neighbors [
    let next ?
    let new-cost [cost-so-far] of current + [cost] of next
    if [cost-so-far] of next = 0 or new-cost < [cost-so-far] of next
    [
      ask next
      [
        set cost-so-far new-cost
        set heuristic simple-distance self (item 3 path)
        set fvalue cost-so-far + heuristic
        ;set plabel round fvalue
      ]
      set path replace-item 2 path (add-frontier next (item 2 path))
      ask next [ set came-from current ]
    ]
  ])

  report path
end



to-report add-frontier [value FRONTIER]
  set FRONTIER fput value FRONTIER
  set FRONTIER sort-by [[fvalue] of ?1 < [fvalue] of ?2] FRONTIER
  report FRONTIER
end


to-report simple-distance [patch1 patch2]
  report [distance patch2] of patch1
end


; TODO
to setup-patches
  ask patches with [pxcor >= 0] [
    set cost 1
    set came-from 0
    set cost-so-far 0
    set heuristic 0
    set fvalue 0
  ]

  ask (visible-patches with [ count sheep-here != 0 or count wolves-here != 0 ]) [ set cost 99 ]
end


;;;
;;; Path data structure
;;;
to-report reconstruct-path [path]
  let current (item 3 path)
  let finalPath (list)
  while [current != (item 4 path)]
  [
    (set finalPath (fput current finalPath))
    (set current ([came-from] of current))
  ]
  set path replace-item 0 path finalPath
  report path
end



;;;
;;; I blame NetLogo
;;;
to-report agentset-to-list [as]
  report [self] of as
end
@#$#@#$#@
GRAPHICS-WINDOW
392
41
659
329
-1
-1
25.7
1
10
1
1
1
0
1
1
1
0
9
0
9
0
0
1
ticks
30.0

SLIDER
12
22
227
55
Wolf_depth_of_field
Wolf_depth_of_field
1
(floor SizeOfMap - 1) / 2
4
1
1
patches
HORIZONTAL

BUTTON
88
62
152
95
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
13
62
76
95
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
21
105
132
138
Single Step Go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
12
154
176
187
diagonal-movement
diagonal-movement
1
1
-1000

SWITCH
12
188
176
221
reactive-sheep
reactive-sheep
0
1
-1000

SLIDER
11
224
183
257
Sheep_depth_of_field
Sheep_depth_of_field
0
((floor SizeOfMap - 1) / 2) / 2
1
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
