__includes ["q-learning.nls" "utils.nls" "matrices.nls"]
extensions [table]

globals [ACTION-LIST NUM-ACTIONS EPSILON MAP-SIZE TIME-STEPS EPISODE LAST-25-TIME-STEPS]

breed [sheep the-sheep]
breed [wolves wolf]

wolves-own [Q-values1 Q-values2 Q-values3 w_previous-state w_action]

to go
  if episode-finished? or TIME-STEPS >= MAX-TIME-STEPS-PER-EPISODE [
    next-episode
    if EPISODE >= MAX-EPISODE [ stop ]
  ]

  tick
  set TIME-STEPS (TIME-STEPS + 1)

  ask sheep [
    sheep-loop
  ]

  ask wolves [
    wolf-think-act-loop
  ]

  ask wolves [
    wolf-learning-loop
  ]
end

to wolf-think-act-loop
  set w_previous-state (compute-state)

  set w_action select-action
  execute-action w_action
end

to wolf-learning-loop
  let reward (get-reward)
  let current-state (compute-state)

  set Q-values1 (update-Q-value Q-values1 (get-partial-state w_previous-state 1) (get-partial-state current-state 1) w_action reward)
  set Q-values2 (update-Q-value Q-values2 (get-partial-state w_previous-state 2) (get-partial-state current-state 2) w_action reward)
  set Q-values3 (update-Q-value Q-values3 (get-partial-state w_previous-state 3) (get-partial-state current-state 3) w_action reward)
end


;;; ==========================================
;;;            ACTION SELECTION
;;; ====================================

;;; @scope wolf
to-report select-action
  ifelse action-selection = "ε-greedy"
    [ report select-action-e-greedy ]
    [ report select-action-greatest-mass ]
end

to-report select-action-e-greedy
  ifelse EPSILON > (random-float 1) [
    report item (random NUM-ACTIONS) ACTION-LIST
  ]
  [
    report select-action-greatest-mass
  ]
end

to-report select-action-greatest-mass
  let action-values get-Q-vectors-summed
  report item (position (max action-values) action-values) ACTION-LIST
end
;;; ====================================
;;;            ACTION SELECTION
;;; ==========================================


;;; @scope turtle
to execute-action [action]
  if not (any? (turtles-at (first action) (last action))) [
    set xcor (xcor + (first action))
    set ycor (ycor + (last action))
  ]
end

to-report get-reward
  ifelse episode-finished?
  [
    report REWARD-VALUE
  ]
  [
    report REWARD-ABORT
  ]
end



to sheep-loop
  ifelse( REACTIVE-SHEEP )[
    let threat (wolves-on (patches in-radius SHEEP-VISUAL-FIELD-DEPTH))
    set threat agentset-to-list threat
    let sumcos 0
    let sumsin 0

    ifelse length threat = 0[
      random-loop
    ]
    [
      let degrees-of-threat []
      foreach threat[
        face ?
        set degrees-of-threat lput heading degrees-of-threat
      ]
      foreach degrees-of-threat[
        set sumcos (sumcos + cos ?)
        set sumsin (sumsin + sin ?)
      ]


      let avg-degree-of-threat atan sumsin sumcos

      set heading (avg-degree-of-threat + 180)

      set heading ((round (heading / 90) ) * 90)
      if((random 100) < SHEEP-MOVEMENT-PROBABILITY)[

        fd 1
      ]
    ]
  ]
  [
    random-loop
  ]

end

to-report agentset-to-list [as]
  report [self] of as
end

to random-loop
  set heading ((random 4) * 90)
  if (random 100) < SHEEP-MOVEMENT-PROBABILITY [
    fd 1
  ]
end





to-report episode-finished?
  report captured-sheep? (the-sheep 0)
end

to-report captured-sheep? [the-sheep]
  let neighbors-with-only-one-agent ([neighbors4] of the-sheep) with [ count turtles-here = 1 ]
  report count neighbors-with-only-one-agent = 4
end



to init-episode
  set TIME-STEPS 0

  ask turtles [
    set-random-position
    set heading 0
  ]
end

;;; @scope none
to next-episode
  set EPSILON max (list 0 (1 - (EPISODE / MAX-EPISODE)))

  set-current-plot "Time Performance"
  ;set-current-plot-pen "time-steps"
  ;plot TIME-STEPS

  set EPISODE (EPISODE + 1)

  set LAST-25-TIME-STEPS lput TIME-STEPS LAST-25-TIME-STEPS
  set-current-plot-pen "average-time-steps"
  ifelse EPISODE >= 25 [
    plot mean LAST-25-TIME-STEPS
    set LAST-25-TIME-STEPS but-first LAST-25-TIME-STEPS
  ]
  [
    plot mean LAST-25-TIME-STEPS
  ]

   init-episode
end



;;; ==========================================
;;;                 STATE
;;; ====================================

;;; Returns a list of rel-cors of all other turtles, sorted by who. Not visible turtles have the same rel-cors as the sheep (whichever they are).
;;;
;;; @scope wolf
to-report compute-state
  let sheep-rel-cors (first rel-cors-of-visible-agents)
  let rel-cors (list)

  foreach rel-cors-of-visible-agents [
    ifelse (first ?) = 0 and (last ?) = 0 [
      set rel-cors lput sheep-rel-cors rel-cors
    ]
    [
      set rel-cors lput ? rel-cors
    ]
  ]

  report (map [(list ((first ?) + WOLF-VISUAL-FIELD-DEPTH) ((last ?) + WOLF-VISUAL-FIELD-DEPTH))] rel-cors)
end

to-report get-partial-state [state wolf-rel-idx]
  report (list (first state) (item wolf-rel-idx state))
end

to-report get-sheep-state [state]
  report (first state)
end

to-report get-wolf-state [state wolf-rel-idx]
  report (item wolf-rel-idx state)
end

;;; ====================================
;;;                 STATE
;;; ==========================================

;;; ==========================================
;;;                 PERCEPTIONS
;;; ====================================

to-report rel-cors-of-visible-agents
  let other-agents (sort (other turtles))
  let rel-cors (list)

  foreach other-agents [
    ifelse distance ? <= WOLF-VISUAL-FIELD-DEPTH [
      set rel-cors lput (rel-cors-toroidal (cors) (cors-of ?)) rel-cors
    ]
    [
      set rel-cors lput (list 0 0) rel-cors ; ASSUMES NO COLLISIONS
    ]
  ]

  report rel-cors
end

;;; @scope wolf
to-report visible-agents
  report other (turtles-on visible-patches)
end

to-report visible-patches
  report patches in-radius WOLF-VISUAL-FIELD-DEPTH
end

;;; ====================================
;;;                 PERCEPTIONS
;;; ==========================================


to-report get-action-idx [action]
  report (position action ACTION-LIST)
end

;;; ==========================================
;;;                 SETUP
;;; ====================================

to setup
  clear-all
  reset-ticks

  set MAP-SIZE (max-pxcor + 1)

  set ACTION-LIST (list
    list 0 0    ; no-move
    list 0 1    ; N north
    list 0 -1   ; S south
    list 1 0    ; E east
    list -1 0   ; W west
    list 1 1    ; NE northeast
    list 1 -1   ; SE southeast
    list -1 1   ; NW northwest
    list -1 -1  ; SW southwest
    )

  ifelse (WOLF-DIAGONAL-MOVEMENT)
  [
    set NUM-ACTIONS 9
  ]
  [
    set NUM-ACTIONS 5
  ]



  ask patches [
    set pcolor scale-color green ((random 500) + 5000) 0 9000
  ]

  spawn-sheep
  spawn-wolves

  set LAST-25-TIME-STEPS (list)
  init-episode
end


;;; @scope none
to spawn-sheep
  set-default-shape sheep "sheep"

  create-sheep 1 [
    set color white
  ]
end

;;; @scope none
to spawn-wolves
  set-default-shape wolves "wolf"

  create-wolves 4 [
    set Q-values1 initial-Q-values
    set Q-values2 initial-Q-values
    set Q-values3 initial-Q-values
  ]

  ask wolf 1 [ set color cyan ]
  ask wolf 2 [ set color magenta ]
  ask wolf 3 [ set color yellow ]
  ask wolf 4 [ set color black ]
end

;;;
;;; Sets the turtle in a random, empty position
;;; @scope turtle
to set-random-position
  setxy random-pxcor random-pycor
  while [any? other turtles-here] [
    setxy random-pxcor random-pycor
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
355
10
665
341
-1
-1
30.0
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

BUTTON
11
10
74
43
NIL
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
89
10
152
43
NIL
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
166
10
247
43
single-go
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

SLIDER
12
57
282
90
Wolf-Visual-Field-Depth
Wolf-Visual-Field-Depth
0
(floor world-height - 1) / 2
3
1
1
NIL
HORIZONTAL

SLIDER
12
99
282
132
Sheep-Movement-Probability
Sheep-Movement-Probability
0
100
25
1
1
%
HORIZONTAL

MONITOR
713
191
803
236
NIL
Time-Steps
17
1
11

MONITOR
713
248
783
293
NIL
Episode
17
1
11

SLIDER
11
143
281
176
Max-Time-Steps-Per-Episode
Max-Time-Steps-Per-Episode
0
5000
1500
1
1
NIL
HORIZONTAL

SLIDER
13
187
283
220
Max-Episode
Max-Episode
0
100000
10000
1
1
NIL
HORIZONTAL

SLIDER
14
232
201
265
Reward-Value
Reward-Value
-1
1
1
0.1
1
NIL
HORIZONTAL

SLIDER
14
270
201
303
Reward-Abort
Reward-Abort
-1
1
-0.1
0.1
1
NIL
HORIZONTAL

SLIDER
14
310
201
343
Learning-Rate
Learning-Rate
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
14
346
201
379
Discount-Factor
Discount-Factor
0
1
0.9
0.1
1
NIL
HORIZONTAL

PLOT
713
10
1323
180
Time Performance
episode
avg-time-steps
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"time-steps" 1.0 0 -16777216 true "" ""
"average-time-steps" 1.0 0 -11221820 true "" ""

CHOOSER
14
386
168
431
Action-selection
Action-selection
"ε-greedy" "greatest-mass"
1

SWITCH
16
437
211
470
Wolf-Diagonal-Movement
Wolf-Diagonal-Movement
1
1
-1000

SWITCH
17
476
228
509
Reactive-Sheep
Reactive-Sheep
0
1
-1000

SLIDER
17
514
227
547
Sheep-Visual-Field-Depth
Sheep-Visual-Field-Depth
0
100
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
