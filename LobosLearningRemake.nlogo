;;;
;;;  Use the "array" extension for easy and efficient Q-value storage
;;;
extensions [array table]
breed [sheep a-sheep]
breed [wolves wolf]
globals[ NUM-ACTIONS SizeOfMap epsilon temperature episode-count time-steps total-time-steps ACTION-LIST LAST-25-TIME-STEPS]

turtles-own [ init_xcor init_ycor prev-xcor prev-ycor]
;distancexy-sheep = (Wolf_depth_of_field,Wolf_depth_of_field) quando não vê a ovelha
wolves-own [Q-values1 Q-values2 Q-values3 reward total-reward action last-action previous-state-of-turtles state-of-turtles]

;;;  =================================================================
;;;      Interface reports
;;;  =================================================================

to-report get-total-time-steps
  report total-time-steps
end

to-report get-episode-count
  report episode-count
end

;;;  =================================================================
;;;      Setup
;;;  =================================================================


;;;
;;;  Setup the simulation.
;;;
to setup
  clear-all
  set-globals
  setup-patches
  setup-turtles
  reset-ticks
end

;;;
;;;  Set global variables' values.
;;;
to set-globals
  set SizeOfMap (max-pxcor + 1)
  set time-steps 0
  set episode-count 0
  set epsilon 1
   set temperature 100
  set LAST-25-TIME-STEPS (list)
  ; defines list of actions as (x y) move increments
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
  ; defines the number of available actions from above
  ifelse(diagonal-movement)
  [set NUM-ACTIONS 9]
  [set NUM-ACTIONS 5]
end



;;;
;;;  Setup patches.
;;;
to setup-patches
  ask patches with [pxcor >= (- SizeOfMap) and pxcor < (SizeOfMap) and pycor >= (- SizeOfMap) and pycor < (SizeOfMap )]
    [ set pcolor scale-color green ((random 500) + 5000) 0 9000 ]
end


;;;
;;;  Setup all the entities.
;;;
to setup-turtles

   set-default-shape sheep "sheep"

  create-sheep 1  ;; create the sheep
  [
    set color white
    set size 1  ;; easier to see
    set-random-position
    set init_xcor xcor
    set init_ycor ycor
    set prev-xcor xcor
    set prev-ycor ycor
    set heading 0
  ]

   set-default-shape wolves "wolf"

  create-wolves 4  ;; create the wolves
  [
    set color black
    set size 1  ;; easier to see
    set-random-position
    set init_xcor xcor
    set init_ycor ycor
    set heading 0
    set prev-xcor xcor
    set prev-ycor ycor
    set Q-values1 get-initial-Q-values
    set Q-values2 get-initial-Q-values
    set Q-values3 get-initial-Q-values
    set reward 0
    set total-reward 0
    set state-of-turtles table:make
  ]
  ask wolf 1[
   set color red
  ]
  ask wolf 2[
   set color yellow
  ]
  ask wolf 3[
   set color blue
  ]
  ask wolf 4[
   set color pink
  ]

  ask wolves [
   update-state
   set previous-state-of-turtles state-of-turtles
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



;;;  =================================================================
;;;      Update
;;;  =================================================================

;;;
;;;  Step the simulation.
;;;
to go

  ; if episode is finished starts new episode, otherwise ask each agent to update
  ifelse episode-finished? or ((time-steps > 1500) and With-abort)[
    reset
    if episode-count >= max-episodes [stop]
  ]
  [

    ;Select action
    ask wolves [ wolf-think-loop ]

    ;Execute action
    ask wolves
     [ wolf-execute-loop ]
    correct-collisions

    ask sheep [ sheep-loop ]
    correct-collisions

    ; increases action count
    set time-steps (time-steps + 1)

    ;Get Reward
    ask wolves [ wolf-reward-loop ]

    ;Update state
    ask wolves [ wolf-update-state-loop ]

    set total-time-steps (total-time-steps + 1)
  ]
end


;;;
;;;  Starts a new learning episode by resetting the simulation.
;;;
to reset
  ask sheep[; reset positions
    set xcor init_xcor
    set ycor init_ycor
    set prev-xcor xcor
    set prev-ycor ycor
  ]

  ask wolves [
    ; plot reward in episode
    set-current-plot "Reward performance"
    set-current-plot-pen (word who "reward")
    plot total-reward
    set total-reward 0

    ; reset positions
    set xcor init_xcor
    set ycor init_ycor
    set prev-xcor xcor
    set prev-ycor ycor
  ]

  ask wolves [
   update-state
   set previous-state-of-turtles state-of-turtles
  ]
; plots and update variables
  set-current-plot "Time performance"
  set-current-plot-pen "time-steps"
  plot time-steps

  set LAST-25-TIME-STEPS lput time-steps LAST-25-TIME-STEPS
  set-current-plot-pen "average-time-steps"
  ifelse episode-count >= 25 [
    plot mean LAST-25-TIME-STEPS
    set LAST-25-TIME-STEPS but-first LAST-25-TIME-STEPS
  ]
  [
    plot mean LAST-25-TIME-STEPS
  ]

  set episode-count (episode-count + 1)
  set time-steps 0

  ; linearly decrease explorations over time
  set epsilon max list 0 (1 - (episode-count / max-episodes))
  set temperature max list 0.8 (epsilon * 10)
  ;set epsilon 0
end



to sheep-loop
  set prev-xcor xcor
  set prev-ycor ycor
  set heading ((random 4) * 90)
  if (random 100) < movement-probability-sheep [
    fd 1
  ]
end



;;;
;;;  Updates a wolf by choosing an action
;;;
to wolf-think-loop
  ; chooses action
  set last-action action
  set action select-action
  if (time-steps = 0)[
    set last-action action
    ]
end

;;;
;;;  Updates a wolf by executing an action
;;;
to wolf-execute-loop
  ; updates environmet
  execute-action
end

;;;
;;;  Updates a wolf by getting its reward
;;;
to wolf-reward-loop
  ; gets reward
  set reward get-reward

  set total-reward (total-reward + reward)

  ; updates Q-value function
  update-Q-learning
end


;;;
;;;  Updates a wolf by getting its reward
;;;
to wolf-update-state-loop
  update-state
end


to update-state
  set previous-state-of-turtles state-of-turtles
  let list-turtles agentset-to-list (turtles with [myself != self])
  foreach list-turtles[
    table:put state-of-turtles ([who] of ? ) (get-distance-state ?)
  ]
end

;;;
;;;Corrects collision through backtracing operations
;;;
to correct-collisions
  let collisions true
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




;;;
;;;  Chooses an action according to the ε-greedy method.
;;;  Tips:
;;;    - use "array:to-list" to convert an array to a list
;;;
to-report select-action
  ; checks dice against epsilon
  let dice random-float 1
  ifelse epsilon > dice [
    ; return random action
    report item (random NUM-ACTIONS) ACTION-LIST
  ]
  [
    ; return max action
    let action-values get-Q-values-summed
    report item (position (max action-values) action-values) ACTION-LIST
  ]
end

;;;  =================================================================
;;;      Utils
;;;  =================================================================

to-report get-distance-state [a-turtle]
  ifelse(turtle-is-visible? a-turtle)[

    let x-distance get-real-distance ([xcor] of a-turtle) xcor
    let y-distance get-real-distance ([ycor] of a-turtle) ycor
    let return-dist list 0 0
    face a-turtle

    ;if a-turtle is on top
    ifelse( (90 > heading and heading >= 0) or (360 > heading and heading >= 270) )[
      set return-dist replace-item 1 return-dist (y-distance + Wolf_depth_of_field)
    ]
    [
      set return-dist replace-item 1 return-dist (0 - y-distance + Wolf_depth_of_field)
    ]

     ;if a-turtle is on the right
    ifelse( (180 > heading and heading >= 0))[
      set return-dist replace-item 0 return-dist (x-distance + Wolf_depth_of_field)
    ]
    [
      set return-dist replace-item 0 return-dist (0 - x-distance + Wolf_depth_of_field)
    ]

    report return-dist

  ]
  [
   ifelse([breed] of a-turtle = sheep)[
     report list Wolf_depth_of_field Wolf_depth_of_field
   ]
   [
     report list (2 * Wolf_depth_of_field + 1) 0
   ]
  ]

end

to-report get-real-distance [ x1 x2]
  report min list (abs (x1 - x2 )) (SizeOfMap - (abs (x1 - x2 )))
end
to-report get-action-index [action-wanted]
  report position action-wanted ACTION-LIST
end

;;;
;;;  Creates the initial Q-value function structure: (x y action) <- 0.
;;;
to-report get-initial-Q-values
  report array:from-list n-values (2 * Wolf_depth_of_field + 1)  [
    array:from-list n-values (2 * Wolf_depth_of_field + 1)  [
      array:from-list n-values (2 * Wolf_depth_of_field + 2)  [
        array:from-list n-values (2 * Wolf_depth_of_field + 1)  [
          array:from-list n-values NUM-ACTIONS [0]]]]]
end


;;;
;;;  Checks whether a episode/trial has finished.
;;;  An episode finishes when all agents/taxis have picked up a different passenger.
;;;
to-report episode-finished?
  report captured-sheep? (a-sheep 0)
end

to-report captured-sheep? [the-sheep]
  let neighbors-with-only-one-agent ([neighbors4] of the-sheep) with [ count turtles-here = 1 ]
  report count neighbors-with-only-one-agent = 4
end


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

to-report visible-turtles
  report (turtles-on visible-patches) with [ myself != self ]
end


;
;Reports an agentset with all the seen wolves that are in the wolf's field of vision
;
to-report visible-wolves
  report (wolves-on visible-patches) with [ myself != self ]
end

to-report collision?
  report any? (turtles-on patch-here) with [self != myself]
end

to-report turtle-is-visible? [a-turtle]
  report any? visible-turtles with [self = a-turtle]

end


to-report agentset-to-list [as]
  report [self] of as
end




to-report get-Q-values-summed
  let turtles-state []
  foreach (sort (table:keys state-of-turtles)) [
    let el (table:get state-of-turtles ?)
    set turtles-state lput el turtles-state
  ]

  let action-values1 get-Q-values Q-values1 (first (item 0 turtles-state)) (last (item 0 turtles-state)) (first (item 1 turtles-state)) (last (item 1 turtles-state))
  let action-values2 get-Q-values Q-values2 (first (item 0 turtles-state)) (last (item 0 turtles-state)) (first (item 2 turtles-state)) (last (item 2 turtles-state))
  let action-values3 get-Q-values Q-values3 (first (item 0 turtles-state)) (last (item 0 turtles-state)) (first (item 3 turtles-state)) (last (item 3 turtles-state))

  let i 0
  while [i < NUM-ACTIONS]
  [
    array:set action-values1 i ((array:item action-values1 i) + (array:item action-values2 i) + (array:item action-values3 i))
    set i (i + 1)
  ]

  report (array:to-list action-values1)
end




to-report get-Q-values [Q-values sheep-x sheep-y wolf-x wolf-y]
  report array:item (array:item (array:item (array:item Q-values sheep-x) sheep-y) wolf-x) wolf-y
end


;;;
;;;  Executes a given action by changing the agent's position accordingly.
;;;
to execute-action

  ; stores previous position
  set prev-xcor xcor
  set prev-ycor ycor

  ; sets position according to action move values for x and y (if possible)
  set xcor xcor + first action
  set ycor ycor + last action
end


;;;
;;;  Gets the reward related with the current state and a given action (x y action).
;;;
to-report get-reward

  ; did it pick a passenger solo?
  ifelse episode-finished?
  [
    report reward-value
  ]
  [
    report reward-abort
  ]
end


;;;
;;;  Updates the Q-value for a given action according to the Q-learning algorithm update rule.
;;;  Tips:
;;;    - use "get-Q-value" and "set-Q-value" to update the action-value function
;;;    - properties "previous-xcor" and "previous-ycor" give access to the previous state
;;;
to update-Q-learning
  let list-of-turtle-states (sort table:keys state-of-turtles)
  let next-sheep-state (table:get state-of-turtles (first list-of-turtle-states))
  let next-wolf1-state (table:get state-of-turtles (item 1 list-of-turtle-states))
  let next-wolf2-state (table:get state-of-turtles (item 2 list-of-turtle-states))
  let next-wolf3-state (table:get state-of-turtles (item 3 list-of-turtle-states))

  let prev-list-of-turtle-states (sort table:keys previous-state-of-turtles)
  let sheep-state (table:get previous-state-of-turtles (first prev-list-of-turtle-states))
  let wolf1-state (table:get previous-state-of-turtles (item 1 prev-list-of-turtle-states))
  let wolf2-state (table:get previous-state-of-turtles (item 2 prev-list-of-turtle-states))
  let wolf3-state (table:get previous-state-of-turtles (item 3 prev-list-of-turtle-states))



  ; get previous Q-value
  let previous-Q-value1 (get-Q-value Q-values1 (first sheep-state)  (last sheep-state) (first wolf1-state) (last wolf1-state) last-action )
  let previous-Q-value2 (get-Q-value Q-values2 (first sheep-state)  (last sheep-state) (first wolf2-state) (last wolf2-state) last-action )
  let previous-Q-value3 (get-Q-value Q-values3 (first sheep-state)  (last sheep-state) (first wolf3-state) (last wolf3-state) last-action )

  ; gets r + (lambda * max_a' Q(s',a')) - Q(s,a)
  let prediction-error1 (reward + (discount-factor * (get-max-Q-value Q-values1 (first next-sheep-state)  (last next-sheep-state) (first next-wolf1-state) (last next-wolf1-state) ) ) - previous-Q-value1)
  let prediction-error2 (reward + (discount-factor * (get-max-Q-value Q-values2 (first next-sheep-state)  (last next-sheep-state) (first next-wolf2-state) (last next-wolf2-state) ) ) - previous-Q-value2)
  let prediction-error3 (reward + (discount-factor * (get-max-Q-value Q-values3 (first next-sheep-state)  (last next-sheep-state) (first next-wolf3-state) (last next-wolf3-state) ) ) - previous-Q-value3)

  ; gets Q(s,a) + (alpha * (r + (lambda * max_a' Q(s',a') - Q(s,a)))
  let new-Q-value1 (previous-Q-value1 + (learning-rate * prediction-error1))
  let new-Q-value2 (previous-Q-value2 + (learning-rate * prediction-error2))
  let new-Q-value3 (previous-Q-value3 + (learning-rate * prediction-error3))

  ; sets new Q-value
  set-Q-value Q-values1 (first sheep-state)  (last sheep-state) (first wolf1-state) (last wolf1-state) new-Q-value1
  set-Q-value Q-values2 (first sheep-state)  (last sheep-state) (first wolf2-state) (last wolf2-state) new-Q-value2
  set-Q-value Q-values3 (first sheep-state)  (last sheep-state) (first wolf3-state) (last wolf3-state) new-Q-value3
end


;;;
;;;  Updates the Q-value for a given action according to SARSA algorithm update rule.
;;;  Tips:
;;;    - use "get-Q-value" and "set-Q-value" to update the action-value function
;;;    - properties "previous-xcor" and "previous-ycor" give access to the previous state
;;;
to update-SARSA

  let list-of-turtle-states (sort table:keys state-of-turtles)
  let next-sheep-state (table:get state-of-turtles (first list-of-turtle-states))
  let next-wolf1-state (table:get state-of-turtles (item 1 list-of-turtle-states))
  let next-wolf2-state (table:get state-of-turtles (item 2 list-of-turtle-states))
  let next-wolf3-state (table:get state-of-turtles (item 3 list-of-turtle-states))

  let prev-list-of-turtle-states (sort table:keys previous-state-of-turtles)
  let sheep-state (table:get previous-state-of-turtles (first prev-list-of-turtle-states))
  let wolf1-state (table:get previous-state-of-turtles (item 1 prev-list-of-turtle-states))
  let wolf2-state (table:get previous-state-of-turtles (item 2 prev-list-of-turtle-states))
  let wolf3-state (table:get previous-state-of-turtles (item 3 prev-list-of-turtle-states))



  ; get previous Q-value
  let previous-Q-value1 (get-Q-value Q-values1 (first sheep-state)  (last sheep-state) (first wolf1-state) (last wolf1-state) last-action )
  let previous-Q-value2 (get-Q-value Q-values2 (first sheep-state)  (last sheep-state) (first wolf2-state) (last wolf2-state) last-action )
  let previous-Q-value3 (get-Q-value Q-values3 (first sheep-state)  (last sheep-state) (first wolf3-state) (last wolf3-state) last-action )

  ; gets r + (lambda * max_a' Q(s',a')) - Q(s,a)
  let prediction-error1 (reward + (discount-factor * (get-Q-value Q-values1 (first next-sheep-state)  (last next-sheep-state) (first next-wolf1-state) (last next-wolf1-state) action ) ) - previous-Q-value1)
  let prediction-error2 (reward + (discount-factor * (get-Q-value Q-values2 (first next-sheep-state)  (last next-sheep-state) (first next-wolf2-state) (last next-wolf2-state) action) ) - previous-Q-value2)
  let prediction-error3 (reward + (discount-factor * (get-Q-value Q-values3 (first next-sheep-state)  (last next-sheep-state) (first next-wolf3-state) (last next-wolf3-state) action) ) - previous-Q-value3)

  ; gets Q(s,a) + (alpha * (r + (lambda * max_a' Q(s',a') - Q(s,a)))
  let new-Q-value1 (previous-Q-value1 + (learning-rate * prediction-error1))
  let new-Q-value2 (previous-Q-value2 + (learning-rate * prediction-error2))
  let new-Q-value3 (previous-Q-value3 + (learning-rate * prediction-error3))

  ; sets new Q-value
  set-Q-value Q-values1 (first sheep-state)  (last sheep-state) (first wolf1-state) (last wolf1-state) new-Q-value1
  set-Q-value Q-values2 (first sheep-state)  (last sheep-state) (first wolf2-state) (last wolf2-state) new-Q-value2
  set-Q-value Q-values3 (first sheep-state)  (last sheep-state) (first wolf3-state) (last wolf3-state) new-Q-value3

end




;;;
;;;  Gets the Q-value for a specific state-action pair
;;;
to-report get-Q-value [q-values x-sheep y-sheep x-wolf y-wolf action-wanted]
  let action-values get-Q-values q-values x-sheep y-sheep x-wolf y-wolf
  report array:item action-values (get-action-index action-wanted)
end


;;;
;;;  Gets the maximum Q-value for a specific state
;;;
to-report get-max-Q-value [q-values x-sheep y-sheep x-wolf y-wolf]
    report max array:to-list get-Q-values q-values x-sheep y-sheep x-wolf y-wolf
end


;;;
;;;  Sets the Q-value for a specific state-action pair
;;;
to set-Q-value [q-values x-sheep y-sheep x-wolf y-wolf value]
  array:set (get-Q-values q-values x-sheep y-sheep x-wolf y-wolf) (get-action-index last-action) value
end
@#$#@#$#@
GRAPHICS-WINDOW
300
25
545
248
-1
-1
38.5
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
4
0
4
0
0
1
ticks
30.0

SLIDER
1
21
216
54
Wolf_depth_of_field
Wolf_depth_of_field
1
(floor SizeOfMap - 1) / 2
2
1
1
patches
HORIZONTAL

BUTTON
37
61
101
94
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
121
61
184
94
go
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
53
106
164
139
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

SLIDER
15
268
187
301
max-episodes
max-episodes
0
10000
10000
1
1
NIL
HORIZONTAL

SLIDER
17
147
189
180
learning-rate
learning-rate
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
16
185
188
218
discount-factor
discount-factor
0
1
0.9
0.1
1
NIL
HORIZONTAL

MONITOR
733
355
847
400
NIL
get-episode-count
17
1
11

MONITOR
732
410
856
455
NIL
get-total-time-steps
17
1
11

SWITCH
14
306
178
339
diagonal-movement
diagonal-movement
1
1
-1000

SLIDER
7
378
179
411
reward-value
reward-value
0
1000
150
1
1
NIL
HORIZONTAL

PLOT
727
10
1854
187
Time performance
episode
time-steps
0.0
10.0
0.0
10.0
true
false
"" "set-plot-y-range  min-pycor max-pycor"
PENS
"time-steps" 1.0 0 -16777216 true "" ""
"average-time-steps" 1.0 0 -11221820 true "" ""

PLOT
728
189
1854
339
Reward performance
episode
total reward
0.0
10.0
0.0
10.0
true
false
"ask wolves [\n  let pen-name (word who \"reward\")\n  create-temporary-plot-pen pen-name\n  set-current-plot-pen pen-name\n  set-plot-pen-color (random color)\n]" ""
PENS

SLIDER
6
412
214
445
movement-probability-sheep
movement-probability-sheep
0
99
0
1
1
NIL
HORIZONTAL

SLIDER
6
446
178
479
reward-abort
reward-abort
-1
0
-0.1
0.1
1
NIL
HORIZONTAL

SWITCH
6
481
122
514
With-abort
With-abort
0
1
-1000

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
NetLogo 5.3
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
