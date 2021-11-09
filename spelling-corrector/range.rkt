#lang rhombus

export:
  end
  --

def end : #false

operator
| (a -- b) : [a,b]
| (  -- b) : [#false,b]

// 1--2
// 3--end
// --4
