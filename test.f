\ This is a test of editing functions.  Normally this stuff would be
\ done interactively by the user, while the engine is running, perhaps
\ with GUI-based tools.

image /image erase

stage0 init-stage
stage0 curStage >!

add-pic myconid myconids.png
add-pic overworld overworld-tiles.png
add-pic willy willyworm.png

import  myconid myconid.f
import  willy willy.f
import  camera camera.f

pic( myconid ) to this
    set-animation 1   0 1 2 3

16 bank mytilemap

: garbage  a!> 512 for 512 for 8 rnd pfloor !+ loop loop ;

mytilemap garbage

mytilemap pic( overworld ) stage 0 init-layer

100 100 at
add-actor myconid  named myboy
add-actor camera   named camera     start
