\ This is a test of editing functions.  Normally this stuff would be
\ done interactively by the user, while the engine is running, perhaps
\ with GUI-based tools.

image /image erase
scene one named default
scene( default ) curScene >!

world0 curscene @> world >!

add-pic myconid myconids.png
add-pic overworld overworld-tiles.png

add-role myconid myconid.f
add-template myconid
    role( myconid ) >role >!
    pic( myconid )  >pic >!

add-role camera camera.f
add-template camera
    role( camera ) >role >!
    

pic( myconid ) to this
    set-animation 1   0 1 2 3

16 bank mytilemap

: garbage  a!> 512 for 512 for 8 rnd pfloor !+ loop loop ;

mytilemap garbage

mytilemap pic( overworld ) curscene @> 0 init-layer

: add-actor  ( - <template> )
    template ($)  stage instance as ;

100 100 at
add-actor myconid  me named myboy
add-actor camera   me named camera
start