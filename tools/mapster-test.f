empty

: blockfile  s" mapster-dev.blk" ;
: prj-blockfile  project count blockfile strjoin ;
prj-blockfile  file-exists [if]
    prj-blockfile -file
[then]
blockfile ld prg/gamester/gamester

quit

displaywh 3 3 2/ resolution
playfield init-scene
playfield >stage >!

add-pic overworld prg/gamester/data/overworld-tiles.png
add-pic myconid   prg/gamester/data/myconids.png
add-pic willy     prg/gamester/data/willyworm.png
import  myconid   prg/gamester/roles/myconid.f
import  willy     prg/gamester/roles/willy.f
import  camera    prg/gamester/roles/camera.f

pic( willy ) to this
    set-animation 1   0 1 2 1

( --== Create a tilemap ==-- )
16 bank mytilemap

: garbage  a!> 512 for 512 for 8 rnd pfloor !+ loop loop ;

mytilemap garbage

mytilemap pic( overworld ) stage layer0 init-layer

( --== Create a game world to test out the map ==-- )

100 100 at
add-instance myconid  named myboy  start

0 0 at
add-instance camera   named camera  stop
me camera >!

150 150 at
add-instance willy  named willy  start

install prg/gamester/tools/mapster.f mapster

run mapster
mytilemap pic( overworld ) layer init-layer

displaywh 2 2 2/ gui res 2!

cr .( Finished loading test code.) 