empty

: blockfile  s" mapster-dev.blk" ;
: prj-blockfile  project count blockfile strjoin ;
prj-blockfile  file-exists [if]
    prj-blockfile  16 megs 0file
[then]
blockfile ld prg/gamester/gamester

quit

displaywh 3 3 2/ resolution
playfield init-scene
playfield gameSlew >!

add-pic myconid ../data/myconids.png
add-pic overworld ../data/overworld-tiles.png
add-pic willy ../data/willyworm.png

import  myconid ../roles/myconid.f
import  willy ../roles/willy.f
import  camera ../roles/camera.f

pic( willy ) to this
    set-animation 1   0 1 2 1


16 bank mytilemap

: garbage  a!> 512 for 512 for 8 rnd pfloor !+ loop loop ;

mytilemap garbage

mytilemap pic( overworld ) stage layer0 init-layer

100 100 at
add-actor myconid  named myboy  start
0 0 at
add-actor camera   named camera  stop \  start
me camera >!
150 150 at
add-actor willy  named willy  start

install prg/gamester/tools/mapster.f mapster
install prg/gamester/tools/mapster.f mapster2

run mapster
mytilemap pic( overworld ) layer init-layer

cr .( Finished loading test code.) 