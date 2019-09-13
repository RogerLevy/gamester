image /image erase
scene one named default
$ scene default curScene ref!


world0 $ scene default world ref!
add-pic myconid myconids.png
add-pic overworld overworld-tiles.png
add-role myconid myconid.f

$ pic myconid to this
set-animation 1   0 1 2 3

*myconid me named myboy

16 bank mytilemap
: garbage  a!> 512 for 512 for 8 rnd pfloor !+ loop loop ;
mytilemap garbage

$ scene default layer0 to this
mytilemap this tilemap-config ref!
$ pic overworld this tilemap-config cell+ ref!



save