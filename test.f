image /image erase
scene one named default
$( scene default ) curScene >!


world0 curscene @> world >!


add-pic myconid myconids.png
add-pic overworld overworld-tiles.png
add-role myconid myconid.f

$( pic myconid ) to this
    set-animation 1   0 1 2 3

*myconid me named myboy

16 bank mytilemap
: garbage  a!> 512 for 512 for 8 rnd pfloor !+ loop loop ;
mytilemap garbage

curscene@ layer0 to this
    mytilemap this tilemap-config >!
    $( pic overworld ) this tilemap-config cell+ >!
    1 1 this parallax 2!


save