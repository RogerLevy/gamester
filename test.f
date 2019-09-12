image /image erase
scene one named default
world0 $ scene default world ref!
add-pic myconid prg/blocks/myconid.png

: *boy 
    stage one as
    $ pic myconid  picsrc ref!
    50 50 x 2!
;
*boy me named myboy



save