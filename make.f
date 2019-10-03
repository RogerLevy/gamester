system clear-bank
gui clear-bank

tool off
lasttool off

: normalize-path
    >r
        r@ ccount s" data/" search if  r@ place  else  2drop  then
    r> drop
;

: normalize-pics
    pic each> path normalize-path
;

normalize-pics



: normalize-blkpath   \ can't use normalize-path ... different kinda counted string :'(
    blkpath count s" data/" search if  blkpath place  else  2drop  then
;

save
normalize-blkpath


:make cold
    revert
    load-pics
    playfield switchto
    quit
;

gild