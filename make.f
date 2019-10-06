system clear-bank
gui clear-bank

quit

: normalize-path
    >r
        r@ ccount s" data/" search if  r@ cplace  else  2drop  then
    r> drop
;

: normalize-pics
    pic each> path normalize-path
;

normalize-pics

\ save out another block file to data/
image /image project count s[ s" data/" +s blkpath count -path +s ]s file!


: normalize-blkpath   \ can't use normalize-path ... different kinda counted string :'(
    s" data/" blkpath count -path strjoin blkpath place
;

\ and change the blkpath to point to that instead
normalize-blkpath


:make cold
    revert
    load-pics
    playfield switchto
    quit
;

gild

