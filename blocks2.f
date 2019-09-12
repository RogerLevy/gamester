64 megs buffer: work

: block  #2 rshift work + ;
: blocks  #2 rshift ;
: >slotva  block ;


: reserve  dup constant over constant + ;

0
4 reserve #classes classes       \ 128 x 32bytes
256 reserve #graphics graphics   \ describes images, sprite sheets, and tilesets
4096 reserve #tilemaps tilemaps  \ a tilemap is one block (!6x16 tiles)
256 reserve #sounds sounds       \ 
4096 reserve #actors actors      \ used for templates as well as gameplay
2048 reserve #dialogs dialogs    \ used for templates as well as gameplay.  point to files, but
                                 \ could also store some state.


cr . .( blocks used)


: graphic  graphics + block ;
: sound  sounds + block ;
: tilemap  tilemaps + block ;

: class  #32 * classes block + ;  \ class 0 is reserved for some variables
: class>name  count ;
: class>details  #28 + @ ;
: class>script  class>name  s" prg/blocks/" s[   +S  s" .f" +S  ]s ;


\ : block-variable  ( block# offset - block# offset+cell )
\     2dup swap block + constant cell+ ;
\     
\ classes 0
\     block-variable classSlot
\ 2drop



0 value (class#)

: export  ( class - )
    (class#) class #28 + ! ;

: load ( class# - )
    dup to (class#)
    class 
        class>script included ;


: path  s" prg/blocks/test.blk" ;
: write  blocks swap block swap path file! ;
: flush  work 64 megs path file! ;
: cache
    path work 64 megs @file
    classes >slotva @ for
        i 1 +  load
    loop
;


\ '\' as a prefix means it's an "Edit time command"

: \class  ( - <name> )
    1 classes >slotva +!
    bl parse classes >slotva @ dup >r  class place
    r> load
;


: table  create cells /allot does> swap cells + ;

: bclass: struct: lastbody export ;


cache
