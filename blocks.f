256 megs buffer: mem

decimal
: block  #2 rshift mem + ;
fixed

\ 0-1023: free blocks
\ 1024-2047: used blocks
\ 2048: root directory of 32 symbols ( name(28),block#(4) )

2048 constant root
: =>  ( block# adr c - block# | 0 )
    locals| c adr |
    block 32 for
        dup count adr c compare 0= if
            r> drop #28 + @
        ;then
        #32 +
    loop 0 ;

: new ( - block# )
    99999
;

: delete ( block# - )
    drop
;

: symbol  ( block# adr c dest# - ) 
    block 32 for
        dup @ 0= if dup >r place  r> #28 + !  r> drop  ;then
        #32 +
    loop -1 abort" Out of space in block" ;
    
    
struct: %project
    %project 4 cells sfield images
    %project 4 cells sfield sounds
    %project 4 cells sfield maps
    %project 4 cells sfield objects
    

: /table  a! 0 !+ new !+ new !+ new !+ new !+ 
    
: create-project  ( - <name> ) 
    new dup  bl parse  root symbol
    block >r
        r@ images /table
        r@ tilesets /table
        r@ sounds /table
        r@ maps /table
        r@ objects /table
    r> drop ;

: slot  ( n table - adr )
    cell+ over 256 / cells + @ block swap cells + ;

: lookup  ( n table - block# )
    slot @ ;

: ?full dup @ 1023 = abort" Table full" ;

: register  ( block# table - index )
    ?full   1 over +!  dup @ dup >r swap slot !  r> ;
    
: replace  ( block# index table - )
    slot dup @ delete ! ;
    

variable project
    
: create-image   new dup project @ images register ;
: create-sound   new dup project @ sounds register ;
: create-map     new dup project @ maps register ;
: create-object  new dup project @ objects register ;

