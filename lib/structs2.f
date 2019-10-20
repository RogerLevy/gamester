0 value lastfield

also venery
    
    struct %datatype
        %datatype %node sembed datatype>node
        %datatype svar datatype.size
        %datatype svar datatype.offset      
        %datatype svar datatype.type        \ datatype
        %datatype svar datatype.typeid      \ 4 chars
        %datatype svar datatype.inspector   \ XT ( adr size -- )
        %datatype svar datatype.embedder    \ datatype

    : *datatype  %datatype sizeof allotment dup /node ;

    : struct:  ( -- <name> struct offset )
        create *datatype 0 ;
        
    : ;struct  ( struct offset -- )
        swap 2dup datatype.size @ < abort" Struct definition overflow."
        datatype.size ! ;
    
    : (.field)  ( adr size - )
        bounds ?do i @ dup if h. else i. then cell +loop ;
            
    : >struct  ( field -- struct )
        node.parent @ ;
    
    : fieldtype:  ( id count inspector -- <name> ) ( struct offset -- struct offset )
        create
            *datatype >r
            r@ datatype.inspector !
            drop @ r@ datatype.typeid !
            r> drop
        does> ( struct offset fieldtype ) lastfield datatype.type ! ;
    
    s" FIXP" ' (.field) fieldtype: <default
        
    : (create-field)  create here to lastfield does> [ 0 datatype.offset ]# + @ + ;
        
    : create-field  ( struct offset size - <name> struct offset+size )  ( adr - adr+n )
        0 locals| f size ofs struct |
        (create-field) 
            *datatype to f
            f struct push
            ofs f datatype.offset !
            size f datatype.size !
            size struct datatype.size +!
        struct   ofs size +
        <default ;
        

previous

            
: sfield  ( struct offset - <name> struct offset )  ( adr - adr+n )
    create-field ;
        
: svar  ( struct offset - <name> offset )  ( adr - adr+n )
    cell sfield ;

: sizeof  ( struct - size )
    datatype.size @ ;

: sembed  ( struct size struct -- <name> )  ( adr -- adr+ofs )
    dup >r sizeof create-field  r> third node.last @ datatype.embedder ! ;
    
: *struct  ( struct - adr )
    here swap sizeof /allot ;

: struct,  ( struct - )
    *struct drop ;

: (.fields)  ( adr struct -- adr ) 
    each> ( adr field )
        
        normal  
            ( field ) dup body> >name ccount type space
        bright
        
        ( field ) dup datatype.embedder @ ?dup if
            >r udup datatype.offset @ + r> recurse drop
        else
            >r ( adr ) dup r@ datatype.offset @ + 
            ( adr adr+ofs ) r@ datatype.size @ r@ datatype.type @ datatype.inspector @ execute
            r> drop
        then         
;

: .fields ( adr struct - )
    dup node.first @ datatype.offset @ u+  (.fields) drop ;



: inspect-cstring drop ccount type ;
: inspect-string  drop count type ;
: inspect-flag    drop @ if ." true " else ." false " then ;
: inspect-body    drop @ .name ;
: inspect-xt      drop @ dup if >body .name else i. then ;
: inspect-fixed   bounds ?do i @ dup if p. else i. then cell +loop ;
: inspect-float   bounds ?do i sf@ f. ." e" cell +loop ; 
: inspect-hex     bounds ?do i @ dup 0= if #5 attribute then ." $" h. normal cell +loop ; 

s" HEXX" ' inspect-hex fieldtype: <hex
s" ADDR" ' inspect-hex fieldtype: <adr
s" ADDR" ' inspect-hex fieldtype: <addr
s" FLOT" ' inspect-float fieldtype: <float
s" CSTR" ' inspect-cstring fieldtype: <cstring
s" FLAG" ' inspect-flag fieldtype: <flag
s" BODY" ' inspect-body fieldtype: <body
s" XTXT" ' inspect-xt fieldtype: <xt
s" FIXP" ' inspect-fixed fieldtype: <fixed
