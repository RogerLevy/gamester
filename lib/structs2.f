also venery
    
    struct %struct
        %struct %node sembed struct>node
        %struct svar struct.size

    struct %field
        %field %node sembed field>node
        %field svar field.offset
        %field svar field.size
        %field svar field.type

    struct %fieldtype
        %fieldtype svar typeid
        %fieldtype svar 'inspector   ( field size -- )
        %fieldtype svar embedder    \ struct

    : struct:  ( -- <name> struct offset )
        create here %struct *struct /node 0 ;
        
    : ;struct  ( struct offset -- )
        swap 2dup struct.size @ < abort" Struct definition overflow."
        struct.size ! ;
    
    : (.field)  ( adr size - )
        bounds ?do i @ dup if h. else i. then cell +loop ;
    
    : >lastfield  ( struct -- field )
        node.last @ ;
    
    : fieldtype:  ( id count inspector -- <name> ) ( struct offset -- struct offset )
        create >r drop @ , r> ,
        does> ( struct offset fieldtype ) third >lastfield field.type ! ;
    
    s" FIXP" ' (.field) fieldtype: <default
        
    : (create-field)  create does> [ 0 field.offset ]# + @ + ;
        
    : create-field  ( struct offset size - <name> struct offset+size )  ( adr - adr+n )
        0 locals| f size ofs struct |
        (create-field)
            %field *struct to f
            f /node  f struct push
            ofs f field.offset !
            size f field.size !
            size struct struct.size +!
        struct   ofs size +
        <default ;
        

previous

            
: sfield  ( struct offset - <name> struct offset )  ( adr - adr+n )
    create-field ;
        
: svar  ( struct offset - <name> offset )  ( adr - adr+n )
    cell sfield ;

: sizeof  ( struct - size )
    struct.size @ ;
    
: *struct  ( struct - adr )
    here swap sizeof /allot ;

: struct,  ( struct - )
    *struct drop ;

: (.fields)  ( adr struct -- adr+ ) 
    each> ( adr field )
        
        normal  
            ( field ) dup body> >name ccount type space
        bright
        
        ( field ) dup embedder ?dup if
            ( adr field struct ) nip recurse drop
        else
            2dup dup field.size @ swap field.type @ 'inspector @ execute        
        then
        
        field.size @ +
;

: .fields ( adr struct - )
    dup node.first @ field.offset @ u+  (.fields) drop ;

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
\ s" WSTR" , inspect-wstring fieldtype: <wstring
s" FLAG" ' inspect-flag fieldtype: <flag
s" BODY" ' inspect-body fieldtype: <body
s" XTXT" ' inspect-xt fieldtype: <xt
s" FIXP" ' inspect-fixed fieldtype: <fixed
