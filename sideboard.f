( --== Modes ==-- )

#16 global mode

: create-mode   create does> dup body> >name ccount mode cplace  @ execute ;

: mode:  ( -- <name> <code> ; )
    get-order get-current common 
    create-mode here 0 , :noname swap !
    set-current set-order
;

