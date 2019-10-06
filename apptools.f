\ #2 #0 #0 [ws] [checkver]

depend ramen/lib/std/rangetools.f

: app-events
    etype ALLEGRO_EVENT_MOUSE_AXES = if
        evt ALLEGRO_MOUSE_EVENT.x 2@ 2p mouse xy!
    then
;

: maus   mouse 2@ globalscale dup 2/ ;
: mdelta  mouse 2@ mickey 2@ 2- ;

: (dialog)
    al_create_native_file_dialog
    display over al_show_native_file_dialog if
        0 al_get_native_file_dialog_path zcount rot ( dest ) place
        \ count -filename 2dup type cwd drop
        true
    else
        drop drop
        false
    then
;

: ?wd  dup count dup if -filename zstring else 2drop zwd then  ;

: ossave  ( dest filter c - flag )
    lb off
    2>r  ?wd  z" Save"  2r> zstring
        ALLEGRO_FILECHOOSER_SAVE (dialog)
;

: osopen  ( dest filter c - flag )
    lb off
    2>r  ?wd  z" Open"  2r> zstring
        ALLEGRO_FILECHOOSER_FILE_MUST_EXIST (dialog)
;

: image-formats  s" *.png;*.jpg;*.bmp;*.gif;*.tif" ;
