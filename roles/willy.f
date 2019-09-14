depend ramen/lib/std/kb.f

state: willy state1
    <left> kstate if -1 x +! then
    <right> kstate if 1 x +! then
    <up> kstate if -1 y +! then
    <down> kstate if 1 y +! then
    x 2@ curscene @> scroll 2!
;
action: willy start  state1 ;
