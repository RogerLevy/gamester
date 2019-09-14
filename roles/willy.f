depend ramen/lib/std/kb.f

state: willy state1
    <left> kstate if -1 x +! then
    <right> kstate if 1 x +! then
    <up> kstate if -1 y +! then
    <down> kstate if 1 y +! then
;
action: willy start  state1 ;
