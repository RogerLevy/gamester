depend ramen/lib/std/kb.f

state: camera state1
    <left> kstate if -1 x +! then
    <right> kstate if 1 x +! then
    <up> kstate if -1 y +! then
    <down> kstate if 1 y +! then
    x 2@ stage scroll 2!
;
action: camera start  state1 ;
