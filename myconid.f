
state: myconid state1  2 rnd 2 rnd 1 1 2- x 2+! ;
action: myconid start  1 0.25 animate  state1 ;
: *myconid
    stage one as
    $ role myconid >role ref!
    $ pic myconid  >pic ref!
    50 50 x 2!
    start
;