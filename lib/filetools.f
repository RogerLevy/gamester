
: inflate  ( path c dest len -- )
    2swap file@ | size mem |
    ( dest len ) mem size 2swap decompress drop
    mem free drop ;

: deflate  ( src len path c -- )
    64 megs dup allocate throw | mem size |
    2>r  mem size compress mem swap 2r> file!
    mem free drop ;
