REM clean up before calling assembler 
del asteroids.p 
del asteroids.lst
del asteroids.sym

call zxasm asteroids

REM call asteroids.p will auto run emulator EightyOne if installed
call asteroids.p