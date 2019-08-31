;.TITLE "PRNTSC ROUTINE"
;.PAGE "ROUTINES FOR PRINTING ON THE SCREEN"

*=$5000

BUFLEN=23

EOL=$98
;
OPEN=$03
OWRIT=$08
PUTCHR=$08
;
IOCB2=$20
ICCOM=$342
ICBAL=$344
ICBAH=$345
ICBLL=$348
ICBLH=$349
ICAX1=$34A
ICAX2=$34B
;
CIOV=$E456
;
SCRNAM .BYTE "E:",EOL
;
OSCR ;OPEN SCREEN ROUTINE
LDX #IOCB2
LDA #OPEN
STA ICCOM,X
;
LOA #SCRNAM&255
STA ICBAL,X
LOA #SCRNAM/256
STA ICBAH,X
;
LOA #OWRIT
STA ICAX1,X
LOA #0
STA ICAX2,X
JSR CIOV
;
LDA #PUTCHR
STA ICCOM,X
;
LOA #TXTBUF&255
STA ICBAL,X
LOA #TXTBUF/256
STA ICBAH,X
RTS
;
PRNT
LOX #IOCB2
LOA #BUFLEN&255
STA ICBLL,X
LOA #BUFLEN/256
STA ICBLH,X
JSR CIOV
RTS
;
TXTBUF=*
;
*= *+BUFLEN
;
.END