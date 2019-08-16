;
; Routine for moving a block of text
;
TXTBUF=$5000
EOL=$9B
;
*=$600
;
TEXT .BYTE $54,$41,$4B,$45,$20,$4D,$45,$20
;     .BYTE $54,$4F,$20,$59,$4F,$55,$52,$20
;     .BYTE $4C,$45,$41,$44,$45,$52,$21,$9B
;
DATMOV
;
;LDX #0
;LOOP LDA TEXT,X
;STA TXTBUF,X
;CMP #EOL
;BEQ FINI
;INX
;JMP LOOP
;FINI RTS
;.END
