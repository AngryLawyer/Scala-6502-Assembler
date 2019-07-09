;
; Cycle the background color
;
*=$0600
LOOP
LDA $02C8
ADC #2
STA $02C8
JMP LOOP
