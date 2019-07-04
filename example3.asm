;
;8-Bit addition with error checking
;
*=$0600
;
ADD8BITS CLD
CLC
LDA $5000
ADC $5001
BCS ERROR
STA $5002
RTS
ERROR RTS
