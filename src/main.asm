.include "consts.inc"
.include "header.inc"
.include "reset.inc"

.segment "ZEROPAGE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    INIT_NES

    lda #%00000111  ;enable Sq1, Sq2 and Tri channels
    sta APU_FLAGS
 
    ;Square 1
    lda #%00111000  ;Duty 00, Volume 8 (half volume)
    sta SQ1_ENV
    lda #$C9        ;$0C9 is a C# in NTSC mode
    sta SQ1_LO      ;low 8 bits of period
    lda #$00
    sta SQ1_HI      ;high 3 bits of period
 
    ;Square 2
    lda #%01110110  ;Duty 01, Volume 6
    sta SQ2_ENV
    lda #$A9        ;$0A9 is an E in NTSC mode
    sta SQ2_LO
    lda #$00
    sta SQ2_HI
 
    ;Triangle
    lda #%10000001  ;Triangle channel on
    sta TRI_CTRL
    lda #$42        ;$042 is a G# in NTSC mode
    sta TRI_LO
    lda #$00
    sta TRI_HI

LoopForever:
    jmp LoopForever          ; Force an infinite execution loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
    rti                      ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRQ interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti                      ; Return from interrupt

.segment "VECTORS"
.word NMI                    ; Address (2 bytes) of the NMI handler
.word Reset                  ; Address (2 bytes) of the Reset handler
.word IRQ                    ; Address (2 bytes) of the IRQ handler