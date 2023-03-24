.include "consts.inc"
.include "header.inc"
.include "reset.inc"

.segment "ZEROPAGE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"
.import nsfx_init
.import nsfx_disable
.import nsfx_load
.import nsfx_play_frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    INIT_NES

    jsr nsfx_init
    jsr nsfx_load

    lda #$88
    sta $2000   ;enable NMIs


LoopForever:
    jmp LoopForever          ; Force an infinite execution loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
    pha     ;save registers
    txa
    pha
    tya
    pha

    jsr nsfx_play_frame

    pla     ;restore registers
    tay
    pla
    tax
    pla
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