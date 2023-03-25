.include "engine/consts.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The iNES header (contains a total of 16 bytes with the flags at $7F00)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "HEADER"
.byte $4E,$45,$53,$1A        ; 4 bytes with the characters 'N','E','S','\n'
.byte $02                    ; How many 16KB of PRG-ROM we'll use (=32KB)
.byte $01                    ; How many 8KB of CHR-ROM we'll use (=8KB)
.byte %00000000              ; Horz mirroring, no battery, mapper 0
.byte %00000000              ; mapper 0, playchoice, NES 2.0
.byte $00                    ; No PRG-RAM
.byte $00                    ; NTSC TV format
.byte $00                    ; Extra flags for TV format and PRG-RAM
.byte $00,$00,$00,$00,$00    ; Unused padding to complete 16 bytes of header

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
    sei                      ; Disable all IRQ interrupts
    cld                      ; Clear decimal mode (not supported by the NES)
    ldx #$FF
    txs                      ; Initialize the stack pointer at address $FF

    inx                      ; Increment X, causing a rolloff from $FF to $00
    stx PPU_CTRL             ; Disable NMI
    stx PPU_MASK             ; Disable rendering (masking background and sprites)
    stx $4010                ; Disable DMC IRQs
    
    lda #$40
    sta $4017                ; Disable APU frame IRQ

    bit PPU_STATUS           ; Read from PPU_STATUS to reset the VBlank flag
Wait1stVBlank:               ; Wait for the first VBlank from the PPU
    bit PPU_STATUS           ; Perform a bit-wise check with the PPU_STATUS port
    bpl Wait1stVBlank        ; Loop until bit-7 (sign bit) is 1 (inside VBlank)

    txa                      ; A = 0
ClearRAM:
    sta $0000,x              ; Zero RAM addresses from $0000 to $00FF
    sta $0100,x              ; Zero RAM addresses from $0100 to $01FF
    sta $0200,x              ; Zero RAM addresses from $0200 to $02FF
    sta $0300,x              ; Zero RAM addresses from $0300 to $03FF
    sta $0400,x              ; Zero RAM addresses from $0400 to $04FF
    sta $0500,x              ; Zero RAM addresses from $0500 to $05FF
    sta $0600,x              ; Zero RAM addresses from $0600 to $06FF
    sta $0700,x              ; Zero RAM addresses from $0700 to $07FF
    inx
    bne ClearRAM

Wait2ndVBlank:               ; Wait for the second VBlank from the PPU
    bit PPU_STATUS           ; Perform a bit-wise check with the PPU_STATUS port
    bpl Wait2ndVBlank        ; Loop until bit-7 (sign bit) is 1 (inside VBlank)

Main:
    jsr nsfx_init
    jsr nsfx_load

    lda #%10010000           ; Enable NMI and set background to use the 2nd pattern table (at $1000)
    sta PPU_CTRL


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