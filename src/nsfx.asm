.include "consts.inc"

.segment "ZEROPAGE"
nsfx_disable_flag:     .res 1   ;a flag variable that keeps track of whether the sound engine is disabled or not. 
nsfx_frame_counter:    .res 1   ;a primitive counter used to time notes in this demo
nsfx_playing:          .res 1   ;a flag that tells us if our sound is playing or not.
nsfx_index:            .res 1   ;our current position in the sound data.

.segment "CODE"
.export nsfx_init
.export nsfx_disable
.export nsfx_load
.export nsfx_play_frame

.proc nsfx_init
    lda #%00001111
    sta APU_FLAGS   ;enable Square 1, Square 2, Triangle and Noise channels
    
    lda #$30
    sta SQ1_ENV     ;set Square 1 volume to 0
    sta SQ2_ENV     ;set Square 2 volume to 0
    sta NOISE_ENV   ;set Noise volume to 0
    lda #$80
    sta TRI_CTRL    ;silence Triangle
    
    lda #$00
    sta nsfx_disable_flag  ;clear disable flag
    ;later, if we have other variables we want to initialize, we will do that here.
    sta nsfx_playing
    sta nsfx_index
    sta nsfx_frame_counter
    rts
.endproc

.proc nsfx_disable
    lda #$00
    sta APU_FLAGS           ;disable all channels
    lda #$01
    sta nsfx_disable_flag  ;set disable flag
    rts
.endproc

.proc nsfx_load
    lda #$01
    sta nsfx_playing         ;set playing flag
    lda #$00
    sta nsfx_index           ;reset the index and counter
    sta nsfx_frame_counter
    rts
.endproc

.proc nsfx_pause
    lda #$00
    sta nsfx_playing
    rts
.endproc

.proc nsfx_unpause
    lda #$01
    sta nsfx_playing
    rts
.endproc

.proc nsfx_play_frame
    lda nsfx_disable_flag
    bne @done   ;if disable flag is set, don't advance a frame
    
    lda nsfx_playing
    beq @done  ;if our sound isn't playing, don't advance a frame
    
    inc nsfx_frame_counter     
    lda nsfx_frame_counter
    cmp #$08    ;***change this compare value to make the notes play faster or slower***
    bne @done   ;only take action once every 8 frames.
    
    ldy nsfx_index
    ;read the next byte from our sound data stream
    lda sfx1_data, y    ;***comment out this line and uncomment one of the ones below to play another data stream (data streams are located in sound_data.i)***
    ;lda sfx2_data, y
    ;lda sfx3_data, y
    
    cmp #$FF
    bne @note   ;if not #$FF, we have a note value
    lda #%00110000    ;else if #$FF, we are at the end of the sound data, so stop the sound and return
    sta SQ1_ENV
    lda #$00
    sta nsfx_playing
    sta nsfx_frame_counter
    rts
@note:          
    asl a       ;multiply by 2, because our note table is stored as words
    tay         ;we'll use this as an index into the note table
    
    lda NoteTable, y   ;read the low byte of our period from the table
    sta SQ1_LO
    lda NoteTable+1, y ;read the high byte of our period from the table
    sta SQ1_HI
    lda #%01111111    ;duty cycle 01, volume F
    sta SQ1_ENV
    lda #%10000000    ;set negate flag so low Square notes aren't silenced
    sta SQ1_SWEEP
    
    inc nsfx_index   ;move our index to the next byte position in the data stream
    lda #$00
    sta nsfx_frame_counter ;reset frame counter so we can start counting to 8 again.    
@done:
    rts
.endproc


sfx1_data:
    .byte A3, D3, Ds3, G3, C4, D4, Ds4, G4
    .byte C5, D5, Ds5, G5, C6, D6, Ds6, G6, C7, $FF     ;Cm/9
sfx2_data:
    .byte C3, E3, G3, B3, C4, E4, G4, B4
    .byte C5, E5, G5, B5, C6, E6, G6, B6, C7, $FF       ;Cmaj7
sfx3_data:
    .byte C3, Ds3, G3, A3, B3, C4, Ds4, G4, A4, B4 
    .byte C5, Ds5, G5, A5, B5, C6, Ds6, G6, $FF         ;Cm/7/6

.include "note_table.inc"