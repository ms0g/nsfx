.include "apu.inc"
.include "consts.inc"

.segment "ZEROPAGE"
nsfx_apu_ports:         .res 16
nsfx_disable_flag:      .res 1  ;a flag variable that keeps track of whether the sound engine is disabled or not. 
nsfx_playing_flag:      .res 1  ;a flag that tells us if our sound is playing or not.
nsfx_sq1_old:           .res 1  ;the last value written to $4003
nsfx_sq2_old:           .res 1  ;the last value written to $4007
nsfx_temp1:             .res 1  ;temporary variables
nsfx_temp2:             .res 1
sound_ptr:              .res 2


;reserve 6 bytes, one for each stream
stream_curr_sound:      .res 6  ;current song/sfx loaded
stream_status:          .res 6  ;status byte.   bit0: (1: stream enabled; 0: stream disabled)
stream_channel:         .res 6  ;what channel is this stream playing on?
stream_ptr_LO:          .res 6  ;low byte of pointer to data stream
stream_ptr_HI:          .res 6  ;high byte of pointer to data stream
stream_vol_duty:        .res 6  ;stream volume/duty settings
stream_note_LO:         .res 6  ;low 8 bits of period for the current note on a stream
stream_note_HI:         .res 6  ;high 3 bits of period for the current note on a stream 
stream_tempo:           .res 6  ;the value to add to our ticker total each frame
stream_ticker_total:    .res 6  ;our running ticker total
stream_note_length:     .res 6  
stream_note_length_counter: .res 6

.segment "CODE"
.export nsfx_init
.export nsfx_disable
.export nsfx_load
.export nsfx_play_frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_init
    lda #%00001111
    sta APU_FLAGS               ;enable Square 1, Square 2, Triangle and Noise channels
    
    lda #$00
    sta nsfx_disable_flag       ;clear disable flag
    sta nsfx_playing_flag

    lda #$FF
    sta nsfx_sq1_old
    sta nsfx_sq2_old

    jsr nsfx_mute
    
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_DISABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_disable
    lda #$00
    sta APU_FLAGS               ;disable all channels
    lda #$01
    sta nsfx_disable_flag       ;set disable flag
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_MUTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_mute
    lda #$30
    sta nsfx_apu_ports      ;set Square 1 volume to 0
    sta nsfx_apu_ports+4    ;set Square 2 volume to 0
    sta nsfx_apu_ports+12   ;set Noise volume to 0
    lda #$80
    sta nsfx_apu_ports+8     ;silence Triangle
    rts
.endproc 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_LOAD
; input:
;   A: song/sfx number to play
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_load
    ldy #$01
    sty nsfx_playing_flag       ;set playing flag

    sta nsfx_temp1              ;save song number
    asl a                       ;multiply by 2.  We are indexing into a table of pointers (words)
    tay
    lda song_headers, y         ;setup the pointer to our song header
    sta sound_ptr
    lda song_headers+1, y
    sta sound_ptr+1

    ldy #$00
    lda (sound_ptr), y          ;stream counter
    sta nsfx_temp2              
    iny
@loop:
    lda (sound_ptr), y          ;stream number
    tax                         ;stream number acts as our variable index
    iny
    
    lda (sound_ptr), y          ;status byte. 1= enable, 0=disable
    sta stream_status, x
    beq @next_stream            ;if status byte is 0, stream disabled, so we are done
    iny
    
    lda (sound_ptr), y          ;channel number
    sta stream_channel, x
    iny
    
    lda (sound_ptr), y          ;initial duty and volume settings
    sta stream_vol_duty, x
    iny
    
    lda (sound_ptr), y          ;pointer to stream data. Little endian, so low byte first
    sta stream_ptr_LO, x
    iny
    
    lda (sound_ptr), y
    sta stream_ptr_HI, x
    iny

    lda (sound_ptr), y
    sta stream_tempo, x

    lda #$A0
    sta stream_ticker_total, x
    
    lda #$01
    sta stream_note_length_counter,x
@next_stream:
    iny
    
    lda nsfx_temp1              ;song number
    sta stream_curr_sound, x
    
    dec nsfx_temp2              ;our loop counter
    bne @loop

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_PAUSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_pause
    lda #$00
    sta nsfx_playing_flag
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_UNPAUSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_unpause
    lda #$01
    sta nsfx_playing_flag
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_PLAY_FRAME
; Advances the sound engine by one frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_play_frame
    lda nsfx_disable_flag
    bne @done                   ;if disable flag is set, don't advance a frame
    
    lda nsfx_playing_flag
    beq @done                   ;if our sound isn't playing, don't advance a frame
    
    ;silence all channels.The purpose of this subroutine call is to silence channels that aren't used by any streams.
    jsr nsfx_mute

    ldx #$00
@loop:
    lda stream_status, x
    and #$01    ;check whether the stream is active
    beq @endloop    ;if the channel isn't active, skip it
    
    ;add the tempo to the ticker total.  If there is a FF-> 0 transition, there is a tick
    lda stream_ticker_total, x
    clc
    adc stream_tempo, x
    sta stream_ticker_total, x
    bcc @set_buffer    ;carry clear = no tick.  if no tick, we are done with this stream

    dec stream_note_length_counter, x   ;else there is a tick. decrement the note length counter
    bne @set_buffer    ;if counter is non-zero, our note isn't finished playing yet
    
    lda stream_note_length, x   ;else our note is finished. reload the note length counter
    sta stream_note_length_counter, x

    jsr nsfx_fetch_byte
@set_buffer:
    ;copy the current stream's sound data for the current frame into our temporary APU vars (nsfx_apu_ports)
    jsr nsfx_set_temp_ports
@endloop:
    inx
    cpx #$06
    bne @loop

    jsr nsfx_set_apu
@done:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_FETCH_BYTE
; Reads one byte from a sound data stream and handles it
; input: 
;   X: stream number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_fetch_byte
    lda stream_ptr_LO, x
    sta sound_ptr
    lda stream_ptr_HI, x
    sta sound_ptr+1
    
    ldy #$00
@fetch:
    lda (sound_ptr), y
    bpl @note                   ;if < #$80, it's a Note
    cmp #$A0
    bcc @note_length            ;else if < #$A0, it's a Note Length
@opcode:                        ;else it's an opcode
    ;do Opcode stuff
    cmp #$FF
    bne @end
    lda stream_status, x        ;if $FF, end of stream, so disable it and silence
    and #%11111110
    sta stream_status, x        ;clear enable flag in status byte
    
    lda stream_channel, x
    cmp #TRIANGLE
    beq @silence_tri            ;triangle is silenced differently from squares and noise
    lda #%00110000              ;squares and noise silenced with #$30
    bne @silence
@silence_tri:
    lda #%10000000              ;triangle silenced with #$80
@silence:
    sta stream_vol_duty, x      ;store silence value in the stream's volume variable.
    jmp @update_pointer         ;done
@note_length:
    ;do Note Length stuff
    and #%01111111              ;chop off bit7
    sty nsfx_temp1             ;save Y because we are about to destroy it
    tay
    lda NoteLengthTable, y    ;get the note length count value
    sta stream_note_length, x   ;save the note length in RAM so we can use it to refill the counter
    sta stream_note_length_counter, x   ;stick it in our note length counter
    ldy nsfx_temp1         ;restore Y
    iny                     ;set index to next byte in the stream
    jmp @fetch              ;fetch another byte
@note:
    ;do Note stuff
    sty nsfx_temp1              ;save our index into the data stream
    asl a
    tay
    lda NoteTable, y
    sta stream_note_LO, x
    lda NoteTable+1, y
    sta stream_note_HI, x
    ldy nsfx_temp1              ;restore data stream index

    ;check if it's a rest
    jsr nsfx_check_rest 
@update_pointer:
    iny
    tya
    clc
    adc stream_ptr_LO, x
    sta stream_ptr_LO, x
    bcc @end
    inc stream_ptr_HI, x
@end:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_CHECK_REST
; Determine if it is a rest or not.  It will set or clear the current
; stream's rest flag accordingly.
; input:
;   X: stream number
;   Y: data stream index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_check_rest
    lda (sound_ptr), y  ;read the note byte again
    cmp #rest
    bne @not_rest
    lda stream_status, x
    ora #%00000010  ;set the rest bit in the status byte
    bne @store  ;this will always branch.  bne is cheaper than a jmp.
@not_rest:
    lda stream_status, x
    and #%11111101  ;clear the rest bit in the status byte
@store:
    sta stream_status, x
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_SET_TEMP_PORTS
; Will copy a stream's sound data to the temporary apu variables
; input:
;   X: stream number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_set_temp_ports
    lda stream_channel, x
    asl a
    asl a
    tay
    
    lda stream_vol_duty, x
    sta nsfx_apu_ports, y       ;vol
    
    lda #$08
    sta nsfx_apu_ports+1, y     ;sweep
    
    lda stream_note_LO, x
    sta nsfx_apu_ports+2, y     ;period LO
    
    lda stream_note_HI, x
    sta nsfx_apu_ports+3, y     ;period HI
    
    ;check the rest flag. if set, overwrite volume with silence value 
    lda stream_status, x
    and #%00000010
    beq @done       ;if clear, no rest, so quit
    lda stream_channel, x
    cmp #TRIANGLE   ;if triangle, silence with #$80
    beq @tri        ;else, silence with #$30
    lda #$30        
    bne @store
@tri:
    lda #$80
@store:    
    sta nsfx_apu_ports, y
@done:
    rts    
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_SET_APU
; Copies the temporary RAM ports to the APU ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_set_apu
@square1:
    lda nsfx_apu_ports+0
    sta $4000
    lda nsfx_apu_ports+1
    sta $4001
    lda nsfx_apu_ports+2
    sta $4002
    lda nsfx_apu_ports+3
    cmp nsfx_sq1_old       ;compare to last write
    beq @square2            ;don't write this frame if they were equal
    sta $4003
    sta nsfx_sq1_old       ;save the value we just wrote to $4003
@square2:
    lda nsfx_apu_ports+4
    sta $4004
    lda nsfx_apu_ports+5
    sta $4005
    lda nsfx_apu_ports+6
    sta $4006
    lda nsfx_apu_ports+7
    cmp nsfx_sq2_old
    beq @triangle
    sta $4007
    sta nsfx_sq2_old       ;save the value we just wrote to $4007
@triangle:
    lda nsfx_apu_ports+8
    sta $4008
    lda nsfx_apu_ports+10   ;there is no $4009, so we skip it
    sta $400A
    lda nsfx_apu_ports+11
    sta $400B
@noise:
    lda nsfx_apu_ports+12
    sta $400C
    lda nsfx_apu_ports+14   ;there is no $400D, so we skip it
    sta $400E
    lda nsfx_apu_ports+15
    sta $400F
    rts
.endproc


NUM_SONGS = $04                 ;if you add a new song, change this number.    
                                ;headers.asm checks this number in its song_up and song_down subroutines
                                ;to determine when to wrap around.

               
song_headers:                   ;this is our pointer table.  Each entry is a pointer to a song header 
    .word song0_header          ;this is a silence song.  See song0.i for more details
    .word song1_header          ;evil, demented notes
    .word song2_header          ;a sound effect.  Try playing it over the other songs.
    .word song3_header          ;a little chord progression.
    .word song4_header          ;a new song taking advantage of note lengths and rests
    .word song5_header          ;another sound effect played at a very fast tempo.

.include "note_table.inc"
.include "note_length_table.inc"
.include "song0.s"              ;holds the data for song 0 (header and data streams)
.include "song1.s"              ;holds the data for song 1
.include "song2.s"
.include "song3.s"
.include "song4.s"
.include "song5.s"