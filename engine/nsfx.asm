.include "apu.inc"

SQUARE_1 = $00 ;these are channel constants
SQUARE_2 = $01
TRIANGLE = $02
NOISE = $03

MUSIC_SQ1 = $00 ;these are stream # constants
MUSIC_SQ2 = $01 ;stream # is used to index into variables
MUSIC_TRI = $02
MUSIC_NOI = $03
SFX_1     = $04
SFX_2     = $05

.segment "ZEROPAGE"
nsfx_disable_flag:      .res 1   ;a flag variable that keeps track of whether the sound engine is disabled or not. 
nsfx_playing_flag:      .res 1   ;a flag that tells us if our sound is playing or not.
nsfx_frame_counter:     .res 1   ;a primitive counter used to time notes in this demo
nsfx_index:             .res 1   ;our current position in the sound data.
sound_temp1:            .res 1           ;temporary variables
sound_temp2:            .res 1
sound_ptr:              .res 2

;reserve 6 bytes, one for each stream
stream_curr_sound:      .res 6     ;current song/sfx loaded
stream_status:          .res 6         ;status byte.   bit0: (1: stream enabled; 0: stream disabled)
stream_channel:         .res 6        ;what channel is this stream playing on?
stream_ptr_LO:          .res 6         ;low byte of pointer to data stream
stream_ptr_HI:          .res 6         ;high byte of pointer to data stream
stream_vol_duty:        .res 6       ;stream volume/duty settings
stream_note_LO:         .res 6        ;low 8 bits of period for the current note on a stream
stream_note_HI:         .res 6        ;high 3 bits of period for the current note on a stream 

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
    sta nsfx_playing_flag
    sta nsfx_frame_counter
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_DISABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_disable
    lda #$00
    sta APU_FLAGS           ;disable all channels
    lda #$01
    sta nsfx_disable_flag  ;set disable flag
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_MUTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_mute
    lda #$30
    sta SQ1_ENV     ;set Square 1 volume to 0
    sta SQ2_ENV     ;set Square 2 volume to 0
    sta NOISE_ENV   ;set Noise volume to 0
    lda #$80
    sta TRI_CTRL    ;silence Triangle
    rts
.endproc 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_LOAD
; input:
;   A: song/sfx number to play
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_load
    ldy #$01
    sty nsfx_playing_flag         ;set playing flag

    sta sound_temp1         ;save song number
    asl a                   ;multiply by 2.  We are indexing into a table of pointers (words)
    tay
    lda song_headers, y     ;setup the pointer to our song header
    sta sound_ptr
    lda song_headers+1, y
    sta sound_ptr+1

    ldy #$00
    lda (sound_ptr), y      ;read the first byte: # streams
    sta sound_temp2         ;store in a temp variable.  We will use this as a loop counter: how many streams to read stream headers for
    iny
@loop:
    lda (sound_ptr), y      ;stream number
    tax                     ;stream number acts as our variable index
    iny
    
    lda (sound_ptr), y      ;status byte.  1= enable, 0=disable
    sta stream_status, x
    beq @next_stream        ;if status byte is 0, stream disabled, so we are done
    iny
    
    lda (sound_ptr), y      ;channel number
    sta stream_channel, x
    iny
    
    lda (sound_ptr), y      ;initial duty and volume settings
    sta stream_vol_duty, x
    iny
    
    lda (sound_ptr), y      ;pointer to stream data.  Little endian, so low byte first
    sta stream_ptr_LO, x
    iny
    
    lda (sound_ptr), y
    sta stream_ptr_HI, x
@next_stream:
    iny
    
    lda sound_temp1         ;song number
    sta stream_curr_sound, x
    
    dec sound_temp2         ;our loop counter
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
    bne @done   ;if disable flag is set, don't advance a frame
    
    lda nsfx_playing_flag
    beq @done  ;if our sound isn't playing, don't advance a frame
    
    inc nsfx_frame_counter     
    lda nsfx_frame_counter
    cmp #$0C    ;***change this compare value to make the notes play faster or slower***
    bne @done   ;only take action once every 8 frames.
    
    ;silence all channels.  se_set_apu will set volume later for all channels that are enabled.
    ;the purpose of this subroutine call is to silence channels that aren't used by any streams.
    jsr nsfx_mute

    ldx #$00
@loop:
    lda stream_status, x
    and #$01    ;check whether the stream is active
    beq @endloop  ;if the channel isn't active, skip it
    jsr nsfx_fetch_byte
    jsr nsfx_set_apu
@endloop:
    inx
    cpx #$06
    bne @loop

    lda #$00
    sta nsfx_playing_flag
    sta nsfx_frame_counter
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
    lda (sound_ptr), y
    bpl @note                ;if < #$80, it's a Note
    cmp #$A0
    bcc @note_length         ;else if < #$A0, it's a Note Length
@opcode:                     ;else it's an opcode
    ;do Opcode stuff
    cmp #$FF
    bne @end
    lda stream_status, x    ;if $FF, end of stream, so disable it and silence
    and #%11111110
    sta stream_status, x    ;clear enable flag in status byte
    
    lda stream_channel, x
    cmp #TRIANGLE
    beq @silence_tri        ;triangle is silenced differently from squares and noise
    lda #$30                ;squares and noise silenced with #$30
    bne @silence
@silence_tri:
    lda #$80                ;triangle silenced with #$80
@silence:
    sta stream_vol_duty, x  ;store silence value in the stream's volume variable.
    jmp @update_pointer     ;done
@note_length:
    ;do Note Length stuff
    jmp @update_pointer    ;not implemented yet
@note:
    ;do Note stuff
    sty sound_temp1     ;save our index into the data stream
    asl a
    tay
    lda NoteTable, y
    sta stream_note_LO, x
    lda NoteTable+1, y
    sta stream_note_HI, x
    ldy sound_temp1     ;restore data stream index
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
; NSFX_SET_APU
; Writes a stream's data to the APU ports
; input: 
;   X: stream number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_set_apu
 lda stream_channel, x
    asl a
    asl a                   ;multiply by 4 so our index will point to the right set of registers
    tay
    lda stream_vol_duty, x
    sta SQ1_ENV, y
    lda stream_note_LO, x
    sta SQ1_LO, y
    lda stream_note_HI, x
    sta SQ1_HI, y
    
    lda stream_channel, x
    cmp #TRIANGLE
    bcs @end        ;if Triangle or Noise, skip this part
    lda #$08        ;else, set negate flag in sweep unit to allow low noteson Squares
    sta SQ1_SWEEP, y
@end:
    rts
.endproc

NUM_SONGS = $04 ;if you add a new song, change this number.    
                ;headers.asm checks this number in its song_up and song_down subroutines
                ;to determine when to wrap around.

;this is our pointer table.  Each entry is a pointer to a song header                
song_headers:
    .word song0_header  ;this is a silence song.  See song0.i for more details
    .word song1_header  ;evil, demented notes
    .word song2_header  ;a sound effect.  Try playing it over the other songs.
    .word song3_header  ;a little chord progression.

.include "note_table.inc"
.include "song0.i"  ;holds the data for song 0 (header and data streams)
.include "song1.i"  ;holds the data for song 1
.include "song2.i"
.include "song3.i"