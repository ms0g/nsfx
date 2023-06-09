.include "apu.inc"
.include "consts.inc"

.segment "ZEROPAGE"
nsfx_apu_ports:         .res 16
nsfx_disable_flag:      .res 1  ;a flag variable that keeps track of whether the sound engine is disabled or not. 
nsfx_playing_flag:      .res 1  ;a flag that tells us if our sound is playing or not.
nsfx_sq1_old:           .res 1  ;the last value written to SQ1_HI
nsfx_sq2_old:           .res 1  ;the last value written to SQ2_HI
nsfx_temp1:             .res 1  ;temporary variables
nsfx_temp2:             .res 1
nsfx_ptr:               .res 2
nsfx_ptr2:              .res 2
jmp_ptr:                .res 2

;reserve 6 bytes, one for each stream
stream_curr_sound:      .res 6  ;current song/sfx loaded
stream_status:          .res 6  ;status byte.   bit0: (1: stream enabled; 0: stream disabled)
stream_channel:         .res 6  ;what channel is this stream playing on?
stream_ptr_LO:          .res 6  ;low byte of pointer to data stream
stream_ptr_HI:          .res 6  ;high byte of pointer to data stream
stream_vol_duty:        .res 6  ;stream volume/duty settings
stream_ve:              .res 6  ;current volume envelope
stream_ve_index:        .res 6  ;current position within the volume envelope
stream_note_LO:         .res 6  ;low 8 bits of period for the current note on a stream
stream_note_HI:         .res 6  ;high 3 bits of period for the current note on a stream 
stream_tempo:           .res 6  ;the value to add to our ticker total each frame
stream_ticker_total:    .res 6  ;our running ticker total
stream_loop1:           .res 6  ;loop counter variable (one for each stream)
stream_note_offset:     .res 6  ;note offset
stream_note_length:     .res 6  
stream_note_length_counter: .res 6

.segment "CODE"
.export nsfx_init
.export nsfx_disable
.export nsfx_load
.export nsfx_play_frame
.export nsfx_pause
.export nsfx_unpause

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
    lda #%00110000
    sta nsfx_apu_ports      ;set Square 1 volume to 0
    sta nsfx_apu_ports+4    ;set Square 2 volume to 0
    sta nsfx_apu_ports+12   ;set Noise volume to 0
    lda #%10000000
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
    sta nsfx_ptr
    lda song_headers+1, y
    sta nsfx_ptr+1

    ldy #$00
    lda (nsfx_ptr), y          ;stream counter
    sta nsfx_temp2              
    iny
@loop:
    lda (nsfx_ptr), y          ;stream number
    tax                         ;stream number acts as our variable index
    iny
    
    lda (nsfx_ptr), y          ;status byte. 1= enable, 0=disable
    sta stream_status, x
    beq @next_stream            ;if status byte is 0, stream disabled, so we are done
    iny
    
    lda (nsfx_ptr), y          ;channel number
    sta stream_channel, x
    iny
    
    lda (nsfx_ptr), y          ;initial duty and volume settings
    sta stream_vol_duty, x
    iny

    lda (nsfx_ptr), y          ;initial volume settings
    sta stream_ve, x
    iny
    
    lda (nsfx_ptr), y          ;pointer to stream data. Little endian, so low byte first
    sta stream_ptr_LO, x
    iny
    
    lda (nsfx_ptr), y
    sta stream_ptr_HI, x
    iny

    lda (nsfx_ptr), y
    sta stream_tempo, x

    lda #$A0
    sta stream_ticker_total, x
    
    lda #$01
    sta stream_note_length_counter, x

    lda #$00
    sta stream_ve_index, x

    lda #$00
    sta stream_loop1, x

    lda #$00
    sta stream_note_offset, x
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
    sta nsfx_ptr
    lda stream_ptr_HI, x
    sta nsfx_ptr+1
    
    ldy #$00
@fetch:
    lda (nsfx_ptr), y
    bpl @note                   ;if < #$80, it's a Note
    cmp #$A0
    bcc @note_length            ;else if < #$A0, it's a Note Length
@opcode:                        ;else it's an opcode
    jsr nsfx_opcode_launcher
    iny                      ;next position in the data stream
    lda stream_status, x
    and #%00000001
    bne @fetch               ;after our opcode is done, grab another byte unless the stream is disabled
    jmp @end
@note_length:
    ;do Note Length stuff
    and #%01111111              ;chop off bit7
    sty nsfx_temp1             ;save Y because we are about to destroy it
    tay
    lda note_length_table, y    ;get the note length count value
    sta stream_note_length, x   ;save the note length in RAM so we can use it to refill the counter
    sta stream_note_length_counter, x   ;stick it in our note length counter
    ldy nsfx_temp1         ;restore Y
    iny                     ;set index to next byte in the stream
    jmp @fetch              ;fetch another byte
@note:
    ;do Note stuff
    sta nsfx_temp2             ;save the note value
    lda stream_channel, x       ;what channel are we using?
    cmp #NOISE                  ;is it the Noise channel?
    bne @not_noise              
    jsr nsfx_do_noise             ;if so, JSR to a subroutine to handle noise data
    jmp @reset_ve                   ;and skip the note table when we return
@not_noise:
    lda nsfx_temp2     ;restore note value
    sty nsfx_temp1              ;save our index into the data stream
    clc
    adc stream_note_offset, x   ;add note offset
    asl a
    tay
    lda note_table, y
    sta stream_note_LO, x
    lda note_table+1, y
    sta stream_note_HI, x
    ldy nsfx_temp1              ;restore data stream index

    lda #$00
    sta stream_ve_index, x  ;reset the volume envelope.

    ;check if it's a rest
    jsr nsfx_check_rest 
@reset_ve:    
    lda #$00
    sta stream_ve_index, x  
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
    lda (nsfx_ptr), y  ;read the note byte again
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
; NSFX_OPCODE_LAUNCHER
; Will read an address from the opcode jump table and indirect jump there.
; input: 
;   A: opcode byte
;   Y: data stream position
;   X: stream number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_opcode_launcher
    sty nsfx_temp1         ;save y register, because we are about to destroy it
    sec
    sbc #$A0                ;turn our opcode byte into a table index by subtracting $A0
    asl a                   ;multiply by 2 because we index into a table of addresses (words)
    tay
    lda nsfx_opcodes, y    ;get low byte of subroutine address
    sta jmp_ptr
    lda nsfx_opcodes+1, y  ;get high byte
    sta jmp_ptr+1
    ldy nsfx_temp1         ;restore our y register
    iny                     ;set to next position in data stream (assume an argument)
    jmp (jmp_ptr)           ;indirect jump to our opcode subroutine
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
    
    jsr nsfx_set_stream_volume  ;volume
    
    lda #$08
    sta nsfx_apu_ports+1, y     ;sweep
    
    lda stream_note_LO, x
    sta nsfx_apu_ports+2, y     ;period LO
    
    lda stream_note_HI, x
    sta nsfx_apu_ports+3, y     ;period HI
    
    rts    
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_SET_STREAM_VOLUME
; input:
;   X: stream number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_set_stream_volume
    sty nsfx_temp1             ;save our index into nsfx_apu_ports (we are about to destroy y)
    
    lda stream_ve, x            ;which volume envelope?
    asl a                       ;multiply by 2 because we are indexing into a table of addresses (words)
    tay
    lda volume_envelopes, y     ;get the low byte of the address from the pointer table
    sta nsfx_ptr               ;put it into our pointer variable
    lda volume_envelopes+1, y   ;get the high byte of the address
    sta nsfx_ptr+1
@read_ve:
    ldy stream_ve_index, x      ;our current position within the volume envelope.
    lda (nsfx_ptr), y          ;grab the value.
    cmp #$FF
    bne @set_vol                ;if not FF, set the volume
    dec stream_ve_index, x      ;else if FF, go back one and read again
    jmp @read_ve                ;  FF essentially tells us to repeat the last
                                ;  volume value for the remainder of the note
@set_vol:
    sta nsfx_temp2             ;save our new volume value (about to destroy A)
    
    cpx #TRIANGLE               
    bne @squares                ;if not triangle channel, go ahead
    lda nsfx_temp2
    bne @squares                ;else if volume not zero, go ahead (treat same as squares)
    lda #%10000000
    bmi @store_vol              ;else silence the channel with #$80
@squares:
    lda stream_vol_duty, x      ;get current vol/duty settings
    and #$F0                    ;zero out the old volume
    ora nsfx_temp2             ;OR our new volume in.
@store_vol:
    ldy nsfx_temp1             ;get our index into nsfx_apu_ports
    sta nsfx_apu_ports, y       ;store the volume in our temp port
    inc stream_ve_index, x      ;set our volume envelop index to the next position
@rest_check:
    ;check the rest flag. if set, overwrite volume with silence value 
    lda stream_status, x
    and #%00000010
    beq @done                   ;if clear, no rest, so quit
    lda stream_channel, x
    cmp #TRIANGLE               ;if triangle, silence with #$80
    beq @tri                    ;else, silence with #$30
    lda #%00110000        
    bne @store                  ;this always branches.  bne is cheaper than a jmp
@tri:
    lda #%10000000
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
    sta SQ1_ENV
    lda nsfx_apu_ports+1
    sta SQ1_SWEEP
    lda nsfx_apu_ports+2
    sta SQ1_LO
    lda nsfx_apu_ports+3
    cmp nsfx_sq1_old       ;compare to last write
    beq @square2            ;don't write this frame if they were equal
    sta SQ1_HI
    sta nsfx_sq1_old       ;save the value we just wrote to SQ1_HI
@square2:
    lda nsfx_apu_ports+4
    sta SQ2_ENV
    lda nsfx_apu_ports+5
    sta SQ2_SWEEP
    lda nsfx_apu_ports+6
    sta SQ2_LO
    lda nsfx_apu_ports+7
    cmp nsfx_sq2_old
    beq @triangle
    sta SQ2_HI
    sta nsfx_sq2_old       ;save the value we just wrote to SQ2_HI
@triangle:
    lda nsfx_apu_ports+8
    sta TRI_CTRL
    lda nsfx_apu_ports+10   ;there is no $4009, so we skip it
    sta TRI_LO
    lda nsfx_apu_ports+11
    sta TRI_HI
@noise:
    lda nsfx_apu_ports+12
    sta NOI_ENV
    lda nsfx_apu_ports+14   ;there is no $400D, so we skip it
    sta NOI_RAND
    lda nsfx_apu_ports+15
    sta NOI_COUNT
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_DO_NOISE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_do_noise
    lda nsfx_temp2     ;restore the note value
    and #%00010000      ;isolate bit4
    beq @mode0          ;if it's clear, Mode-0, so no conversion
@mode1:
    lda nsfx_temp2     ;else Mode-1, restore the note value
    ora #%10000000      ;set bit 7 to set Mode-1
    sta nsfx_temp2
@mode0:
    lda nsfx_temp2
    sta stream_note_LO, x   ;temporary port that gets copied to $400E
    rts
.endproc

;;;;;;;;;;;;; NSFX OPCODE SUBROUTINES ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_ENDSOUND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_endsound
    lda stream_status, x    ;end of stream, so disable it and silence
    and #%11111110
    sta stream_status, x    ;clear enable flag in status byte
    
    lda stream_channel, x
    cmp #TRIANGLE
    beq @silence_tri        ;triangle is silenced differently from squares and noise
    lda #%00110000          ;squares and noise silenced with #$30
    bne @silence            ; (this will always branch.  bne is cheaper than a jmp)
@silence_tri:
    lda #%10000000  ;triangle silenced with #$80
@silence:
    sta stream_vol_duty, x  ;store silence value in the stream's volume variable.

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_INFINITE_LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_infinite_loop
    lda (nsfx_ptr), y      ;read ptr LO from the data stream
    sta stream_ptr_LO, x    ;update our data stream position
    iny
    lda (nsfx_ptr), y      ;read ptr HI from the data stream
    sta stream_ptr_HI, x    ;update our data stream position
    
    sta nsfx_ptr+1         ;update the pointer to reflect the new position.
    lda stream_ptr_LO, x
    sta nsfx_ptr
    ldy #$FF                ;after opcodes return, we do an iny.  Since we reset  
                            ;the stream buffer position, we will want y to start out at 0 again.
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_CHANGE_VE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_change_ve
    lda (nsfx_ptr), y      ;read the argument
    sta stream_ve, x        ;store it in our volume envelope variable
    lda #$00
    sta stream_ve_index, x  ;reset volume envelope index to the beginning
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_DUTY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_duty
    lda (nsfx_ptr), y
    sta stream_vol_duty, x
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_SET_LOOP1_COUNTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_set_loop1_counter
    lda (nsfx_ptr), y      ;read the argument (# times to loop)
    sta stream_loop1, x     ;store it in the loop counter variable
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_LOOP1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_loop1
    dec stream_loop1, x     ;decrement the counter
    lda stream_loop1, x     ;and check it
    beq @last_iteration     ;if zero, we are done looping
    jmp nsfx_op_infinite_loop ;if not zero, loop back
@last_iteration:
    iny                     ;skip the first byte of the address argument
                            ; the second byte will be skipped automatically upon return
                            ; (see nsfx_fetch_byte.  There is an "iny" after "jsr nsfx_opcode_launcher")
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_SET_NOTE_OFFSET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_set_note_offset
    lda (nsfx_ptr), y          ;read the argument
    sta stream_note_offset, x  ;set the note offset.
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_ADJUST_NOTE_OFFSET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_adjust_note_offset
    lda (nsfx_ptr), y          ;read the argument (what value to add)
    clc
    adc stream_note_offset, x   ;add it to the current offset
    sta stream_note_offset, x   ;and save.
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NSFX_OP_TRANSPOSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc nsfx_op_transpose
    lda (nsfx_ptr), y          ;read low byte of the pointer to our lookup table
    sta nsfx_ptr2              ;store it in a new pointer variable
    iny
    lda (nsfx_ptr), y          ;read high byte of pointer to table
    sta nsfx_ptr2+1
 
    sty nsfx_temp1              ;save y because we are about to destroy it
    lda stream_loop1, x         ;get loop counter, put it in Y
    tay                         ;   this will be our index into the lookup table
    dey                         ;subtract 1 because indexes start from 0.
 
    lda (nsfx_ptr2), y         ;read a value from the table.
    clc
    adc stream_note_offset, x   ;add it to the note offset
    sta stream_note_offset, x
 
    ldy nsfx_temp1              ;restore Y
    rts
.endproc

.include "note_table.inc"
.include "note_length_table.inc"
.include "vol_envelopes.inc"
.include "nsfx_opcodes.inc"
.include "../songs/songs_all.inc"
