nsfx_opcodes:
    .word nsfx_op_endsound              ;$A0
    .word nsfx_op_infinite_loop         ;$A1
    .word nsfx_op_change_ve             ;$A2
    .word nsfx_op_duty                  ;$A3
    .word nsfx_op_set_loop1_counter     ;$A4
    .word nsfx_op_loop1                 ;$A5
    .word nsfx_op_set_note_offset       ;$A6
    .word nsfx_op_adjust_note_offset  ;$A7

;these are aliases to use in the sound data.
endsound            = $A0
loop                = $A1
volume_envelope     = $A2
duty                = $A3
set_loop1_counter   = $A4
loop1               = $A5
set_note_offset     = $A6
adjust_note_offset  = $A7