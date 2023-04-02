nsfx_opcodes:
    .word nsfx_op_endsound            ;$A0
    .word nsfx_op_infinite_loop       ;$A1
    .word nsfx_op_change_ve           ;$A2
    .word nsfx_op_duty                ;$A3

;these are aliases to use in the sound data.
endsound            = $A0
loop                = $A1
volume_envelope     = $A2
duty                = $A3