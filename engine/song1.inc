song1_header:
    .byte $04           ;4 streams
    
    .byte MUSIC_SQ1     ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte SQUARE_1      ;which channel
    .byte $70           ;initial duty (01)
    .byte ve_tgl_1      ;volume envelope
    .word song1_square1 ;pointer to stream
    .byte $53           ;tempo
    
    .byte MUSIC_SQ2     ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte SQUARE_2      ;which channel
    .byte $B0           ;initial duty (10)
    .byte ve_tgl_2      ;volume envelope
    .word song1_square2 ;pointer to stream
    .byte $53           ;tempo
    
    .byte MUSIC_TRI     ;which stream
    .byte $01           ;status byte (stream enabled)
    .byte TRIANGLE      ;which channel
    .byte $80           ;initial volume (on)
    .byte ve_tgl_2      ;volume envelope
    .word song1_tri     ;pointer to stream
    .byte $53           ;tempo
    
    .byte MUSIC_NOI     ;which stream
    .byte $00           ;disabled.  Our load routine will skip the
                        ;   rest of the reads if the status byte is 0.
                        ;   We are disabling Noise because we haven't covered it yet.
    
song1_square1:
    .byte eighth
    .byte A2, A2, A2, A3, A2, A3, A2, A3
    .byte F3, F3, F3, F4, F3, F4, F3, F4
    .byte A2, A2, A2, A3, A2, A3, A2, A3
    .byte F3, F3, F3, F4, F3, F4, F3, F4
    .byte E3, E3, E3, E4, E3, E4, E3, E4
    .byte E3, E3, E3, E4, E3, E4, E3, E4
    .byte Ds3, Ds3, Ds3, Ds4, Ds3, Ds4, Ds3, Ds4
    .byte D3, D3, D3, D4, D3, D4, D3, D4
    .byte C3, C3, C3, C4, C3, C4, C3, C4
    .byte B2, B2, B2, B3, B2, B3, B2, B3
    .byte As2, As2, As2, As3, As2, As3, As2, As3
    .byte A2, A2, A2, A3, A2, A3, A2, A3
    .byte Gs2, Gs2, Gs2, Gs3, Gs2, Gs3, Gs2, Gs3
    .byte G2, G2, G2, G3, G2, G3, G2, G3
    .byte $FF


    
song1_square2:
    .byte d_eighth
    .byte rest    ;offset for delay effect
    .byte eighth
    .byte A4, C5, B4, C5, A4, C5, B4, C5
    .byte A4, C5, B4, C5, A4, C5, B4, C5
    .byte A4, C5, B4, C5, A4, C5, B4, C5
    .byte A4, C5, B4, C5, A4, C5, B4, C5
    .byte Ab4, B4, A4, B4, Ab4, B4, A4, B4
    .byte B4, E5, D5, E5, B4, E5, D5, E5
    .byte A4, Eb5, C5, Eb5, A4, Eb5, C5, Eb5
    .byte A4, D5, Db5, D5, A4, D5, Db5, D5
    .byte A4, C5, F5, A5, C6, A5, F5, C5
    .byte Gb4, B4, Eb5, Gb5, B5, Gb5, Eb5, B4
    .byte F4, Bb4, D5, F5, Gs5, F5, D5, As4
    .byte E4, A4, Cs5, E5, A5, E5, sixteenth, Cs5, rest
    .byte eighth
    .byte Ds4, Gs4, C5, Ds5, Gs5, Ds5, C5, Gs4
    .byte sixteenth
    .byte G4, Fs4, G4, Fs4, G4, Fs4, G4, Fs4
    .byte eighth
    .byte G4, B4, D5, G5
    .byte $FF
    
song1_tri:
    .byte eighth
    .byte A5, C6, B5, C6, A5, C6, B5, C6 ;triangle data
    .byte A5, C6, B5, C6, A5, C6, B5, C6
    .byte A5, C6, B5, C6, A5, C6, B5, C6
    .byte A5, C6, B5, C6, A5, C6, B5, C6
    .byte Ab5, B5, A5, B5, Ab5, B5, A5, B5
    .byte B5, E6, D6, E6, B5, E6, D6, E6
    .byte A5, Eb6, C6, Eb6, A5, Eb6, C6, Eb6
    .byte A5, D6, Db6, D6, A5, D6, Db6, D6
    .byte A5, C6, F6, A6, C7, A6, F6, C6
    .byte Gb5, B5, Eb6, Gb6, B6, Gb6, Eb6, B5
    .byte F5, Bb5, D6, F6, Gs6, F6, D6, As5
    .byte E5, A5, Cs6, E6, A6, E6, Cs6, A5
    .byte Ds5, Gs5, C6, Ds6, Gs6, Ds6, C6, Gs5
    .byte sixteenth
    .byte G5, Fs5, G5, Fs5, G5, Fs5, G5, Fs5
    .byte G5, B5, D6, G6, B5, D6, B6, D7
    .byte $FF