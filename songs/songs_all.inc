song_headers:                   ;this is our pointer table.  Each entry is a pointer to a song header 
    .word song0_header          ;this is a silence song.  See song0.i for more details
    .word song1_header          ;evil, demented notes
    .word song2_header          ;a sound effect.  Try playing it over the other songs.
    .word song3_header          ;a little chord progression.
    .word song4_header          ;a new song taking advantage of note lengths and rests
    .word song5_header          ;another sound effect played at a very fast tempo.
    .word song6_header

.include "song0.inc"              ;holds the data for song 0 (header and data streams)
.include "song1.inc"              ;holds the data for song 1
.include "song2.inc"
.include "song3.inc"
.include "song4.inc"
.include "song5.inc"
.include "song6.inc"