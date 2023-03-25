;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for PPU registers mapped from addresses $2000 to $2007
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PPU_CTRL    = $2000
PPU_MASK    = $2001
PPU_STATUS  = $2002
OAM_ADDR    = $2003
OAM_DATA    = $2004
PPU_SCROLL  = $2005
PPU_ADDR    = $2006
PPU_DATA    = $2007
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for APU registers mapped from addresses $4000 to $4017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SQ1_ENV     = $4000
SQ1_SWEEP   = $4001
SQ1_LO      = $4002
SQ1_HI      = $4003
SQ2_ENV     = $4004
SQ2_SWEEP   = $4005
SQ2_LO      = $4006
SQ2_HI      = $4007
TRI_CTRL    = $4008
TRI_LO      = $400A
TRI_HI      = $400B
NOISE_ENV   = $400C
APU_FLAGS   = $4015

