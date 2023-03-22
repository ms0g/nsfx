TARGET=nsfx.nes
CFG=cfg/nes.cfg
OBJS=src/main.o
CA65=ca65
LD=ld65
LDFLAGS=-C $(CFG) 

src/%.o : src/%.asm 
	$(CA65) $<
$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $@ 

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJS)