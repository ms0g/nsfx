TARGET=nsfx.nes
CFG=cfg/nes.cfg
OBJS=src/main.o
ASM=ca65
LD=ld65
LDFLAGS=-C $(CFG) 

src/%.o : src/%.asm 
	$(ASM) $<
$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $@ 

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJS)