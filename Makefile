TARGET=demo.nes
CFG=cfg/nes.cfg
OBJS=src/demo.o src/nsfx.o
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