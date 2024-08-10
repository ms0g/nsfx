TARGET=demo.nes
CFG=cfg/nes.cfg
OBJS=demo.o engine/nsfx.o
ASM=ca65
LD=ld65
LDFLAGS=-C $(CFG) 

%.o : %.s
	$(ASM) $<

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $@ 

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJS) *.fdb