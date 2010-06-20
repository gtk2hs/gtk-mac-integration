
PROGS  = osxdemo
SOURCES = OSXDemo.hs

all : $(PROGS)

osxdemo : OSXDemo.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROG)

HC=ghc
