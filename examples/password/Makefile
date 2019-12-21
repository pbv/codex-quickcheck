
SPEC = Password.hs
CC= gcc
CFLAGS = -fPIC
GHC = ghc

%.o: %.c 
	$(CC) -c -o $@ $< $(CFLAGS)

example% : solution%.o $(SPEC) 
	$(GHC) -o $@ $(SPEC) $<

all : example1 example2
