

SPEC = Queue.hs
SRC= queue.c
OBJ= queue.o


Queue : $(OBJ) $(SPEC)
	ghc $(SPEC) $(OBJ)

$(OBJ) : $(SRC)
	gcc -fpic -c $(SRC)

