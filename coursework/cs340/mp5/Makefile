CC=g++

all: mp5

mp5: mp5.o Card.o CardPile.o DrawPile.o Game.o LaydownPile.o PlayerHand.o Round.o
	$(CC) -o mp5 mp5.o Card.o CardPile.o DrawPile.o Game.o LaydownPile.o PlayerHand.o Round.o

mp5.o: mp5.h mp5.cpp
	$(CC) -o mp5.o -c mp5.cpp

Card.o: mp5.h Card.cpp
	$(CC) -o Card.o -c Card.cpp

CardPile.o: mp5.h CardPile.cpp
	$(CC) -o CardPile.o -c CardPile.cpp

DrawPile.o: mp5.h DrawPile.cpp
	$(CC) -o DrawPile.o -c DrawPile.cpp

Game.o: mp5.h Game.cpp
	$(CC) -o Game.o -c Game.cpp

LaydownPile.o: mp5.h LaydownPile.cpp
	$(CC) -o LaydownPile.o -c LaydownPile.cpp

PlayerHand.o: mp5.h PlayerHand.cpp
	$(CC) -o PlayerHand.o -c PlayerHand.cpp

Round.o: mp5.h Round.cpp
	$(CC) -o Round.o -c Round.cpp

clean:
	rm -f *.o core
