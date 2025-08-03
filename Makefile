##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## Makefile
##

NAME= 	imageCompressor

PAF	=	$(shell stack path --local-install-root)

all: 	$(NAME)

$(NAME):
	cd ImageCompressor
	stack build
	cp $(PAF)/bin/ImageCompressor-exe .
	mv ImageCompressor-exe $(NAME)

clean:
	rm -f *.o

fclean: clean
	rm -rf $(EXE)

re: 	fclean all

.PHONY: clean, fclean, re, $(NAME)
