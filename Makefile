##
## EPITECH PROJECT, 2022
## B-FUN-400-STG-4-1-wolfram-jeffrey.winkler
## File description:
## Makefile
##

NAME			=	imageCompressor
PATH_CMD		=	$(shell stack path --local-install-root)

all: $(NAME)

$(NAME): $(PATH_CMD)
	stack build
	cp "$(PATH_CMD)"/bin/imageCompressor-exe $(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: $(NAME)