##
## EPITECH PROJECT, 2024
## MyPandoc
## File description:
## Makefile
##

NAME		:=	mypandoc
BUILD_DIR	:=	$(shell stack path --local-install-root)


all:
	@stack build
	@ln -sf $(BUILD_DIR)/bin/MyPandoc-exe $(NAME)

test:all
	@stack test

clean:
	@stack clean

fclean:	clean
	@rm -f $(NAME)

re:		fclean all
