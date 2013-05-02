# CS 3110 PS6 Makefile
# usage:
#    make game                   -  equiv. to build_game.*
#    make team name=<team-name>  -  equiv. to build_team.* foo
#    make all name=<team-name>   -  equiv. to build_game followed by build_team
#    make clean                  -  clean the project

.SILENT:

all:
ifeq ($(name),)
	echo 'usage: make [all] name=<team-name>'
	echo 'or     make game'
	echo 'or     make team name=<team-name>'
	echo 'or     make clean'
else
	make game
	make team
endif

.PHONY: game
game:
	ocamlc -o game/game.exe -I +threads -I shared -I game unix.cma threads.cma \
    str.cma shared/thread_pool.mli shared/thread_pool.ml shared/connection.mli \
    shared/connection.ml shared/constants.ml shared/definitions.ml \
    shared/util.ml game/netgraphics.mli game/netgraphics.ml game/state.mli \
    game/state.ml game/game.mli game/game.ml game/server.ml  

.PHONY: team
team:
ifeq ($(name),)
	echo usage: make team name=<team-name>
else
	ocamlc -o team/$(name).exe -I +threads -I game -I shared -I team unix.cma \
    threads.cma str.cma shared/thread_pool.mli shared/thread_pool.ml \
    shared/connection.mli shared/connection.ml shared/constants.ml \
    shared/definitions.ml shared/util.ml team/team.ml team/$(name).ml \
    game/state.ml game/state.mli 
endif

.PHONY: clean
clean:
	rm game/*.cmi game/*.cmo game/game.exe shared/*.cmi shared/*.cmo \
    team/*.exe team/*.cmi team/*.cmo
