.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

## to test lib_chan
##    make server in one window
##    make client in the other window

MODS =  message_client message_server


ERL = erl -boot start_clean

all:	compile
	@echo "To run the chat test program"

compile: ${MODS:%=%.beam}
	mkdir -p ${HOME}/.erlang_config/
	@echo "make clean - clean up"


client: compile
	erl -sname chatter -setcookie abc -s message_client test

client2: compile
	erl -sname chatter2 -setcookie abc -s message_client test2

server: compile
	erl -sname messenger -setcookie abc -s message_server start

