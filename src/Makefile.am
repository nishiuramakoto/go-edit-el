

myelib	=  mylib.el my-matrix.el my-tree.el  my-util.el 
goedit   =  gtp.el go-game.el  sgf.el goban.el go-edit.el go-clock.el

dist_lisp_LISP = $(myelib) $(goedit) 

dist_lisp_DATA=  gnugo-xpms.el gnugo-xpms-small.el test-tsumego.el
dist_data_DATA   = compile-getopt.in
dist_bin_SCRIPTS = compile-getopt
CLEANFILES= $(dist_bin_SCRIPTS)

compile-getopt: compile-getopt.in Makefile
	sed -e 's,[@]lispdir[@],$(lispdir),g' $(srcdir)/compile-getopt.in > compile-getopt
	chmod +x compile-getopt
