
go-edit-el
==========

This is a program to graphically edit SGF files in emacs.  SGF is a
format to record various board games, most importantly (?) the game we
call Go, Igo, Weiqi, or Baduk, all of which mean something like *The
(Surrounding) Board Game*. (Etymologically the Japanese *Go* == board +
stone, while the chinese *Qi* == board + wood == "Gi" of ShoGi (if
you know that) , and *I" == *Wei* == to surround. Not sure about the
Korean name, please let me know if you know it.)

I wrote this long ago to practice tsumegos (or Go problems, tsumeru =
to pack,shorten,finalize, or something like that). But eventually I
threw it away, mostly due to lack of time, but partly because I
realized I would never reach the level of high dans. (I was 2d KGS at
the time.) The intended usage scenario is this: you grab a book of
tsumegos (classical ones are not copyrighted) from your local library
(if any), enter SGF files, and practice it.

Apparently the program works for emacs-24, so I decided to upload it.
I wish I knew GitHub. Hope this little program still helps someone.

Build & Basic Usage
---------------------

On your emacs shell,
	
	./configure
	make
	cd src
	M-x load-library RET test-suit
	M-x go-edit RET test.sgf

Arrow keys and  mouse wheels can be used to navigate the sgf tree.
Please see "src/go-edit.el" for the details of the interface. (I forgot)
