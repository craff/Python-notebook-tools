
DUNE = $(shell opam var bin)/dune
PREFIX = /usr/local

all:
	dune build

clean:
	dune clean
	- rm tests/tic_tac_toe*.ipynb tests/tic_tac_toe*.py
	- rm tests/polynome2*.ipynb tests/polynome2*.py
	- rm -rf tests/tmp tests/tmp2

tests: all
	dune exec -- py2nb/py2nb.exe tests/tic_tac_toe.tpy
	dune exec -- nb2py/nb2py.exe tests/tic_tac_toe.ipynb tests/tic_tac_toe_corrige.ipynb tests/tic_tac_toe_test.ipynb
	if [ ! -d tests/tmp ]; then mkdir tests/tmp; fi
	cp tests/tic_tac_toe_corrige.py tests/tic_tac_toe.py tests/tmp
	python3 evaluation/evaluation.py tests/tic_tac_toe_test.py tests/tmp tests/tic_tac_toe.txt
	dune exec -- py2nb/py2nb.exe tests/polynome2.tpy
	dune exec -- nb2py/nb2py.exe tests/polynome2.ipynb tests/polynome2_corrige.ipynb tests/polynome2_test.ipynb
	if [ ! -d tests/tmp2 ]; then mkdir tests/tmp2; fi
	cp tests/polynome2_corrige.py tests/polynome2.py tests/tmp2
	python3 evaluation/evaluation.py tests/polynome2_test.py tests/tmp2
	python3 evaluation/evaluation.py tests/good.py tests/to_test

install: all
	sudo ${DUNE} install --prefix=${PREFIX}
	sudo install -d ${PREFIX}/lib/pynotebooktools/
	sudo install evaluation/evaluation.py ${PREFIX}/lib/pynbtools/
