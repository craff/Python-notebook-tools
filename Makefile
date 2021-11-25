
all:
	dune build

clean:
	dune clean
	rm tests/*.ipynb tests/*.py

tests: all
	dune exec -- py2nb/py2nb.exe tests/tic_tac_toe.tpy
	dune exec -- nb2py/nb2py.exe tests/tic_tac_toe.ipynb tests/tic_tac_toe_corrige.ipynb
