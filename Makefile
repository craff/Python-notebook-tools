
all:
	dune build

clean:
	dune clean
	rm tests/tic_tac_toe*.ipynb tests/tic_tac_toe*.py
	rm -rf tests/tmp

tests: all
	dune exec -- py2nb/py2nb.exe tests/tic_tac_toe.tpy
	dune exec -- nb2py/nb2py.exe tests/tic_tac_toe.ipynb tests/tic_tac_toe_corrige.ipynb tests/tic_tac_toe_test.ipynb
	python3 evaluation/evaluation.py tests/good.py tests/to_test
	if [ ! -d tests/tmp ]; then mkdir tests/tmp; fi
	cp tests/tic_tac_toe_corrige.py tests/tic_tac_toe.py tests/tmp
	python3 evaluation/evaluation.py tests/tic_tac_toe_test.py tests/tmp tests/tic_tac_toe.txt
