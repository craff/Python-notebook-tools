Python notebook tools
=====================

Some tools to convert generate notebook (.ipynb) from python templates
(.tpy) or python files (.py) and python file (.py) from notebook (.ipynb)

py2nb files : converts python file or templates to notebooks.
the convention is that line starting with ``##`` are markdown
cells. When such lines are consecutive, they are in the same cell.

A python templates (file.tpy) may contains marks like
```
	#QUESTION
	...
	#FIN
```
or
```
	#CORRIGE
	...
	#FIN
```
In this case two notebooks are produced: file.ipynb and file_corrige.ipynb,
the former not containing the line marked with ``#CORRIGE``, the latter
not containing the line marker with ``#QUESTION``.

```
usage: py2nb/py2nb.exe files ...
  -q only produces question for templates
  -c only produces correction for templates
  -help  Display this list of options
  --help  Display this list of options
```

nb2py file : converts notebook back to python with the same convention as above.

```
usage: nb2py/nb2py.exe files ...
  -r forget markdown cells
  -help  Display this list of options
  --help  Display this list of options
```

Caveats: py2nb followed by nb2py may remove or add some empty lines that
separates cells. Basically, we enforce one empty line between cells.

WORK IN PROGRESS:
The folder tests contains a file notation.py that is a good starting example if
you want to evaluate the workd of some students.
