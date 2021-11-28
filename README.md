Python notebook tools
=====================

Some tools to convert generate notebook (.ipynb) from python templates
(.tpy) or python files (.py) and python file (.py) from notebook (.ipynb)

py2nb files : converts python file or templates to notebooks.
the convention is that line starting with ``##`` are markdown
cells. When such lines are consecutive, they are in the same cell.

A python templates (file.tpy) may contains marked code section like
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
or
```
	#TEST
	...
	#FIN
```

In this case three notebooks are produced: ``file.ipynb``, ``file_corrige.ipynb``
and ``file_test.ipynb``. Annotation with ``#QUESTION`` are only copied in
``file.ipynb``, annotation with ``#CORRIGE`` are copied in  ``file_corrige.ipynb``
and ``file_test.ipynb`` and annotation with ``#TEST`` are only copied in
``file_test.ipynb``.

An annotation
```
	#COMMMENT
	...
	#FIN
```
means that the annotated lines are commented in ``file.ipynb``


```
usage: py2nb/py2nb.exe files ...
  -q only produces questions for templates
  -c only produces corrections for templates
  -t only produces tests for templates
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
The folder ``evaluation`` contains a file ``evaluation.py``
that is a scrit to evaluate the work of students...
