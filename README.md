Python notebook tools
=====================

CONVERSIONS
-----------

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

AUTO NOTATION
-------------

The folder ``evaluation`` contains a file ``evaluation.py``
that is a scrit to evaluate the work of students. It uses tests added in your
.tpy file with the math ``#TEST``.

To test your file there are three basic function

* You can test the stdout of running the whole python file using
  ```
  new_stdout_test(name,test)
  ```
  This test, like all other needs a name for printing the result, and returns
  a score between $0.0$ and $1.0$. The ``test(stdout)`` is run where ``stdout``
  is the output of the whole file being evaluated.

* You can test a value declared in the file with
  ```
  new_value_test(name,test,vname,timeout=1)
  ```
  where ``vname`` in the name of a value ``v`` declared in the file to be tested.
  as above, ``test(v)`` is run and returns a score. ``v`` maybe a function.

* You can compare the output of the teacher version with the following functions:
  ```
  d=new_compare_test(name,fname,vector=[],timeout=1)
  add_compare_test(d,*args,**kargs)
  add_to_test_vector(vector,*args,**kargs)
  ```
  The name of the function to be compared is ``fname``
  Typically, you create the compare test with the first function and
  add arguments (positional and names) with ``add_compare_test``.
  If several functions need to be tested with the same arguments,
  you can create a test vector using
  ``add_to_test_vector(vector,*args,**kargs)``
  and an initially empty sequence as vector. Then, you can use this vector
  in new_compare_test.

Here is a simple example:
```
## A simple example of python template
## ===================================

def add(x,y):
    """return the sum of x and y"""
    #CORRIGE
    return x+y
    #FIN
    #QUESTION
    pass # finish the function
    #FIN

#TEST
d=new_compare_test("T0",add)
for i in range(-10,10):
  for j in range(-10,10):
      add_compare_test(d,i,j)
#FIN
```
