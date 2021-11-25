import signal
import os
from io import StringIO
import sys

def stop():
    raise SystemExit

signal.signal(signal.SIGALRM, stop)

def value_test(f1,v2,timeout=1):
    try:
        signal.alarm(timeout)
        note = f1(v2,*args,*kargs)
    except:
        note = 0
        signal.alarm(0)
    return(note)

def compare_test(f1,f2,vector,timeout=1):
    total = len(vector)
    good = 0
    for (args,kargs) in vector:
        a1 = None
        try:
            signal.alarm(timeout)
            a1 = (1,f1(*args,*kargs))
        except:
            a1 = (0, None)
        signal.alarm(0)
        a2 = None
        try:
            signal.alarm(timeout)
            a2 = (1,f2(*args,*kargs))
        except:
            a2 = (0, None)
        signal.alarm(0)
        if a1 == a2: good += 1
    return(good/total)

def open_file(fname,stdin=None):
    f = {}
    if stdin != None:
        sys.stdin = StringIO(stdin)
    exec(open(fname).read(),globals(),f)
    sys.stdin = sys.__stdin__
    return f

def print_header(value_tests,compare_tests,sep=","):
    print("Nom, Prenom, Classe",end="")
    for d in value_tests:
        print(",",d["test_name"],end="")
    for d in compare_tests:
        print(",",d["fname"],end=", ")
    print("Note")

def test_file(fprof,value_tests,compare_tests,file,stdin=None):
    nb_test = 0
    total = 0
    f2 = open_file(file,stdin)
    print(f2["eleve"]["Nom"],f2["eleve"]["Prenom"],f2["eleve"]["Classe"],
          sep=", ",end="")
    for d in value_tests:
        test_name = d["test_name"]
        vname = d["vname"]
        score = value_test(fprof[test_name],f2[vname])
        nb_test += 1
        total += score
        print(",",score, end="")
    for d in compare_tests:
        fname = d["fname"]
        score = compare_test(fprof[fname],f2[fname],d["vector"])
        nb_test += 1
        total += score
        print(",",score, end=", ")
    print(total/nb_test*20.0)

def test_all():
    correction = sys.argv[1]
    folder = sys.argv[2]
    stdin = sys.argv[3] if len(sys.argv) > 3 else None
    fprof = open_file(correction,stdin)
    files = os.listdir(folder)
    tests = fprof["tests"]
    value_tests = tests["value_tests"]
    compare_tests = tests["compare_tests"]
    print_header(value_tests,compare_tests)

    for file in files:
        if file.endswith(".py"): test_file(fprof,value_tests,compare_tests,
                                           folder + "/" + file,
                                           stdin)


test_all()
