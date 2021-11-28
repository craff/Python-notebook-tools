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
        note = f1(v2)
    except:
        note = 0
        signal.alarm(0)
    return(note)

def compare_test(f1,f2,vector,timeout=1):
    total = len(vector)
    good = 0
    for (args,kargs) in vector:
        out = open("tmp.out",mode='w')
        try:
            sys.stdout = out
            signal.alarm(timeout)
            a1 = (1,f1(*args,*kargs))
            out.close()
            out1 = open("tmp.out").read()
            os.remove("tmp.out")
        except:
            out.close()
            os.remove("tmp.out")
            a1 = None
            out1 = None
        signal.alarm(0)
        out = open("tmp.out",mode='w')
        try:
            sys.stdout = out
            signal.alarm(timeout)
            a2 = (1,f2(*args,*kargs))
            sys.stdout = sys.__stdout__
            out.close()
            out2 = open("tmp.out").read()
            os.remove("tmp.out")
        except:
            sys.stdout = sys.__stdout__
            out.close()
            os.remove("tmp.out")
            a2 = None
            out2 = None
        signal.alarm(0)
        if a1 == a2 and out1 == out2: good += 1
    return(good/total)

evaluation_tests = {
    "value_tests":[],
    "compare_tests":[],
    "stdout_test":None
}

def new_value_test(test_name,test,vname):
    print(evaluation_tests)
    evaluation_tests["value_tests"].append({ "vname":vname, "test_name":test_name, "test":test })

def new_compare_test(fname,vector=[],timeout=1):
    print(evaluation_tests)
    ct = evaluation_tests["compare_tests"]
    d = None
    for x in ct:
        if x["fname"] == fname:
            d = x["fname"]
            break
    if d == None:
        d = { "fname":fname, "timeout":timeout, "vector":vector }
        ct.append(d)
    return(d)

def add_to_test_vector(vector,*args,**kargs):
    vector.append((args,kargs))

def add_compare_test(d,*args,**kargs):
    add_to_test_vector(d["vector"],*args,**kargs)

def set_stdout_test(test):
    evaluation_tests["stdout_test"] = test

def open_file(fname,stdin=None):
    f = { "new_value_test" :new_value_test,
          "new_compare_test": new_compare_test,
          "add_to_test_vector": add_to_test_vector,
          "add_compare_test": add_compare_test,
          "set_stdout_test": set_stdout_test }
    out = open("tmp.out",mode='w')
    sys.stdout = out
    if stdin != None:
         sys.stdin = StringIO(stdin)
    exec(open(fname).read(),f,f)
    sys.stdin = sys.__stdin__
    sys.stdout = sys.__stdout__
    out.close()
    out = open("tmp.out").read()
    os.remove("tmp.out")
    return (f,out)

def print_header(value_tests,compare_tests,sep=","):
    print("Nom, Prenom, Classe",end="")
    for d in value_tests:
        print(",",d["test_name"],end="")
    for d in compare_tests:
        print(",",d["fname"],end="")
    print(", Note")

def test_file(fprof,value_tests,compare_tests,stdout_test,file,stdout,stdin=None):
    nb_test = 0
    total = 0
    (f2,out2) = open_file(file,stdin)
    print(f2["eleve"]["Nom"],f2["eleve"]["Prenom"],f2["eleve"]["Classe"],
          sep=", ",end="")
    for d in value_tests:
        test = d["test"]
        vname = d["vname"]
        score = value_test(test,f2[vname])
        if type(score) == bool:
            score = 1 if score else 0
        nb_test += 1
        total += score
        print(",",score, end="")
    for d in compare_tests:
        fname = d["fname"]
        timeout = d["timeout"] if "timeout" in d else 1
        score = compare_test(fprof[fname],f2[fname],d["vector"],timeout=timeout)
        nb_test += 1
        total += score
        print(",",score, end="")
    if stdout_test != None:
        score = stdout_test(stdout,out2)
        if type(score) == bool:
            score = 1 if score else 0
        nb_test += 1
        total += score
    print(",", total/nb_test*20.0)


def test_all():
    correction = sys.argv[1]
    folder = sys.argv[2]
    stdin = open(sys.argv[3]).read() if len(sys.argv) > 3 else None
    (fprof,stdout) = open_file(correction,stdin)
    files = os.listdir(folder)
    tests = evaluation_tests
    value_tests = tests["value_tests"]
    compare_tests = tests["compare_tests"]
    stdout_test = tests["stdout_test"]
    print_header(value_tests,compare_tests)

    for file in files:
        if file.endswith(".py"): test_file(fprof,
                                           value_tests,compare_tests,stdout_test,
                                           folder + "/" + file,
                                           stdout,stdin)


test_all()
