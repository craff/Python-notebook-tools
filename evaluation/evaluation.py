import signal
import os
from io import StringIO
import sys
import tempfile

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
    tmp_name="/tmp/"+next(tempfile._get_candidate_names())
    for (args,kargs) in vector:
        out = open(tmp_name,mode='w')
        try:
            sys.stdout = out
            signal.alarm(timeout)
            a1 = (1,f1(*args,*kargs))
            out.close()
            out1 = open(tmp_name).read()
            os.remove(tmp_name)
        except:
            out.close()
            os.remove(tmp_name)
            a1 = None
            out1 = None
        signal.alarm(0)
        out = open(tmp_name,mode='w')
        try:
            sys.stdout = out
            signal.alarm(timeout)
            a2 = (1,f2(*args,*kargs))
            sys.stdout = sys.__stdout__
            out.close()
            out2 = open(tmp_name).read()
            os.remove(tmp_name)
        except:
            sys.stdout = sys.__stdout__
            out.close()
            os.remove(tmp_name)
            a2 = None
            out2 = None
        signal.alarm(0)
        if a1 == a2 and out1 == out2: good += 1
    return(good/total)

evaluation_tests = []

def new_value_test(test_name,test,vname,timeout=1):
    evaluation_tests.append({ "type":"value", "vname":vname,
                              "test_name":test_name, "test":test,
                              "timeout":timeout})

def new_compare_test(test_name,fname,vector=[],timeout=1):
    ct = evaluation_tests
    d = None
    for x in ct:
        if x["type"]=="compare" and x["fname"] == fname:
            d = x["fname"]
            break
    if d == None:
        d = { "type":"compare", "test_name":test_name,"fname":fname, "timeout":timeout, "vector":vector }
        ct.append(d)
    return(d)

def add_to_test_vector(vector,*args,**kargs):
    vector.append((args,kargs))

def add_compare_test(d,*args,**kargs):
    add_to_test_vector(d["vector"],*args,**kargs)

def new_stdout_test(name,test):
    evaluation_tests.append({"type": "stdout", "test_name":name, "stdout_test":test})

def open_file(fname,stdin=None,timeout=1,no_catch=False):
    f = { "new_value_test": new_value_test,
          "new_compare_test": new_compare_test,
          "add_to_test_vector": add_to_test_vector,
          "add_compare_test": add_compare_test,
          "new_stdout_test": new_stdout_test }
    tmp_name="/tmp/" + next(tempfile._get_candidate_names())
    out = open(tmp_name,mode='w')
    sys.stdout = out
    if stdin != None:
         sys.stdin = StringIO(stdin)
    if no_catch: exec(open(fname).read(),f,f)
    else:
        try:
            signal.alarm(timeout)
            exec(open(fname).read(),f,f)
        except:
            pass
    signal.alarm(0)
    sys.stdin = sys.__stdin__
    sys.stdout = sys.__stdout__
    out.close()
    out = open(tmp_name).read()
    os.remove(tmp_name)
    return (f,out)

def print_header(tests,sep=","):
    sep=""
    for d in tests:
        print(sep,d["test_name"],end="")
        sep=","
    print(", Note")

def test_file(fprof,multi,tests,file,stdout,stdin=None):
    nb_test = 0
    total = 0
    (f2,out2) = open_file(file,stdin)
    sep = ""
    for d in tests:

        if d["type"] == "value":
            test = d["test"]
            vname = d["vname"]
            timeout = d["timeout"] if "timeout" in d else 1
            score = value_test(test,f2[vname],timeout) if vname in f2 else 0
            if type(score) == bool:
                score = 1 if score else 0
            nb_test += 1
            total += score
        elif d["type"] == "compare":
            fname = d["fname"]
            timeout = d["timeout"] if "timeout" in d else 1
            score = compare_test(fprof[fname],f2[fname],d["vector"],timeout=timeout) if fname in f2 else 0
            nb_test += 1
            total += score
        elif d["type"] == "stdout":
            score = stdout_test(stdout,out2)
            if type(score) == bool:
                score = 1 if score else 0
            nb_test += 1
            total += score
        if multi: print(sep,score, end="")
        else: print(d["test_name"],":",score)
        sep=","
    if multi: print(sep, total/nb_test*20.0)
    else: print("Total:", total/nb_test*20.0)

def test_all():
    correction = sys.argv[1]
    folder = sys.argv[2]
    stdin = open(sys.argv[3]).read() if len(sys.argv) > 3 else None
    (fprof,stdout) = open_file(correction,stdin,no_catch=True)
    tests = evaluation_tests
    if os.path.isdir(folder):
        print_header(tests)
        files = os.listdir(folder)
        for file in files:
            if file.endswith(".py"): test_file(fprof,True,
                                               tests,
                                               folder + "/" + file,
                                               stdout,stdin)
    else:
        file = folder
        if file.endswith(".py"): test_file(fprof,False,
                                           tests,
                                           file,
                                           stdout,stdin)

test_all()
