import signal
import os
from io import StringIO
import sys
import tempfile

def stop(sig,frame):
    raise MemoryError

def pf(x):
    return "{:.2f}".format(x)

signal.signal(signal.SIGALRM, stop)

def set_alarm(t):
    signal.setitimer(signal.ITIMER_REAL, t)
def rm_alarm():
    signal.setitimer(signal.ITIMER_REAL, 0)

def value_test(f1,*v2,timeout=0.2):
    try:
        set_alarm(timeout)
        note = f1(*v2)
        rm_alarm()
    except:
        rm_alarm()
        note = 0
    return(note)

def compare_test(f1,f2,vector,timeout=0.2):
    total = len(vector)
    good = 0
    for (args,kargs) in vector:
        tmp_name="/tmp/"+next(tempfile._get_candidate_names())+"_pynbeval.txt"
        out = open(tmp_name,mode='w')
        try:
            sys.stdout = out
            set_alarm(timeout)
            a1 = (1,f1(*args,*kargs))
            rm_alarm()
            out.close()
            out1 = open(tmp_name).read()
            os.remove(tmp_name)
        except:
            rm_alarm()
            out.close()
            os.remove(tmp_name)
            a1 = None
            out1 = None
        tmp_name="/tmp/"+next(tempfile._get_candidate_names())+"_pynbeval.txt"
        out = open(tmp_name,mode='w')
        try:
            sys.stdout = out
            set_alarm(timeout)
            a2 = (1,f2(*args,*kargs))
            rm_alarm()
            sys.stdout = sys.__stdout__
            out.close()
            out2 = open(tmp_name).read()
            os.remove(tmp_name)
        except:
            rm_alarm()
            sys.stdout = sys.__stdout__
            out.close()
            os.remove(tmp_name)
            a2 = None
            out2 = None
        if a1 == a2 and out1 == out2: good += 1
    return(good/total)

evaluation_tests = []

def new_value_test(test_name,test,*vname,timeout=0.2):
    evaluation_tests.append({ "type":"value", "vname":vname,
                              "test_name":test_name, "test":test,
                              "timeout":timeout})

def new_compare_test(test_name,fname,vector=[],timeout=0.2):
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

def get_seed(fname):
    first_line = open(fname).readline()
    if first_line[0:6] == "#SEED:":
        return(int(first_line[6:]))

def open_file(fname,stdin=None,timeout=0.5,no_catch=False,seed=0):
    f = { "new_value_test": new_value_test,
          "new_compare_test": new_compare_test,
          "add_to_test_vector": add_to_test_vector,
          "add_compare_test": add_compare_test,
          "new_stdout_test": new_stdout_test,
          "seed": seed}
    tmp_name="/tmp/" + next(tempfile._get_candidate_names())+"_pynbeval.txt"
    out = open(tmp_name,mode='w')
    sys.stdout = out
    if stdin != None:
         sys.stdin = StringIO(stdin)
    if no_catch:
        ok = True
        exec(open(fname).read(),f,f)
    else:
        try:
            set_alarm(timeout)
            exec(open(fname).read(),f,f)
            ok = True
            rm_alarm()
        except:
            ok = False
            rm_alarm()
    sys.stdin = sys.__stdin__
    sys.stdout = sys.__stdout__
    out.close()
    out = open(tmp_name).read()
    os.remove(tmp_name)
    return (f,out) if ok else None

def print_header(tests,sep=","):
    sep=""
    for d in tests:
        print(sep,d["test_name"],end="")
        sep=","
    print(", Note")

def test_file(fprof,multi,tests,file,stdout,stdin=None,seed=0):
    nb_test = 0
    total = 0
    res = open_file(file,stdin,timeout=10.0,seed=seed)
    if res == None:
        print("Error")
        return
    (f2,out2) = res
    sep = ""
    for d in tests:

        if d["type"] == "value":
            test = d["test"]
            vname = d["vname"]
            timeout = d["timeout"] if "timeout" in d else 1
            vals = [f2[name] if name in f2 else None for name in vname]
            score = value_test(test,*vals,timeout=timeout)
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
        if multi: print(sep,pf(score), end="")
        else: print(d["test_name"],":",pf(score))
        sep=","
    r = total/nb_test*20.0 if nb_test > 0 else 0.0
    if multi: print(sep, pf(r))
    else: print("Total:", pf(r))

def test_all():
    correction = sys.argv[1]
    file = sys.argv[2]
    stdin = open(sys.argv[3]).read() if len(sys.argv) > 3 and len(sys.argv[3]) > 0 else None
    seed = get_seed(file)
    (fprof,stdout) = open_file(correction,stdin,no_catch=True,timeout=10.0,seed=seed)
    tests = evaluation_tests
    test_file(fprof,False,tests,file,stdout,stdin,seed=seed)

test_all()
