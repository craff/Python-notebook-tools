import signal
import os

def stop():
    raise SystemExit

signal.signal(signal.SIGALRM, stop)

def test(f1,f2,vector,timeout=1):
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

def open_file(fname):
    f = {}
    exec(open(fname).read(),globals(),f)
    return f

fprof = open_file("good.py")

tests = [
    { "fname": "f", "vector": [([-1,-1],{}),
                               ([0,0],{}),
                               ([0,1],{}),
                               ([2,2],{}),
                               ([1,2],{})]},
    { "fname": "g", "vector": [([-1],{}),
                               ([0],{}),
                               ([1],{})]},
    { "fname": "h", "vector": [([-1],{}),
                               ([0],{}),
                               ([1],{})]}
]

def print_header(tests,sep=","):
    print("Nom, Prenom, Classe",end="")
    for d in tests:
        print(",",d["fname"],end="")
    print()

def test_file(file):
    f2 = open_file(file)
    print(f2["eleve"]["Nom"],f2["eleve"]["Prenom"],f2["eleve"]["Classe"],
          sep=", ",end="")
    for d in tests:
        fname = d["fname"]
        print(",",test(fprof[fname],f2[fname],d["vector"]), end="")
    print()

files = os.listdir("to_test")

print_header(tests)
for file in files:
    if file.endswith(".py"): test_file("to_test/" + file)
