
y = 0

def test_y(y):
    y == 0

def f(x,y):
    return(x+y)

def test_f(f):
    if f(2,2) == 4 and f(3,3) == 6:
        return 1
    else: return 0

def g(x):
    return(x)

def h(x):
    return x

tests = { "value_tests": [
              { "vname": "y", "test_name": "test_y" },
              { "vname": "f", "test_name": "test_f" }
          ],
          "compare_tests":
          [
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
         }
