
y = 0

def test_y(y):
    return y == 0

new_value_test("test_y",test_y,"y")

def f(x,y):
    return(x+y)

def f2(x):
    return(f(x,x))

z = f2(4)

def test_f(f):
    if f(2,2) == 4 and f(3,3) == 6:
        return 1
    else: return 0

new_value_test("test_f",test_f,"f")

t = new_compare_test("f","f")
add_compare_test(t,-1,-1)
add_compare_test(t,0,0)
add_compare_test(t,0,1)
add_compare_test(t,2,2)
add_compare_test(t,1,2)

def g(x):
    return(x)

t = new_compare_test("g","g")
add_compare_test(t,-1)
add_compare_test(t,0)
add_compare_test(t,1)
add_compare_test(t,2)

def h(x):
    return x


t = new_compare_test("h","h")
add_compare_test(t,-1)
add_compare_test(t,0)
add_compare_test(t,1)
add_compare_test(t,2)
