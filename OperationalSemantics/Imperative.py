### IMP ###
### Arithmetic Expressions ###
class Var:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

class Int:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return str(self.value)
       
class AExpr:
    pass

class AExprVar(AExpr):
    def __init__(self, var):
        self.var = var

    def __repr__(self):
        return str(self.var.name)

    def eval(self, store):
        new_expr = AExprInt(Int(store[self.var.name]))
        new_store = store
        return new_store, new_expr
        
class AExprInt(AExpr):
    def __init__(self, integer):
        self.integer = integer

    def __repr__(self):
        return str(self.integer.value)
        
    def eval(self, store):
        new_store = store
        new_expr = self
        return new_store, new_expr
        
class AExprAdd(AExpr):
    def __init__(self, e1, e2):
        self.e1 = e1
        self.e2 = e2

    def __repr__(self):
        return "(" + repr(self.e1) + " + " + repr(self.e2) + ")"
        
    def eval(self, store):
        if isinstance(self.e1, AExprInt) and isinstance(self.e2, AExprInt):
            p = self.e1.integer.value + self.e2.integer.value
            return store, AExprInt(Int(p))
        elif isinstance(self.e1, AExprInt):
            new_store, new_e2 = self.e2.eval(store)
            return new_store, AExprAdd(self.e1, new_e2)
        else:
            new_store, new_e1 = self.e1.eval(store)
            return new_store, AExprAdd(new_e1, self.e2)

class AExprMul(AExpr):
    def __init__(self, e1, e2):
        self.e1 = e1
        self.e2 = e2

    def __repr__(self):
        return "(" + repr(self.e1) + " * " + repr(self.e2) + ")"

    def eval(self, store):
        if isinstance(self.e1, AExprInt) and isinstance(self.e2, AExprInt):
            p = self.e1.integer.value * self.e2.integer.value
            return store, AExprInt(Int(p))
        elif isinstance(self.e1, AExprInt):
            new_store, new_e2 = self.e2.eval(store)
            return new_store, AExprMul(self.e1, new_e2)
        else:
            new_store, new_e1 = self.e1.eval(store)
            return new_store, AExprMul(new_e1, self.e2)

### Boolean Expressions ###
class Bool:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return str(self.value)

class BExpr:
    pass

class BExprBool(BExpr):
    def __init__(self, boolean):
        self.boolean = boolean

    def __repr__(self):
        return str(self.boolean.value)

    def eval(self, store):
        new_store = store
        new_expr = self
        return new_store, new_expr

class BExprLessThan(BExpr):
    def __init__(self, a1, a2):
        self.a1 = a1
        self.a2 = a2

    def __repr__(self):
        return "(" + repr(self.a1) + " < " + repr(self.a2) + ")"

    def eval(self, store):
        if isinstance(self.a1, AExprInt) and isinstance(self.a2, AExprInt):
            new_expr = BExprBool(Bool(self.a1.integer.value < self.a2.integer.value))
            return store, new_expr
        if isinstance(self.a1, AExprInt):
            new_store, new_a2 = self.a2.eval(store)
            return new_store, BExprLessThan(self.a1, new_a2)
        else:
            new_store, new_a1 = self.a1.eval(store)
            return new_store, BExprLessThan(new_a1, self.a2)

### Command Expressions ###
class CExpr:
    pass

class Skip():
    def __init__(self):
        self.value = "SKIP"

    def __repr__(self):
        return str(self.value)

class CExprSkip(CExpr):
    def __init__(self, skip):
        self.skip = skip

    def __repr__(self):
        return str(self.skip.value)

    def eval(self, store):
        new_store = store
        new_expr = self
        return new_store, new_expr

class CExprAssgn(CExpr):
    def __init__(self, var, a):
        self.var = var
        self.a = a

    def __repr__(self):
        return repr(self.var) + " := " + repr(self.a)
    
    def eval(self, store):
        if isinstance(self.a, AExprInt):
            _, self.a = self.a.eval(store)
            store[self.var.var.name] = self.a.integer.value
            return store, CExprSkip(Skip())
        else:
            new_store, new_a = self.a.eval(store)
            return new_store, CExprAssgn(self.var, new_a)

class CExprSequence(CExpr):
    def __init__(self, c1, c2):
        self.c1 = c1
        self.c2 = c2

    def __repr__(self):
        return repr(self.c1) + "; " + repr(self.c2)

    def eval(self, store):
        if isinstance(self.c1, CExprSkip):
            new_store = store
            return new_store, self.c2
        else:
            new_store, new_c1 = self.c1.eval(store)
            return new_store, CExprSequence(new_c1, self.c2)

class CExprIf(CExpr):
    def __init__(self, b, c1, c2):
        self.b = b
        self.c1 = c1
        self.c2 = c2

    def __repr__(self):
        return "if " + repr(self.b) + " then " + repr(self.c1) + " else " + repr(self.c2)

    def eval(self, store):
        if not isinstance(self.b, BExprBool):
            new_store, new_b = self.b.eval(store)
            return new_store, CExprIf(new_b, self.c1, self.c2)
        elif self.b.boolean.value:
            new_store, new_c1 = self.c1.eval(store)
            return new_store, new_c1
        else:
            new_store, new_c2 = self.c2.eval(store)
            return new_store, new_c2

class CExprWhile(CExpr):
    def __init__(self, b, c):
        self.unevaluated_b = b
        self.b = b
        self.c = c

    def __repr__(self):
        return "while " + repr(self.unevaluated_b) + " do " + repr(self.c)

    def eval(self, store):
        self.unevaluated_b = self.b
        # we only end up evaluating a less_than condition a maximum of three times recursively before arriving at a boolean expression. If we eval a boolean expression,
        # that's fine. It just returns itself. If we do decide to increase recursion depth of less_than expression or some non-evaluated bool, 
        # increase num of times eval is called on self.b.  
        new_store, self.b = self.b.eval(store)
        new_store, self.b = self.b.eval(store)
        new_store, self.b = self.b.eval(store)
        if self.b.boolean.value:
            new_store = store
            new_expr = CExprIf(self.unevaluated_b, CExprSequence(self.c, CExprWhile(self.unevaluated_b, self.c)), CExprSkip(Skip()))
            return new_store, new_expr
        return store, CExprSkip(Skip())

### Eval ###
def eval_once(store, expr):
    #step our program only once. 
    store, expr = expr.eval(store)
    return store, expr

def eval_multiple(store, expr):
    while True:
        new_store, new_expr = expr.eval(store)
        # if our current expression and our new expressions were both SKIP, then we know we are done and can return.
        if isinstance(new_expr, CExprSkip) and isinstance(expr, CExprSkip):
            return new_store, new_expr
        store, expr = new_store, new_expr

### Programs ###
### Program 0: IF HARDCODED BOOLEAN WITH TWO ASSIGNMENT CLAUSES ###
# y = 6
# if False:
#   y = y * 5
# else:
#   y = 3
s0 = {}
ea = CExprAssgn(AExprVar(Var("y")), AExprInt(Int(6)))
eb = CExprIf(BExprBool(Bool(False)), 
             CExprAssgn(AExprVar(Var("y")), AExprMul(AExprVar(Var("y")), AExprInt(Int(5)))), 
             CExprAssgn(AExprVar(Var("y")), AExprInt(Int(3))))

#s0, e0 = eval_once(s0, ea)
#s1, e1 = eval_multiple(s0, eb)
#print(s1)

print("Program being run:")
s0, e0 = eval_once(s0, ea)
print(ea)
s1, e1 = eval_once(s0, eb)
print(eb)
s2, e2 = eval_multiple(s1, e1)
print(f"Resulting program store: {s2}\n")

### Program 1: IF CONDITION WITH TWO ASSIGNMENTS ###
# y = 6
# if 1 < y:
#   y = y * 5
# else:
#   y = 3
s0 = {}
ea = CExprAssgn(AExprVar(Var("y")), AExprInt(Int(200)))
eb = CExprIf(BExprLessThan(AExprInt(Int(1)), AExprVar(Var("y"))), 
             CExprAssgn(AExprVar(Var("y")), AExprMul(AExprVar(Var("y")), AExprInt(Int(5)))), 
             CExprAssgn(AExprVar(Var("x")), AExprInt(Int(3))))

#s0, e0 = eval_once(s0, ea)
#s1, e1 = eval_multiple(s0, eb)
#print(s1)

print("Program being run:")
s0, e0 = eval_once(s0, ea)
print(ea)
s1, e1 = eval_once(s0, eb)
print(eb)
s2, e2 = eval_multiple(s1, e1)
print(f"Resulting program store: {s2}\n")

### Program 2: Assign to y (5*2) + 3 ###
# y = 2
# y = (y * 2) + 3
s0 = {}
ea = CExprAssgn(AExprVar(Var("y")), AExprInt(Int(2)))
eb = CExprAssgn(AExprVar(Var("y")), AExprAdd(AExprMul(AExprVar(Var('y')), AExprInt(Int(2))), AExprInt(Int(3))))

print("Program being run:")
s0, e0 = eval_once(s0, ea)
print(ea)
s1, e1 = eval_once(s0, eb)
print(eb)
s2, e2 = eval_multiple(s1, e1)
print(f"Resulting program store: {s2}\n")

### Program 3: SEQUENCE OF TWO ASSIGNMENTS ###
# y = 5; x = 5;
s0 = {}
ea = CExprSequence(CExprAssgn(AExprVar(Var("y")), AExprInt(Int(5))), 
                   CExprAssgn(AExprVar(Var("x")), AExprInt(Int(5))))

print("Program being run:")
s0, e0 = eval_once(s0, ea)
print(ea)
s1, e1 = eval_multiple(s0, e0)
print(f"Resulting program store: {s1}\n")

### Program 4: WHILE LOOP WITH A COMPARISON, ADDITION, ASSIGNMENT ###
# y = 0
# while y < 4:
#   y = y + 1
s0 = {}
ea = CExprAssgn(AExprVar(Var("y")), AExprInt(Int(0)))
eb = CExprWhile(BExprLessThan(AExprVar(Var("y")), AExprInt(Int(29))), 
                CExprAssgn(AExprVar(Var("y")), AExprAdd(AExprVar(Var("y")), AExprInt(Int(1)))))

print("Program being run:")
s0, e0 = eval_once(s0, ea)
print(ea)
s1, e1 = eval_once(s0, eb)
print(eb)
s2, e2 = eval_multiple(s1, e1)
print(f"Resulting program store: {s2}\n")

### Program 5: Sequence of Assignment involving multiplication ###
# y = 5; x = y * 13;
s0 = {}
ea = CExprSequence(CExprAssgn(AExprVar(Var("y")), AExprInt(Int(5))), 
                   CExprAssgn(AExprVar(Var("x")), AExprMul(AExprVar(Var("y")), AExprInt(Int(13)))))

print("Program being run:")
s0, e0 = eval_once(s0, ea)
print(ea)
s1, e1 = eval_multiple(s0, e0)
print(f"Resulting program store: {s1}\n")
