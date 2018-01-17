from functools import lru_cache

# Cilj: izračunajte vrednosti Fibonaccijevega zaporadja za 100, 500, 1000,
# 10**5, and 10**6 člen.
# Za vsako definicijo preizkusite kako pozne člene lahko izračuante in poglejte
# zakaj se pojavi problem (neučinkovitost, pregloboka rekurzija,
# premalo spomina ...).

# Definirajte naivno rekurzivno različico.
# Omejitev: Prepočasno.
def fib(n):
    if n == 1 or n == 0:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

# Z uporabo dekoratorja izboljšajte naivno različico.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~350.

@lru_cache()
def fib_cache(n):
    if n == 1 or n == 0:
        return n
    else:
        return fib_cache(n - 1) + fib_cache(n - 2)

# Nariši drevo klicov za navadno rekurzivno fib funkcijo pri n=5 in
# ugotovi kateri podproblemi so klicani večkrat.

# Definirajte rekurzivno memoizirano funkcijo fib brez uporabe dekoratorja.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~1000.

#faks

def fib_memo_rec(n):
    res = [None] * max(n + 1, 2)
    res[0] = 0
    res[1] = 1
    def aux(n):
        if res[n] != None:
            return res[n]
        else:
            x = aux(n - 1) + aux(n - 2)
            res[n] = x
            return x

    return aux(n)


#internet - http://ujihisa.blogspot.si/2010/11/memoized-recursive-fibonacci-in-python.html
#tko smo tud na predavanjih definiral memoizacijo y Pythonu v splosnem

def memoize(f):
    cache = {}
    def decorated_function(*args):
        if args in cache:
            return cache[args]
        else:
            cache[args] = f(*args)
            return cache[args]
    return decorated_function

@memoize
def fibb(n):
    return n if n < 2 else fibb(n-2) + fibb(n-1)


#same function, but shorter

def memoize_1(f):
    cache = {}
    return lambda *args: cache[args] if args in cache else cache.update({args: f(*args)}) or cache[args]

@memoize
def fib_1(n):
    return n if n < 2 else fib_1(n-2) + fib_1(n-1)

# Na katere podprobleme se direktno skicuje rekurzivna definicija fib?

# Definirajte fib ki gradi rezultat od spodaj navzgor (torej računa in si zapomni
# vrednosti od 1 proti n.)
def fib_memo_iter(n):
    res = [None] * max(2, n + 1)
    res[0] = 0
    res[1] = 1
    for i in range(2, n + 1):
        res[i] = res[i - 1] + res[i - 2]
    return res[i]


# Izboljšajte prejšnjo različico tako, da hrani zgolj rezultate, ki jih v
# nadaljevanju nujno potrebuje.
def fib_iter(n):
    res1 = 0
    res2 = 1
    for i in range(2, n + 1):
        res = res1 + res2
        res1 = res2
        res2 = res
    return res
        

