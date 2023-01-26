## Untyped Lambda Calculus  
Implementation of untyped lambda calculus using Haskell StateT, Reduction Strategies, List Examples

### Usage 
```
> runhaskell.exe main.hs test01.txt 
```  

main.hs  
``` haskell 
...  
{-- import only one Eval file for the Reduction Strategy you want to run ----}
import Eval1NOR -- Normal Order
--import Eval2CBV -- Call-by-value
--import Eval3HD -- Head 
--import Eval4CBN -- Call-by-name
--import Eval5APP -- Applicative Order 
...
```


```
{- test01.txt block comment -}

(\x.x) ((\x.x) (\z. (\x.x) z)); -- line comment
y/;
z/;
(\x.((x y)(y x)))((\w.(w w))z); 
(\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f x)) (\f.\x.f (f x));
```

`lambda abstraction := \x. expr`

<table>
  <caption>result</caption>
  <thead>
    <tr>
      <th></th> <th>\x.M ---> \x.M'</th><th>\x.M -/-> \x.M'</th>
  </thead>
  <tr>
    <th align="center"> App t1 t2<br>   ---><br> App t1 t2' </th>
    <td>1NOR (Normal Order Reduction)<br>
        5APP (Applicative Order<sup>&dagger;&dagger;</sup> Reduction)<br>
        (\z.z);
        <br>y/;<br>z/;<br>
        (((z z) y) (y (z z)));<br>
        (\f.(\x.(f (f (f (f x))))));
    </td>
    <td>2CBV (Call-by-value Reduction)<br><br>(\z.((\x.x) z));
        <br>y/;<br>z/;<br>
        ((\x.((x y) (y x))) (z z));<br>
        (\f.(\x.(((\f'.(\x'.(f' (f' x')))) f) (((\f'.(\x'.(f' (f' x')))) f) x))));
    </td>
  </tr>
  <tr>
    <th align="center"> App t1 t2<br> -/-><br> App t1 t2' </th>
    <td>3HD (Head<sup>&dagger;</sup> Reduction )<br>(\z.z);
        <br>y/;<br>z/;<br>
        (((z z) y) (y ((\w.(w w)) z)));<br>
        (\f.(\x.(f (f (((\f'.(\x'.(f' (f' x')))) f) x)))));
    </td>
    <td>4CBN (Call-by-name Reduction)<br>(\z.((\x.x) z));
        <br>y/;<br>z/;<br>
        (((z z) y) (y ((\w.(w w)) z)));<br>
        (\f.(\x.(((\f'.(\x'.(f' (f' x')))) f) (((\f'.(\x'.(f' (f' x')))) f) x))));
    </td>
  </tr>
</table>


<sup>&dagger;</sup> Head : Reduce the outermost redex and inside abstractions, no reduction under argument.

<sup>&dagger;&dagger;</sup> Applicative Order : Reduce the leftmost innermost redex and inside abstractions, reduce argument before applying function.
#### List Examples (Normal Order Reduction only)

```
> runhaskell.exe main.hs test02.txt 
``` 

```
{--------------- test02.txt list examples --------------}

{-------- "Collected Lambda Calculus Functions" --------}
{--- from https://jwodder.freeshell.org/lambda.html  ---}

{---------- Booleans ----------}
True := \t. \f. t; 
False := \t. \f. f;
And :=\p.\q. p q False;
Or :=\p.\q. p True q;
Not := \p. p False True;

{---------- Pairs -----------}
Pair := \f.\s.\b. b f s;
Fst := \p. p (\t. \f. t);
Snd := \p. p (\t. \f. f);

{---------- Mathematical Operators -----------}
Succ := \n. \s. \z. s (n s z); 
Succ2 := \n. \s. \z. (n s (s z));  -- another Succ
IsZro := \m. m (\x. False) True;
Plus := \m. \n. \f. \x. m f (n f x);

{---------- Natural Numbers -----------}
c0 := \s. \z. z; -- zero
c1 := Succ c0; 
c2 := Succ c1; 
c3 := Succ c2;
c4 := Succ c3;
c5 = Succ2 c4;

{---------- List ----------}
Cons := Pair;
Nil := \x. True;
Head := Fst;
Tail := Snd;
IsNil := \p. p (\x. \y. False);

{---------- List Functions ----------}
Len := \self. \xs. (IsNil xs) c0 (Plus (self (Tail xs)) c1);
Sum := \self. \xs. (IsNil xs) c0 (Plus (Head xs) (self (Tail xs)));
Nth:= \x.\i. Fst (i Snd x);
Append := \g.\a.\b. IsNil a b (Pair (Fst a) (g (Snd a) b));

{---------- Fixed point combinators ----------}
YCombi := \f. (\x. f (x x)) (\x. f (x x));
ZCombi := \f. (\x. f (\y. x x y)) (\x. f (\y. x x y));
TuringCombi := (\x.\y.y(x x y))(\x.\y.y(x x y));


-- list examples
ls2 := Cons c1(Cons c0 Nil);
ls3 := Cons c4(Cons c3(Cons c2 Nil));
appendLs2Ls3 := YCombi Append ls2 ls3;
nthappendLs2Ls3c4 = Nth appendLs2Ls3 c4;
lenAppendLs2Ls3 = ZCombi Len appendLs2Ls3;
sumAppendLs2Ls3 = TuringCombi Sum appendLs2Ls3;
```


result  

```  
True := (\t.(\f.t));
False := (\t.(\f.f));
...
...
ls2 := ((Cons c1) ((Cons c0) Nil));
ls3 := ((Cons c4) ((Cons c3) ((Cons c2) Nil)));
appendLs2Ls3 := (((YCombi Append) ls2) ls3);
nthAppendLs2Ls3c4 = (\s.(\z.(s (s z))));
lenAppendLs2Ls3 = (\f.(\x.(f (f (f (f (f x)))))));
sumAppendLs2Ls3 = (\f.(\x.(f (f (f (f (f (f (f (f (f (f x))))))))))));
[Finished in 14.4s]

``` 


#### Reference

[Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/)  implementations untyped fulluntyped