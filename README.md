# hyperlamb
Hyperlamb is a hypermedia-driven lambda calculus evaluator. Yes.

## Starting the lambda calculus evaluator
Open a browser and navigate to http://localhost:8080/hyperlamb

You may enter a lambda expression here.

You might want to start with something simple, like the identity function:

```
λx.x
```

But there's not a whole lot you can do with an expression like that. In particular, it is already on _normal form_, which means it cannot be reduced further. So you might want to try something more involved, like this:

```
(λn.λf.λx.f (n f x)) (λf.λx.x)
```

Which can be interpreted as applying the successor function to the Church encoding of the number zero. In general when a lambda expression can be reduced further, there will be a _next_ link available to perform a reduction step. If the evaluation process terminates (as it will in this case), we will eventually end up with an expression on _normal form_ again.

Lambda expressions may be _named_ to make them slightly more wieldy to work with. This is work in progress.
