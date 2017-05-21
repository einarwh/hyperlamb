# hyperlamb
Hyperlamb is a hypermedia-driven lambda calculus evaluator. Yes.

This is very much work in progress.

## Starting the lambda calculus evaluator
If you want, you can clone the repository, build the project, open a browser and navigate to http://localhost:8080/hyperlamb. However, it's even easier to go to http://hyperlamb.azurewebsites.net

In either case, you should see a text box where you can enter a lambda expression.

You might want to start with something simple, like the identity function:

```
λx.x
```

But there's not a whole lot you can do with an expression like that. In particular, it is already on _normal form_, which means it cannot be reduced further. So you might want to try something more involved, like this:

```
(λn.λf.λx.f (n f x)) (λf.λx.x)
```

Which can be interpreted as applying the successor function to the Church encoding of the number zero. In general when a lambda expression can be reduced further, there will be a _next_ link available to perform a reduction step. If the evaluation process terminates (as it will in this case), we will eventually end up with an expression on _normal form_ again.

Lambda expressions may be _named_ to make them slightly more wieldy to work with. Adding a name to a lambda expression creates an alias hyperlink for navigating to the expression. 

An example should make things clearer. Say we have the expression for the Church encoding of zero:

```
λf.λx.x
```

We find it useful to add the name _zero_ for this expression. This adds a hyperlink, /hyperlamb/names/zero, that can be used to navigate to the expression. In addition, the name will be shown whenever we look at that particular lambda expression. You can also use the name as an expanding text macro in your lambda expressions, by prefixing the name with a dollar sign. For example, you can write $zero in a lambda expression and have it expand to the expression with that name.

If you proceed to add the name _succ_ to the following expression:

```
λn.λf.λx.f (n f x)
```

You can now write *$succ $zero* and have it expand to this:

```
(λn.λf.λx.f (n f x)) (λf.λx.x)
```
