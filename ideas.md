# language

Usecases can be engines themselves and we can have  composition operators on them

We want different languages for P => R, P1, P2 => R. The only real difference is in the when/because code
so scenario(1, 2) produces "something" when (l,r) => l<r would be nice


# Usecases defined in the constructor
It's a pain, but the alternatives look worse
It's a pain because we want nice error messages, which requires not having huge structures.
To not have a huge structure, I think we need side effects. I couldn't make a fluent design look nice

# Usecases are the main unit of composition
So they need issues. 
This makes the scala test reporting work nicely as well. 

# Need nicer reporting when working with tests that don't work

It would be cool to have a report with a link you can click on (probably in a temp directory)
That report should have enough info to explain the current tree, the current node, and it can't be added.

# Need to validate scenarios on creation

For example what if the code or the why or the because don't work, or an assertion fails

# Lens in Decision Tree
There is a lot of type casting going on... can we avoid this?
When making reports we want things like 'the list of decision tree nodes I went through' i.e ancestors
The implicit constraint that we have nodes and leaves is hard to capture with lens / the type system ... is this why we are having to cast?

# Engine builder debugger / logger
At the moment using 'trace' instead. 
This is probably a really nice idea.
Have this as an implicit in scope. default does nothing

# Engines should have a debug interface.

This will allow things like: show me the path I took to get here.
It will allow omniscient debugging which is seriously cool 

#Engine composition

Obviously we can smash engines with same P R together
We can chain use cases making a new engine that does 1 then 2
We can fan in E[X, P1], E[Y, P2] + E[(P1, P2), Z] => E[(X,Y), Z]

# Engines and monads
Really want this to work with microservices.
If Engines are kleslis that just works.
Can have Kleisli monad easy enough

This allows things that have to throw exceptions / fail to be done easily

Need nice names for engines

# Remember
Most things are just small chunks of behavior and we mix them up to do what we want. Most usecases will 
have one to three scenarios.
Some things will have thousands of chunks of behavior: These are our USP so they have to work, but
not at the expense of destroying the ease of use of the simple cases.


# Junit testing story

We can have a big composed engine and give it to the Junit tester.
So it will be coded rather than automatic
This is good because we are going to have curried and partially applied engines

# Tagless interpreters
I can do this for engine composition... Not sure about engine construction, but don't really need it
This is seriously exciting with microservices as can automake pacts....

This should be optional, and everything should just work without it.

# Shared Assertions
OK we can move assertions into EngineComponentData so that Usecases and Scenarios and Engines can all have assertions

Assertions are additive: a scenario has it's own, and the use cases (recursive) and the engines..

# Need to make sure that we use all given reasons

I think the merge story does this very nicely now. 


# We want to seriously think about 'training' 'test' 'final' in the work flow
We want to learn from things, and be able to add code and go really quickly

# Structure should be considered: do we want a structure engine... 
This is only worth it if it goes a lot faster.

# Reports
Need to be made much prettier. Css person.

# Website
Ditto

# Engines with currying etc and different arity
I think this is fairly straight forwards... need to try it

# Making engines easily from different use cases and maybe easy A/B testing and stuff...





# Examples
Lennon ... could we still do it
Tennis is a good one
Conditions of service
