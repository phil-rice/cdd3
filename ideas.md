#language

Clearly scenario is now so general it can be the directly built thing: simplifying language

Usecases can be engines themselves and we can have  composition operators on them

Usecases need types: that's annoying, so have a builder in scope?

We want different languages for P => R, P1, P2 => R. The only real difference is in the when/because code
so scenario(1, 2) produces "something" when (l,r) => l<r would be nice


# Usecases defined in the constructor
Consider doing this. Many nice properties.

How do I specify the types... 

Gives scope/types for scenarios automatically. (mind you agg does that too ... ) 
Can have variables for scenarios easily accessible. Correct s
scenario builder language automatically in scope

At the end of the day it's hard to do scenarios without a problem:
Either huge compile time constructs, or side effects or coding mess... 

#Lens in Decision Tree
When making reports we want things like 'the list of decision tree nodes I went through' i.e ancestors
There is a lot of type casting going on... can we avoid this?
The implicit constraint that we have nodes and leaves is hard to capture with lens / the type system ... is this why we are having to cast?


# Engine builder debugger
Have this as an implicit in scope. default does nothing

# Engines should have a debug interface.

This will allow things like: show me the path I took to get here.
It will allow omniscient debugging which is seriously cool 

#Engine composition

Obviously we can smash engines with same P R together
We can chain use cases making a new engine that does 1 then 2
We can fan in E[X, P1], E[Y, P2] + E[(P1, P2), Z] => E[(X,Y), Z]

#Engines and monads
Really want this to work with microservices.
If Engines are kleslis that just works.

Can have Kleisli monad easy enough

This allows things that have to throw exceptions / fail to be done easily


# remember
Most things are just small chunks of behavior and we mix them up to do what we want. Most usecases will 
have one to three scenarios.
Some things will have thousands of chunks of behavior: These are our USP so they have to work, but
not at the expense of destroying the ease of use of the simple cases.


#Junit testing story
Is less important. A smoke test does good stuff. 

We can have a big composed engine and give it to the Junit tester though. That works! And we can split it 
automatically into suites which will be nice.


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


#Reports
Need to be moved from Cdd2

#Website
Ditto

# Engines with currying etc and different arity
I think this is fairly straight forwards... need to try it

# Making engines easily from different use cases and maybe easy A/B testing and stuff...





# Examples
Lennon ... could we still do it
Tennis is a good one
Conditions of service
