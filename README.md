# Atomic

Hey.

Hey you.

With the nice shoes.

Are you tired of seeing error messages like this

```
insert python version error
```

or this

```
insert something else
```

or this?

```
one more
```

Yeah, we are too. Why is it so hard to deal with dependencies? Every language seems to have the same problems.

And we have a hypothesis why.

Languages generally are built to have cool features and clever designs for the language itself, but little thought gets put into code distribution. Package managers get bolted on after the runtime and compiler have already been built, and innovation is extremely limited. The current system of module imports is hardly any different from the C processor macros from fifty years ago.

We are trying something new with Atomic. We are trying to build a language that puts distribution front and center.

## So what is the big idea?

The big idea is to steal from git. If you don't know, under the hood in git there is a content addressed database that stores all sorts of data about the state of the repository. The actual contents are pretty interesting, but what you need to know is that

1. You can store all sorts of data in the content addressed database.
2. Whatever you put in there gets hashed. The name of the data is the hash.
3. Data can refer to other data by hash.
4. Once something is put in the database, it cannot change. You can add new things, but once data is entered the data is immutable.

It is perhaps not obvious why these rules make git so amazing, but meditate on them for a while and you might start to get it. An immutable content addressed database is an awesome way to reliably store data that depends on other data that depends on other data that depends on...

So we thought, hey, if it works for git, why don't we do the same thing for a programming language?

See one of the big problems you have with code distribution is that we are distributing too much stuff all together. Say you have a library with ten thousand lines of code. A security vulnerability is revealed with some off-by-one error in one line somewhere. The vulnerability is patched, but now the entire library has to be redownloaded to update it - all ten thousand lines - for a measly change of a couple characters.

Uh yeah this needs some work. Really the core idea is that we distribute not libraries but individual values: constants, functions, records of constants and functions, etc.

And since the values are stored in a hashed database, we don't really need to know where they came from. This enables extremely flexible delivery policies - caching, distributed databases, untrusted sources, etc.

This also means awesome things like the fact that dependencies are automatically shared without causing problems thanks to multiple versions of the same dependency.

Also since values will only bring in their direct dependencies almost all software should be much smaller by construction than anything else. You never have to prune symbols from libraries. This happens on the front end - you don't even pull in libraries in the first place!

Also this allows you to do nifty things like use a single function from a library without pulling in the whole library.

## Dreaming big

While the immediate goals of this project are to provide a demonstration - a prototype proof-of-concept that the ideas are worthwhile, we have greater ambitions. We think the whole world could be rebuilt in the image of Atomic.

- Websites just download little pieces of code that depend on other little pieces of code. The atomic runtime system could replace a good portion of the complexity of the web as an application delivery system. We could even encode not just programs, but data like images and audio and deliver that in a highly structured format.
- Dependent types could work wonders with Atomic and the above notion of structured data. Instead of relying on informal data specifications with parsers, the Atomic runtime could do all the content delivery, parsing, and validation for us.
- Lazy loading of dependencies could allow extremely fast startup times for applications. Imagine a CAD program that you didn't really need to download before using - you just grabbed the hash and have the Atomic runtime start executing. As the application loads into the local atomic cache it's performance will increase, but you don't have to wait for all that to happen before starting to use it.

## Challenges

Atomic is a very exciting prospect. The possiblities are enormous. But then so are the challenges. We suspect that these challenges will solve themselves as we get used to dealing with a new system like this, and in fact we suspect that many of the solutions will be far better than what we have today. But until then we want to figure out a way to deal with them.

- How do we manage securing vulnerable software with Atomic? We could perhaps publish lists of specific hashes that have known vulnerabilities and warn users when values depend on hashes with those vulnerabilities. But this could be tedious and error-prone.
- Upgrade paths in general seem like a difficult thing to pin down. For this I suspect taking inspiration from git will find us a solution but idk for sure.
- This is alien stuff. How do we get good performance from this sort of system? How do we get good ergonomics? What standard programming idioms will break entirely when we don't have libraries? How can we take advantage of these ideas? How can we explain the core ideas and get new people on board?

## What can I do to help?

If you want to help us, the greatest thing you could do is give us feedback. Let us know you are interested in the project, give us some use cases, suggest some improvements. We want to know what you need as a user.

The next best thing you can do is talk about us. Tell your friends, post on social media, whatever. We are very excited about this project and the potential it has and we want to make sure everyone who is as excited as we are knows about it.

Finally, you can contribute. Contribute by improving the core language. Contribute documentation. Contributed by building things on top of Atomic. Everything helps.

## Can I use these ideas for my own project?

Please, *please* steal whatever ideas you can from this. While everyone working on Atomic wants fame, riches, and glory, our very first priority is to fix the mess that is the modern software distribution system. It is far more important that the core ideas get out there than that anyone on this project gets credit for them.

That said, we would all be very grateful if you could mention us. It doesn't cost that much.

Or if you are building something cool based off the ideas of Atomic, at least let us know. We want to know what cool things you are building!

## Priorities and tradeoffs / defense of design

Since this is a prototype, we are doing prototypish things. That means some tradeoffs.

In every case we are going to prefer simplicity of implementation to just about any other factor. We want high development velocity to test a lot of things, so we are willing to sacrifice for simplicity.

Interesting features are preferred to stability. Again, this not being intended for production use gives us opportunities to explore different designs.

Security and performance will be the primary sacrifices. I suspect that we could actually hit huge gains in both of these domains, but security and performance are worth nothing on a system that no-one uses. Version two of atomic we will explore the possibilities for security and performance benefits from atomic, but until then we are focused on the ergonomics and developer experience.

Quick aside on the security and performance world. There are reasons I think we could win out on both domains.

On the security side we could have a much, much better picture for secure source code. Since we can explicitly refer to libraries by hash, so long as the hashing algorithm, the build system, and the runtime system are all secure there won't be a way to inject code. This is a much smaller attack surface area than traditional build systems.

We also benefit in security by clearly delineating which parts of program compilation are entirely deterministic and which parts depend on some sort of I/O that could be hijacked. Many build systems do not include this clarity, but since we can pull values by hash directly we reduce the amount of external code we have to trust dramatically.

On performance, we can think of performance at the micro-scale and the macro-scale. Micro scale performance is what you look at when optimizing hot loops in your code - all the minor optimizations, equivalent term expressions, useful architectural details, and so on you can exploit to make your code run in twenty clock cycles instead of three hundred. As far as I can see there is no reason to believe Atomic would be any better than any other system. In order to get Atomic to go super duper fast will require some elbow grease but term rewrite systems are getting pretty dang fast. There are all sorts of ways to go to make things rip - we could have a JIT that operates on the value database or you could explicitly compile a particular value into a proper executable or we could compile into something like HVM or LLVM.

But anyhow I don't see any benefits to micro-scale optimization. HOWEVER.

I think the applications to macro-scale code optimization could be tremendous. This sort of optimization is the high-level architectural optimization we seek. I believe there is a lot of low-hanging fruit here and that the value of macro-scale optimization is substantially greater than the value of micro-scale optimization in the modern environment.

First of all code bundles will automatically be minimal, or very nearly minimal. Since dependencies are tracked at the value level, it is trivial to recursively enumerate all dependencies that give context to one value and import just those dependencies. Forget having to download two hundred megabytes to bootstrap an electron app with javascript - using Atomic as an IR will give you the equivalent to code bundles that have all unused symbols stripped out by default.

Second, perhaps more important, because Atomic will make code distribution so much easier it will make it possible to reuse the same code all over the place. Where before atomic it may have seemed silly to import an entire library just to take advantage of one-or-two heavily optimized functions, with Atomic you don't have to worry about the imports! Values are only loaded by demand, so it will make sense to lean heavily on other peoples code.

It is universally true that the best way to make your code faster is to use the code someone else has already optimized. Want to write a fast matrix operation in Python? Don't be an idiot - just use numpy! Whenever code sharing becomes easier performance benefits.

So I have high hopes for both security and performance from Atomic. But that isn't going into V1 of the system.

## IR for code distribution

Perhaps the best way to understand Atomic is that it is an intermediate representation for code distribution. We have intermediate targets for compilers to emit code before optimization. Atomic is just like that but aims to do so for code distribution too. Just as LLVM permits optimizations to be written once and then ported to dozens of languages simultaneously, Atomic will enable software distribution schemes to be built once and then distributed everywhere.

So instead of writing your language and then realizing you need a way to distribute code, just compile down to the Atomic IR and we handle the code distribution for you! Simple as.

## TODO

- [ ] Front End Language (FEL) definition
- [ ] Pretty print FEL
- [ ] Compile FEL to core
- [ ] Execute core
- [ ] HTTP server to download core
- [ ] Continuation-based IO system

## MISC


-- it would be nifty to display a dependency graph for our terms. that could be a killer feature here

-- one entry - we just eval a term
-- could be e.g. `atomic '#IMPORT "path/local-file.a"'

-- we gonna want to have multiple different value DB sources. need to spec a simple communication protcol.
-- thinking we could have a local caching server, remote servers (perhaps several), and other systems

-- a major challenge is the way we pair documentation to the ValueDB. how
-- are people supposed to discover relevant information? something we need to think about

-- also, next steps require us to be able to push and pull from a db
-- for now lets suppose that a db is append only. don't worry about space. all you can do is push

-- obvious important thing to do is find the transitive closure of dependencies for a core expression