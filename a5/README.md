# Vehicle Analysis

This is our first multi-file program and it's a really nice opportunity to explore
what domain-modelling in a functional programming language looks like. You'll
realise that a lot of the things you may have believed were unique or exclusive
to object oriented programming aren't really exclusive to it at all, and that
all the domain-modelling essentials tend to be available across paradigms.

The `fuel.ml`, `lease.ml` and `vehicle.ml` files contain modules with types
and `string_of` functions for each type respectively. No special declaration
syntax is needed to define a module. Any `.ml` file will be made into a module
whose name corresponds to the filename, however the first letter will be capitalized.
For example, the `fuel.ml` file is implicitly turned into a module called `Fuel`.
Your editor will give you grief about the modules being unbound if you don't compile
them before you use them in another file/module.

You will notice that
in some cases it makes more sense to use an ADT (Algebraic Data Type) than
a record type. Of course, all the business logic is in `scenario_analysis.ml`
and the majority of the code really is just parsing the input file. The
solution I've written is not different computationally from a working
solution to the original assignment in Java, but the way we retrieve
and treat data is obviously different. We never mutate any data besides
the input stream while parsing in vehicles, so notice how that affects
the way the code is designed.

As a final note, if you've never compiled a multi-file OCaml project
before and want to know the mechanics of doing so, feel free to
check out the Makefile. It's just a matter of making sure you pass
all the necessary `.ml` files to the compiler.
