* TODO write doc

- Some background:
- 2:22:45 ealfonso I like the explicit error handling approach of golang and google c++ statusor, is there something similar in lisp?
- 2:23:42 AAA how does it work?
- 2:26:36 ealfonso in c++ there is a type StatusOr<T>, which could be a value or an error. then there are macros: ASSIGN_OR_RETURN(auto value, file::GetFileContents(filename));
- 2:27:29
- ealfonso GetFileContents may return a status (error), which is propagated to the caller of the function containing ASSIGN_OR_RETURN
- 2:27:35 AAA so an option type?
- 2:27:50 ealfonso assuming that function also has a StatusOr<T> return type
- 2:30:44 ealfonso yeah, I wasn't familiar with the name "option type". I haven't seen much CL code using this pattern
- 2:31:14 AAA me neither. it's related to having things that return a value or nil, but more involved with the monad kind of stuff
- 2:40:51 ealfonso the thing about returning nil for an error is that it makes it hard for a lower-level utility function to propagate the reason of an error up the stack to a caller who may be in a better position to handle it, e.g. display a message to the user, try again, etc. of course this could be done using conditions.
- 2:41:42 ealfonso but it seems more tedious to define the condition, handle it.
- 2:42:53 ealfonso if-let doesn't give you context on where execution stopped because of a nil, and why
- 2:44:43 ealfonso another problem with nil as sentinel for error is that it conflicts with "empty, no error"
- 2:50:05 ealfonso one could write an "if-let-ok" macro, where the let expressions return an option type as 2-values, and with the "else" clause capturing the first non-ok option. but it would be an uphill battle since most existing code doesn't use this convention.
- 2:54:04 BBB https://srfi.schemers.org/srfi-189/srfi-189.html is a very new proposal for Maybe aka Option and Either types for Scheme, but there is no reason it couldn't be translated to CL.  It also has a number of protocol converters that interchange between Maybe objects and various other conventions: value or NIL, 1 or more values or 0 values, etc. etc.
- 3:05:04 ealfonso interesting. yeah, I'm curious to follow how that will go, and maybe it could be adapted to CL.
- 3:06:26 ealfonso for now I will start using an "if-let-ok" macro pattern for new code and seeing how it goes
- 3:14:05 AAA i feel like monads use some of their luster in an environment that doesn't use a lot of pattern matching
- 3:14:37 CCC for what it's worth, although the new package 'maiden,' does not work as described, the former package 'colleen' seems to work just fine.
- 3:14:41 AAA plus Just conses
- 3:42:18 beach Good morning everyone!
- 3:59:59 BBB ealfonso: It's stable now.
- 4:00:47 BBB The general monad library does what Haskell does under the covers: you pass around an object with procedures in it that contain the basic necessities.
-
