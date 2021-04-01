# Avaleryar

An implementation of [Soutei](http://okmij.org/ftp/papers/Soutei.pdf).

Not yet fit for human consumption.

> It's easier to ask forgiveness than it is to get permission. --Rear Admiral Grace Murray Hopper

## Overview

Avaleryar is an implementation of Pimlott and Kiselyov's Soutei trust-management system, comprising

* A [Datalog](https://en.wikipedia.org/wiki/Datalog)-like rule language for describing authorization
  policies (including the evaluator, parser, and a pretty-printer).
* Convenient library support for extending the policy language with application-specific predicates.
* A REPL for exploring and debugging policies.
* An integrated unit-testing system. (NB: It's pretty bad; we intend to improve it someday).

We have more improvements planned for the future, which you can read about
[#planned-improvements](below).

## A Quick Example

Soutei is an extremely flexible system, capable of expressing various styles of authorization
policies (RBAC, ABAC, other acronyms that end in -BAC).  For this introductory example, we'll write
a simple policy for a hypothetical blogging platform.  When a user attempts to take an action, the
platform will consult the policy to advise it whether or not to allow the operation to proceed.
Here's an informal version of our policy, in English:

* Anyone may read a post that's been published
* Anyone who is allowed to edit a post may read it
* The author of a post may edit and publish it
* The owner of a blog may create new posts
* A user may leave a comment on a post if they're permitted to read it, and are friends with the
  post's author.

```prolog
;; Anyone may read a post that's been published
may(read) :-
  application says status(published).
  
;; Anyone who is allowed to edit a post may read it
may(read) :-
  may(edit).

;; The author of a post may edit it...
may(edit) :-
  application says user(?user),
  application says author(?user).

;; ...and publish it
may(publish) :-
  application says user(?user),
  application says author(?user).

;; The owner of a blog may create new posts
may(create) :-
  application says user(?user),
  application says blog-owner(?user).

;; A user may leave a comment on a post if they're permitted to read it, and are friends with the
;; post's author.
may(comment) :-
  may(read),
  application says user(?user),
  application says author(?author),
  ?author says friend(?user).
```

What we have is a collection of _rules_ describing the circumstances under which a particular action
should be permitted.  When our blog application wants to know whether a request is authorized, it
will ask Soutei.  Soutei will then try to prove that the rules permit the access somehow, and let
the application know if it succeeded.  We'll discuss this syntax in detail below.  Briefly, though,
you can decode, say, the third rule (the author of a post may edit and publish it) like this:
"`edit` is permitted when the application tells us there's a user (denoted by the _variable_
`?user`) associated with the request, and the application tells us that that user is identically the
author of the post".  Observe that `:-` (which I'll pronounce "when") is like an implication in
logic.  The second rule (if you're allowed to edit you're allowed to read) is just saying that
`edit` implies `read` (or, since the implication goes right-to-left, perhaps "`read` is implied by
`edit`" would be better).

Don't worry if this is still confusing.  The example demonstrates several different features of the
system.  Nonetheless, I hope you agree it's a relatively compact and clean description of a
not-completely-trivial authorization policy.

## Using Soutei for Authorization

Architecturally, Soutei provides support for implementing a so-called "policy decision point".  Its
job is to advise an application on whether it should permit a request by determining if it complies
with its security policy.  It does _not_ provide policy _enforcement_.  This is to say, you ask
whether to allow access, and you get an answer, but it's still up to you to send the 403 back to the
client if that answer was "no".

To make use of the policy engine, you formulate a query and send it along.  For instance, when
implementing the commenting system for your blog platform, you might ask Soutei `may(comment)` (this
is concrete syntax, in practice you'll assemble the query using library functions).  Of course,
whether commenting is permitted depends on information that Soutei doesn't have.  In our example,
that's at least the identity of the commenting user and the author of the post, and probably some
more information necessary to deduce the ability to read the post.  Soutei only knows about the
rules you've given it, so where do these extra facts about this particular commenting operation come
from?  The application provides them as a parameter to the query.  So in our example, the
application might ask "is commenting permitted? (and by the way, the post has been published, the
current user is `bob`, the author of the post is `alice`, and `alice` says `bob` is her friend, in
case that helps you make your decision)".

This may be a rather different style of access control than you're used to, and it requires a bit of
a shift in perspective.

TODO: Say more here.

## Syntax and Semantics

Here's a rule about activities:

```prolog
can(?activity) :-
  want-to(?activity).
```

It says that an activity is permitted (`can(?activity)`) when we want to do that activity
(`want-to(?activity)`).  We can see several features of the syntax in this small example.  Rules
have two parts (a **head** and a **body**), separated by the symbol `:-` (which I tend to pronounce
"when"), and terminated with a period.  The head of the rule is `can(?activity)`, and the body of
the rule is `want-to(?activity)`. They use a function-call like notation (called a **literal**) to
describe logical **predicates** (i.e., `can` and `want-to`).  Predicates are usually denoted with
their arity, so rather than writing `can`, we'd write `can/1`.  If we had a predicate describing
friendship between two people (`friend(alice, bob)`), we'd refer to it as `friend/2`.  We can also
see that **variables** are written with a prefix question-mark (`?activity`).

If this rule were our entire policy, it would never permit us to do anything, because it has no way
to establish what we `want-to/1` do.  In general, determining our psyche's innermost desires can be
complicated, and we could write a bunch of complicated rules to define `want-to/1`.  But let's just
assume we're blessed with unusual self awareness, and add a special kind of rule, called a **fact**,
that will express our yearning:

```prolog
can(?activity) :-
  want-to(?activity).
  
;; we can dance if we want to
want-to(dance).
```

We can see that a fact is a rule without a body.  We can also see that comments are introduced with
semicolons and extend to the end of the line.  This policy will now permit a query of the form
`can(dance)`.  Let's look at how that deduction works.  We ask Soutei `can(dance)`, and it reasons: "I
can prove `can(dance)` if, when `?activity` is `dance`, I can prove `want-to(dance)`.  Oh! and I can
prove `want-to(dance)` because I know that for fact!".

Notice that `dance` isn't a variable, it's just a symbol.  We could have written `want-to("dance")`,
using double-quotes to delimit the string, but it's unnecessary (and un-idiomatic) when the string
doesn't contain spaces or commas.

Now suppose we want to say that we can dance if we want to, act if we want to, but sing under any
circumstances?  (This whole example will probably make more sense with a bit more
[context](https://www.youtube.com/watch?v=AjPau5QYtYs)):

```prolog
can(?activity) :-
  want-to(?activity).

can(sing).

;; we can dance if we want to
want-to(dance).

;; we can act if we want to
want-to(act).
```

What `can/1` we do now?  Well, we `can(dance)`, we `can(dance)`, we `can(sing)`, and we `can(act)`.
This shows that we're able to express different ways to conclude that an action is permitted by
writing multiple rules for the same predicate (remember that facts are rules without bodies).  Rule
bodies aren't limited to a single literal.  Let's add an example:

```prolog
can(?activity) :-
  want-to(?activity).

can(sing).

;; we can dance if we want to
want-to(dance).

;; we can act if we want to
want-to(act).

;; we only want to go when the night is young, and so am I
want-to(go) :-
  time-of(night, young),
  age-of(me, young).
```

We require every predicate in the body of a rule to succeed in order for the rule to succeed.  So
`want-to(go)` needs both `time-of(night, young)` and `age-of(me, young)` to be proven in order for
this rule to prove that `want-to(go)`.  Of course, what we have so far isn't enough to prove
`can(go)` yet, because `age-of/2` and `time-of/2` aren't actually defined anywhere.  This isn't
considered an error; it simply means that an attempt to prove `want-to(go)` will fail.  (As a
reminder, there's no special meaning attached to `night`, `young` or `me`---they're just symbols I'm
using to demonstrate the syntax).

It may be occurring to you by now that these rules don't actually depend on anything---the assertion
we're developing (a collection of rules like this is called an **assertion**---an assertion is kind
of like a module or a namespace) will always prove exactly the same things.  A rule may consult
predicates in _other_ assertions than the one in which it's written.  Let's hypothesize two new
assertions, `clock` and `bio`, that contain chronological and biographical rules, respectively:

```prolog
can(?activity) :-
  want-to(?activity).

can(sing).

;; we can dance if we want to
want-to(dance).

;; we can act if we want to
want-to(act).

;; we only want to go when the night is young, and so am and I
want-to(go) :-
  clock says time-of(night, young),
  bio says age-of(me, young).
```

We've changed the body of our `want-to(go)` rule by adding `clock says time-of(night, young)` and
`bio says age-of(me, young)`.  This tells Soutei to try and resolve `time-of/2` in the assertion
named `clock` and `age-of/2` in the assertion named `bio`.  By "resolve a predicate in an
assertion", I mean that Soutei will load all the rules in that assertion, and continue trying to
satisfy the predicate using the rules in _that_ assertion.  Of course, if any rule in this new
assertion has a body literal of the form `assertion says pred(...)`, then Soutei will load
the rules in `assertion` and resolve `pred` there.

Other than a short discussion about variable binding and another on so-called "native predicates",
we've now looked (albeit briefly) at the entirety of the semantics of the language.  I point that
out because in order to continue our Safety Dance, we'll need to introduce a convention that
`avaleryar` (following the original paper) imposes: the `application` assertion.  Semantically, the
`application` assertion is no different than any other.  However, when `avaleryar` runs a query, it
accepts a collection of facts as, effectively, parameters that are made available to our rules
through the `application` assertion.  (NB: I'm doing my best to distinguish Soutei-the-language from
`avaleryar`-the-implementation-of-Soutei-the-language, I hope this isn't too confusing).  So to
demonstrate, let's write a needlessly complicated rule determining whether the dancing we can do is,
properly a Safety Dance.

```prolog
can(?activity) :-
  want-to(?activity).

can(sing).

;; we can dance if we want to
want-to(dance).

;; we can act if we want to
want-to(act).

;; we only want to go when the night is young, and so am and I
want-to(go) :-
  clock says time-of(night, young),
  bio says age-of(me, young).

;; we can overextend the efficacy a questionable pop-culture reference
safety(?activity) :-
  can(?activity),
  application says out-of(control, everything),
  application says doing-it(from, pole),
  application says doing-it(to, pole),
  application says looking-at(hands, ?somebody),
  ?somebody says taking(the-chance).
```

This (completely inane, it's getting pretty late as I write this---the examples section below won't
be this silly) new rule uses a bunch of information provided by the application querying for
authorization advice (`application says ...`)  in addition to some locally written rules
(`can(?activity)`).  It also uses an assertion _determined by the query_ (`?somebody says
taking(the-chance)`) to ultimately establish that indeed, `safety(dance)`.  The ability to
dynamically choose different assertions in which to reason is a powerfully expressive feature of
Soutei.

TODO: unification and native predicates.

TODO: why encoding "'Cause your friends don't dance, and if they don't dance, then they're no
friends of mine" isn't (naively) possible.

TODO: mode checking.

TODO: monotonicity.

## Examples and Advice

### Example: Unix File Permissions

Here is a simplified version of file permissions on Unix.  We assume that the application will tell
us what the file is, that there's an assertion named after the file that knows what permissions are
set on it, who owns it, and which group it's associated with, as well as that each group has an
assertion named after it that can tell us whether a user is a member of that group.  For example, we
might have an assertion for some file that looked like:

```prolog
;; assertion for /path/to/some/file

owner(mary).
group(wheel).
perm(user, read).
perm(user, write).
perm(user, execute).
perm(group, read).
perm(group, execute).
perm(other, read).
```

This would represent a file owned by `mary`, with group `wheel`, with permissions `754` (as in,
`chmod 754 /path/to/some/file`).

```prolog
;; allow ?access when the user is the owner and ?access is enabled for them.
may(?access) :-
  application says file(?file),
  application says user(?user),
  ?file says owner(?user),
  ?file says perm(user, ?access).

;; allow ?access when it's enabled on the file, the file is associated with a group ?group,
;; and the user is a member of ?group.
may(?access) :-
  application says file(?file),
  application says user(?user),
  ?file says group(?group),
  ?group says member(?user),
  ?file says perm(group, ?access).
  
;; allow ?access if it's enabled for all users ("other" permissions)
may(?access) :-
  application says file(?file),
  ?file says perm(other, ?access).
```

Unix permissions are more sophisticated than this---for instance, if you have `read` access to a
directory, you are permitted to see the _names_ of the files in that directory, but not other
metadata (roughly, you're allowed to see the output of `ls`, but not `ls -l`) unless you also have
`execute` permission on the directory as well.  A more nuanced version of this policy would need to
know what metadata is being sought by the application, and probably require some native predicates
to compute the directory part of a file path.

### Example: Role-Based Access Control (RBAC)

In RBAC, we have a discrete set of primitive _permissions_, a collection of named _roles_ each of
which is a subset of the permissions, and an assignment of users to (possibly multiple) roles.  To
determine if a user is permitted to take some action, we check that the user has been assigned some
role that contains the appropriate permission.

```prolog
;; permissions

perm(manage-users).
perm(manage-computers).
perm(access-lab).
perm(run-experiment).
perm(create-experiment).
perm(approve-experiment).

;; roles

; grad students get to do all the work
role(grad-student, access-lab).
role(grad-student, run-experiment).

; professors have at least the permissions of grad students, plus they can
; create experiments.
role(professor, ?perm) :-
  role(grad-student, ?perm).
role(professor, create-experiment).

; poison control should probably be allowed into the lab, just in case
role(poison-control, access-lab).

; the IT department needs to access the lab to manage its computers
role(it-support, access-lab).
role(it-support, manage-computers).

; the dean has nothing to do with the science, but they still sign the
; checks and assign personel
role(dean, approve-experiment).
role(dean, manage-users).

;; users

has-role(bill, grad-student).
has-role(clara, professor).
has-role(dmitri, grad-student).
has-role(dmitri, poison-control). ; dmitri is a volunteer EMT
has-role(emily, it-support).
has-role(fabian, dean).

;; RBAC

may(?perm) :-
  application says user(?user),
  has-role(?user, ?role),
  role(?role, ?perm).
```

### Example: Access Control Lists (ACLs)

### Advice: Don't use ACLs

## Using Avaleryar

TODO: tutorial module.

## Glossary

## Planned Improvements

* An interactive debugger
* A better persistence story (to make dynamic rule submission usable)
* Assertion signatures (enabling the use of variables for native assertions)
* Some kind of [abduction](https://en.wikipedia.org/wiki/Abductive_logic_programming) (to offer
  explanations of query failure)
