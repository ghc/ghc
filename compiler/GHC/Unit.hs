{-# LANGUAGE FlexibleInstances #-}

-- | Units are library components from Cabal packages compiled and installed in
-- a database
module GHC.Unit
   ( module GHC.Unit.Types
   , module GHC.Unit.Info
   , module GHC.Unit.Parser
   , module GHC.Unit.State
   , module GHC.Unit.Module
   , module GHC.Unit.Home
   )
where

import GHC.Unit.Types
import GHC.Unit.Info
import GHC.Unit.Parser
import GHC.Unit.Module
import GHC.Unit.Home
-- source import to avoid DynFlags import loops
import {-# SOURCE #-} GHC.Unit.State

{-

Note [About Units]
~~~~~~~~~~~~~~~~~~

Haskell users are used to manipulate Cabal packages. These packages are
identified by:
   - a package name :: String
   - a package version :: Version
   - (a revision number, when they are registered on Hackage)

Cabal packages may contain several components (libraries, programs,
testsuites). In GHC we are mostly interested in libraries because those are
the components that can be depended upon by other components. Components in a
package are identified by their component name. Historically only one library
component was allowed per package, hence it didn't need a name. For this
reason, component name may be empty for one library component in each
package:
   - a component name :: Maybe String

UnitId
------

Cabal libraries can be compiled in various ways (different compiler options
or Cabal flags, different dependencies, etc.), hence using package name,
package version and component name isn't enough to identify a built library.
We use another identifier called UnitId:

  package name             \
  package version          |                       ________
  component name           | hash of all this ==> | UnitId |
  Cabal flags              |                       --------
  compiler options         |
  dependencies' UnitId     /

Fortunately GHC doesn't have to generate these UnitId: they are provided by
external build tools (e.g. Cabal) with `-this-unit-id` command-line parameter.

UnitIds are important because they are used to generate internal names
(symbols, etc.).

Wired-in units
--------------

Certain libraries (ghc-prim, base, etc.) are known to the compiler and to the
RTS as they provide some basic primitives.  Hence UnitIds of wired-in libraries
are fixed. Instead of letting Cabal chose the UnitId for these libraries, their
.cabal file uses the following stanza to force it to a specific value:

   ghc-options: -this-unit-id ghc-prim    -- taken from ghc-prim.cabal

The RTS also uses entities of wired-in units by directly referring to symbols
such as "base_GHCziIOziException_heapOverflow_closure" where the prefix is
the UnitId of "base" unit.

Unit databases
--------------

Units are stored in databases in order to be reused by other codes:

   UnitKey ---> UnitInfo { exposed modules, package name, package version
                           component name, various file paths,
                           dependencies :: [UnitKey], etc. }

Because of the wired-in units described above, we can't exactly use UnitIds
as UnitKeys in the database: if we did this, we could only have a single unit
(compiled library) in the database for each wired-in library. As we want to
support databases containing several different units for the same wired-in
library, we do this:

   * for non wired-in units:
      * UnitId = UnitKey = Identifier (hash) computed by Cabal

   * for wired-in units:
      * UnitKey = Identifier computed by Cabal (just like for non wired-in units)
      * UnitId  = unit-id specified with -this-unit-id command-line flag

We can expose several units to GHC via the `package-id <unit-key>` command-line
parameter. We must use the UnitKeys of the units so that GHC can find them in
the database.

During unit loading, GHC replaces UnitKeys with UnitIds. It identifies wired
units by their package name (stored in their UnitInfo) and uses wired-in UnitIds
for them.

For example, knowing that "base", "ghc-prim" and "rts" are wired-in units, the
following dependency graph expressed with database UnitKeys will be transformed
into a similar graph expressed with UnitIds:

   UnitKeys
   ~~~~~~~~                      ----------> rts-1.0-hashABC <--
                                 |                             |
                                 |                             |
   foo-2.0-hash123 --> base-4.1-hashXYZ ---> ghc-prim-0.5.3-hashUVW

   UnitIds
   ~~~~~~~               ---------------> rts <--
                         |                      |
                         |                      |
   foo-2.0-hash123 --> base ---------------> ghc-prim


Note that "foo-2.0-hash123" isn't wired-in so its UnitId is the same as its UnitKey.


Module signatures / indefinite units / instantiated units
---------------------------------------------------------

GHC distinguishes two kinds of units:

   * definite units:
      * units without module holes and with definite dependencies
      * can be compiled into machine code (.o/.a/.so/.dll/...)

   * indefinite units:
      * units with some module holes or with some indefinite dependencies
      * can only be type-checked

Module holes are constrained by module signatures (.hsig files). Module
signatures are a kind of interface (similar to .hs-boot files). They are used in
place of some real code. GHC allows modules from other units to be used to fill
these module holes: the process is called "unit/module instantiation". The
instantiating module may either be a concrete module or a module signature. In
the latter case, the signatures are merged to form a new one.

You can think of this as polymorphism at the module level: module signatures
give constraints on the "type" of module that can be used to fill the hole
(where "type" means types of the exported module entitites, etc.).

Module signatures contain enough information (datatypes, abstract types, type
synonyms, classes, etc.) to typecheck modules depending on them but not
enough to compile them. As such, indefinite units found in databases only
provide module interfaces (the .hi ones this time), not object code.

To distinguish between indefinite and definite unit ids at the type level, we
respectively use 'IndefUnitId' and 'DefUnitId' datatypes that are basically
wrappers over 'UnitId'.

Unit instantiation / on-the-fly instantiation
---------------------------------------------

Indefinite units can be instantiated with modules from other units. The
instantiating units can also be instantiated themselves (if there are
indefinite) and so on.

On-the-fly unit instantiation is a tricky optimization explained in
http://blog.ezyang.com/2016/08/optimizing-incremental-compilation
Here is a summary:

   1. Indefinite units can only be type-checked, not compiled into real code.
   Type-checking produces interface files (.hi) which are incomplete for code
   generation (they lack unfoldings, etc.) but enough to perform type-checking
   of units depending on them.

   2. Type-checking an instantiated unit is cheap as we only have to merge
   interface files (.hi) of the instantiated unit and of the instantiating
   units, hence it can be done on-the-fly. Interface files of the dependencies
   can be concrete or produced on-the-fly recursively.

   3. When we compile a unit, we mustn't use interfaces produced by the
   type-checker (on-the-fly or not) for the instantiated unit dependencies
   because they lack some information.

   4. When we type-check an indefinite unit, we must be consistent about the
   interfaces we use for each dependency: only those produced by the
   type-checker (on-the-fly or not) or only those produced after a full
   compilation, but not both at the same time.

   It can be tricky if we have the following kind of dependency graph:

      X (indefinite) ------> D (definite, compiled) -----> I (instantiated, definite, compiled)
      |----------------------------------------------------^

   Suppose we want to type-check unit X which depends on unit I and D:
      * I is definite and compiled: we have compiled .hi files for its modules on disk
      * I is instantiated: it is cheap to produce type-checker .hi files for its modules on-the-fly

   But we must not do:

      X (indefinite) ------> D (definite, compiled) -----> I (instantiated, definite, compiled)
      |--------------------------------------------------> I (instantiated on-the-fly)

      ==> inconsistent module interfaces for I

   Nor:

      X (indefinite) ------> D (definite, compiled) -------v
      |--------------------------------------------------> I (instantiated on-the-fly)

      ==> D's interfaces may refer to things that only exist in I's *compiled* interfaces

   An alternative would be to store both type-checked and compiled interfaces
   for every compiled non-instantiated unit (instantiated unit can be done
   on-the-fly) so that we could use type-checked interfaces of D in the
   example above. But it would increase compilation time and unit size.


The 'Unit' datatype represents a unit which may have been instantiated
on-the-fly:

   data Unit = RealUnit DefUnitId         -- use compiled interfaces on disk
             | VirtUnit InstantiatedUnit  -- use on-the-fly instantiation

'InstantiatedUnit' has two interesting fields:

   * instUnitInstanceOf :: IndefUnitId
      -- ^ the indefinite unit that is instantiated

   * instUnitInsts :: [(ModuleName,(Unit,ModuleName)]
      -- ^ a list of instantiations, where an instantiation is:
           (module hole name, (instantiating unit, instantiating module name))

A 'VirtUnit' may be indefinite or definite, it depends on whether some holes
remain in the instantiated unit OR in the instantiating units (recursively).
Having a fully instantiated (i.e. definite) virtual unit can lead to some issues
if there is a matching compiled unit in the preload closure.  See Note [VirtUnit
to RealUnit improvement]

Unit database and indefinite units
----------------------------------

We don't store partially instantiated units in the unit database.  Units in the
database are either:

   * definite (fully instantiated or without holes): in this case we have
     *compiled* module interfaces (.hi) and object codes (.o/.a/.so/.dll/...).

   * fully indefinite (not instantiated at all): in this case we only have
     *type-checked* module interfaces (.hi).

Note that indefinite units are stored as an instantiation of themselves where
each instantiating module is a module variable (see Note [Representation of
module/name variables]). E.g.

   "xyz" (UnitKey) ---> UnitInfo { instanceOf       = "xyz"
                                 , instantiatedWith = [A=<A>,B=<B>...]
                                 , ...
                                 }

Note that non-instantiated units are also stored as an instantiation of
themselves.  It is a reminiscence of previous terminology (when "instanceOf" was
"componentId"). E.g.

   "xyz" (UnitKey) ---> UnitInfo { instanceOf       = "xyz"
                                 , instantiatedWith = []
                                 , ...
                                 }

TODO: We should probably have `instanceOf :: Maybe IndefUnitId` instead.


Note [Pretty-printing UnitId]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we pretty-print a UnitId for the user, we try to map it back to its origin
package name, version and component to print "package-version:component" instead
of some hash. How to retrieve these information from a UnitId?

Solution 0: ask for a UnitState to be passed each time we want to pretty-print a
SDoc so that the Outputable instance for UnitId could retrieve the information
from it. That what we used in the past: a DynFlags was passed and the UnitState
was retrieved from it. This is wrong for several reasons:

    1. The UnitState is accessed when the message is printed, not when it is
       generated. So we could imagine that the UnitState could have changed
       in-between. Especially if we want to allow unit unloading.

    2. We want GHC to support several independent sessions at once, hence
       several UnitState. This approach supposes there is a unique UnitState
       (the one given at printing-time), moreover a UnitId doesn't indicate
       which UnitState it comes from (think about statically defined UnitId for
       wired-in units).

Solution 1: an obvious approach would be to store the required information in
the UnitId itself. However it doesn't work because some UnitId are defined
statically for wired-in units and the same UnitId can map to different units in
different contexts. This solution would make wired-in units harder to deal with.

Solution 2: another approach would be to thread the UnitState to all places
where a UnitId is pretty-printed and to retrieve the information from the
UnitState only when needed. It would mean that UnitId couldn't have an
Outputable instance as it would need an additional UnitState parameter to be
printed. It means that many other types couldn't have an Outputable instance
either: Unit, Module, Name, InstEnv, etc. Too many to make this solution
feasible.

Solution 3: the approach we use is a compromise between solutions 0 and 2: the
appropriate UnitState has to be threaded close enough to the function generating
the SDoc so that the latter can use `pprWithUnitState` to set the UnitState to
fetch information from. However the UnitState doesn't have to be threaded
explicitly all the way down to the point where the UnitId itself is printed:
instead the Outputable instance of UnitId fetches the "sdocUnitIdForUser"
field in the SDocContext to pretty-print.

   1. We can still have Outputable instances for common types (Module, Unit,
      Name, etc.)

   2. End-users don't have to pass a UnitState (via a DynFlags) to print a SDoc.

   3. By default "sdocUnitIdForUser" prints the UnitId hash. In case of a bug
      (i.e. GHC doesn't correctly call `pprWithUnitState` before pretty-printing a
      UnitId), that's what will be shown to the user so it's no big deal.


Note [VirtUnit to RealUnit improvement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Over the course of instantiating VirtUnits on the fly while typechecking an
indefinite library, we may end up with a fully instantiated VirtUnit. I.e.
one that could be compiled and installed in the database. During
type-checking we generate a virtual UnitId for it, say "abc".

Now the question is: do we have a matching installed unit in the database?
Suppose we have one with UnitId "xyz" (provided by Cabal so we don't know how
to generate it). The trouble is that if both units end up being used in the
same type-checking session, their names won't match (e.g. "abc:M.X" vs
"xyz:M.X").

As we want them to match we just replace the virtual unit with the installed
one: for some reason this is called "improvement".

There is one last niggle: improvement based on the unit database means
that we might end up developing on a unit that is not transitively
depended upon by the units the user specified directly via command line
flags.  This could lead to strange and difficult to understand bugs if those
instantiations are out of date.  The solution is to only improve a
unit id if the new unit id is part of the 'preloadClosure'; i.e., the
closure of all the units which were explicitly specified.

Note [Representation of module/name variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In our ICFP'16, we use <A> to represent module holes, and {A.T} to represent
name holes.  This could have been represented by adding some new cases
to the core data types, but this would have made the existing 'moduleName'
and 'moduleUnit' partial, which would have required a lot of modifications
to existing code.

Instead, we use a fake "hole" unit:

     <A>   ===> hole:A
     {A.T} ===> hole:A.T

This encoding is quite convenient, but it is also a bit dangerous too,
because if you have a 'hole:A' you need to know if it's actually a
'Module' or just a module stored in a 'Name'; these two cases must be
treated differently when doing substitutions.  'renameHoleModule'
and 'renameHoleUnit' assume they are NOT operating on a
'Name'; 'NameShape' handles name substitutions exclusively.

-}
