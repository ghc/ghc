# PLFH starting prompt

Build *Programming Language Foundations in Haskell* (PLFH) as a yardstick for this fork’s Dependent Haskell implementation. Follow *Programming Language Foundations in Agda*, edition 19.08. For each theorem, ask whether a Haskell programmer can state it and prove it with the same hypotheses. A small example that the fork cannot express is a valuable result.

Gemini 4 Neon owns the book and its proofs. Work in `/home/tritlo/Code/PLFH`. Edit chapters under `content/` and keep the theorem ledger in `content/Status.md`. The compiler team owns GHC and HaskLib in `/home/tritlo/Code/ghc` on `dependent-haskell`. The book's extraction script copies the Haskell modules and reproducing examples into that checkout. [Book repository](https://github.com/Tritlo/PLFH) · [Extraction and replay instructions](https://github.com/Tritlo/PLFH#quick-start)

Read `/home/tritlo/Code/ghc/PLFH-NOTES.md` before starting and after each verified batch. Write to that same file from either checkout. Give the theorem, a minimal example, the compiler revision, the replay command and the diagnostic. The compiler team replies under the entry. Rerun the original example when a fix is available. [Shared notebook](../../../PLFH-NOTES.md)

In batch 24, **Properties** checks substitution typing and preservation. **DeBruijn** checks progress for all seven constructors of a closed intrinsically typed term. **More** extends the language with products, sums and lists. Its list case substitutes two values. Check the book's ledger and the latest compiler reply before choosing another theorem. [Batch 24 ledger](https://github.com/Tritlo/PLFH/blob/2fd9b9a5a832bcb4ef91f2c6bd69e703bb084c7f/content/Status.md)

## Original acceptance example

1. Read the definition of addition in **Naturals** and the theorem `+-assoc` in **Induction**. Compare the recursion on the first argument with HaskLib’s ordinary Haskell function `plus`. Its `plusZero` lemma already proves right identity by induction. Record that as reuse. [Naturals](https://plfa.inf.ed.ac.uk/19.08/Naturals/) · [Induction source at v19.08](https://github.com/plfa/plfa.github.io/blob/63fcbbe25ec3686ff4c6df2c5b58a96b3a2054f3/src/plfa/part1/Induction.lagda.md) · [HaskLib.Size](../hasklib/HaskLib/Size.hs)
2. Try the following statement. Record whether this exact spelling checks.

   ```haskell
   plusAssoc :: foreach (m :: Size) -> foreach (n :: Size) -> foreach (p :: Size)
             -> plus (plus m n) p :~: plus m (plus n p)
   ```

   Prove it by cases on `m`, using the induction hypothesis in the successor case. Have a second module import the theorem and use it at unknown inputs. As a negative control, try to prove zero equal to its successor. Check that GHC rejects the equality itself. Record the exact command and diagnostic.
3. Put the runnable command in a short PLFH README and the result in the notebook. If the direct proof gets stuck, save the smallest failing module with its intended type. Record any singleton or type-family workaround separately. Continue with independent lemmas while the compiler team investigates.

## What to measure next

Track declarations and the exercises you attempt by chapter. Distinguish a checked direct port, reuse from HaskLib, a workaround and a blocker. Record the upstream declaration, Haskell module and compiler evidence. Construct each proof without holes, deferred errors, `undefined`, circular proof bindings or unsafe coercions. Runtime examples can illustrate a theorem. Use an actual checked proof to establish it for arbitrary finite inputs. State where the Agda argument relies on totality.

After arithmetic, try an indexed relation, then equality and lists. The larger target is **Lambda** and **Properties**. Represent typing and one-step reduction, then prove progress for closed well-typed terms and preservation across a step. Keep **DeBruijn** as a separate milestone for intrinsically typed terms. Record any change to a theorem’s quantifiers, premises or conclusion. [Edition contents](https://plfa.inf.ed.ac.uk/19.08/) · [Properties](https://plfa.inf.ed.ac.uk/19.08/Properties/) · [DeBruijn](https://plfa.inf.ed.ac.uk/19.08/DeBruijn/)

The reference edition is Philip Wadler and Wen Kokke, *Programming Language Foundations in Agda* (2019), tag `v19.08`, commit `63fcbbe25ec3686ff4c6df2c5b58a96b3a2054f3`. Credit the book and chapter in adapted material, link the CC BY 4.0 licence, and identify the changes made in translation. [Tagged source](https://github.com/plfa/plfa.github.io/tree/v19.08) · [Edition citation](https://github.com/plfa/plfa.github.io/blob/v19.08/src/plfa/citing.md) · [Licence](https://creativecommons.org/licenses/by/4.0/)
