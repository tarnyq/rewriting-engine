
Rewrite Rule:
- Matching pattern for the state and optionally a predicate ("requires
  clause" in K).
- It "matches" or "applies" for a state when the LHS matches that state and
  any additional conditions for the rule are met.
- The result of applying a rewrite is its RHS with the substitution
  applied.
- (Note that "matching" here is different from case matching in Haskell
  where a case might match but evaluate to "no result" (typically `Nothing`
  or `[]`).)

Rewriting system.
- A set of rewrite rules. Each can individually match/apply; how one deals
  with the results depends on how you step.
- Given an initial state, a rewriting system induces a (possibly infinite)
  graph of states, where the each child node is the result of applying some
  rule in the system. This is a model of the rewriting system called a
  _Kripke structure;_ a connection of models to the structure gives a
  _logic._
- (There are "lonely states" that are just points, states that cannot be
  produced by any rewrite the rewriting system can apply.)
- Any valid initial states you designate, and all states that can be
  produced by the rewriting system from those states, are the set of
  "reachable" states.
- The result of "evaluating" the initial state is a leaf, or set of leafs
  in this tree.

Steps:
- A _rewrite_ is the application of one rule.
- A _step_ is the application of _all_ the rules from the rewriting system
  that an execution strategy cares to apply while never applying the same
  rule more than once, to produce zero or more new states. (The input set
  of states may be larger than just one state.)
- Note that a step may not apply all the rules in a rewriting system: an
  execution strategy may decide the step is complete once e.g. a single
  rule matches.
- An _evaulation_ is repeatedly applying steps until no rules match any of
  the states (giving us only _terminal states_) or until it decides to stop
  for other reasons, e.g., a step limit being reached.

Thus, the _strategy_ one uses for execution provides the _next_ function,
and _eval_ applies _next_ and then decides when to stop based on either
having only terminal states in the input or because it's taken as many
steps as it cares to take.

The types of executions we want to do give us some or all of the the leafs
of the trees.
- For concrete execution we would use e.g. a first-match evaluation
  strategy and sequence that one state through evals until no rules match,
  producing the terminal state.
- For model checking we might want to produce (possibly in a lazy way) the
  entire tree and check for properties of each state (intermediate or
  terminal), pruning by dropping duplicate states.
