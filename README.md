# constraint-algebras
Algebras for qualitative reasoning about time, space, etc.

NOTE: This code was written in the early 1990s and has only been partially tested since then using Armed Bear Common Lisp (ABCL).  The portions tested appeared to work OK.  However, it does not appear to work using Steel Bank Common Lisp (SBCL).

The code here was used to write the following paper (a copy is in the "docs" folder of this repository):

<b>"Intervals, Points, and Branching Time"</b>, Alfred J. Reich, Time-94: An International Workshop on Temporal Representation and Reasoning, 1994.

ABSTRACT: This paper extends Allen’s interval algebra to include points and either left or right-branching time. The branching time algebras each contain 24 relations: Allen’s original 13 relations, 5 more relations resulting from the inclusion of points, and 6 relations because of the inclusion of branching time. In this paper, I also present:(1) A technique for automatically deriving algebras of relations;(2) a new way of representing temporal constraint networks—the constraint matrix; and (3) a new way of performing constraint propagation—via constraint matrix multiplication.
