#lang scribble/manual

@title{SQLite Table}

@(require (for-label "main.rkt"))

@defmodule[sqlite-table]{

This library is intended to simplify the process of transforming
a list-of-lists or list-of-vectors or list-of-sequences representation
of data into a SQLite table, and then to perform database operations
like SELECT and JOIN on them.

If the operation you're looking for is missing, you can always sidestep
the library and operate on the generated tables directly using the db
library.

Note on storage: This library stores all of its permanent tables in
@filepath{/tmp/racket-tables.sqlite}. That's just hard-coded. Yikes!

@defproc[(make-table [column-labels (list-of string?)]
                      [data (sequence/c (sequence/c any/c))]
                      [#:permanent permanent permanent?]
                      [#:use-existing use-existing? boolean?]) table?]{
Creates a sqlite table from the given data, using the given column labels.
If a string is supplied as the @racket[#:permanent] argument, the table
is created as a permanent table, in the library's storage file. If the
symbol @racket['_] is provided as a permanent table name, it's created
as a permanent table with a name that is a number randomly chosen between
0 and @racket[1e+10]. If no @racket[#:permanent] argument is supplied,
the table is created as a temporary table with a name assigned from a
sequential pool.

If the @racket[#:use-existing] argument is present and not @racket[#f], then
the call will simply return an existing table. This is useful if, for instance,
your code is written in a declarative way, and you don't want the program to
re-generate every table every time it's run. Please note that no checking is
performed, so if the inputs to the make-table call change, the use of
@racket[#:use-existing] could lead to bugs.


 Example of @racket[make-table]'s use:

 @racketblock[
 (make-table '(student a b)
             '(#("bob" 3 8)
               #("annie" 4 9)
               #("bob" 6 12)))]}

@defproc[(find-table (name string?)) table?]{Given a table name, return
 the table. Names matching the regular expression
  @racket[#px"^temp_[0-9]+$"] are looked up as temporary tables.}

@defproc[(table-size (table table?)) natural?]{Returns the number of
 rows in a table.}

@defproc[(table-select (table table?)
                       (cols (listof colspec?))
                       [#:where where-constraints (listof where-clause?)]
                       [#:group-by group-by-columns (listof symbol?)])
         (sequence/c (vectorf any/c))]{Given a table and a list of column
 specifications (including aggregate specifications such as
  @racket['(count)]), perform a SELECT. There is very limited support
  for WHERE constraints, specifically using the @racket[<], @racket[<=], and
  @racket[=] operators, and for GROUP BY, allowing the naming of columns
  on which to group.

  Here's an example, in the form of a pair of test cases:

@racketblock[
 (define t1
   (make-table '(a b zagbar quux)
               (list (list 3 4 5 "p")
                     (list 8 87 2 "q")
                     (list 1 88 2 "q")
                     (list 1 87 2 "q"))))

 (check-equal? (table-select t1 '(a (min b)) #:group-by '(a))
               '(#(1 87)
                 #(3 4)
                 #(8 87)))

 (check-equal? (table-select t1 '(b) #:where '((< 2 a)))
               '(#(4)
                 #(87)))
 ]
 }

 @defproc[(inner-join (table-a table?)
                      (table-b table?)
                      (join-cols (listof symbol?))
                      [#:permanent permanent permanent?]
                      [#:use-existing use-existing? boolean?])
          table?]{
 Creates a new table (actually a VIEW) by performing an inner-join on
 the two tables, using the specified columns. The @racket[#:permanent]
 and @racket[#:use-existing] arguments are treated as they are in
 @racket[make-table].
 }

 @defproc[(in-table-column (table table?) (column symbol?)) (sequence/c any/c)]{Given
 a table and a column name, produces a sequence of the unique values appearing in
 that column of the table.  This is useful in producing grouped results. For example:

 @racketblock[
 (for/list ([team (in-table-column table 'team)])
  (table-select table '(student-id) #:where `((= team-name ,team))))]}

@defproc[(table? (t any/c)) boolean?]{Determines whether a value is a table.
 Currently, tables are represented simply as strings, specifically the name
 of the table in the database.}

 Undocumented functions:

 @verbatim{
(provide
 (contract-out [make-table-from-select
                (->* (table? (listof colspec?))
                     (#:where any/c
                      #:group-by (listof symbol?)
                      #:permanent permanent?
                      #:use-existing boolean?)
                     table?)]
               [in-table-column (-> table?
                                    symbol?
                                    (sequence/c any/c))]
               [table-ref (-> table? symbol? symbol? any/c
                              (sequence/c any/c))]
               [table-ref1 (->* (table? symbol? symbol? any/c)
                                (any/c)
                                any/c)]
               [natural-join (->* (table? table?)
                                  (#:permanent permanent?
                                   #:use-existing boolean?)
                                  table?)]
               [back-door/rows (-> string? boolean? any/c)]))
 }
 
}