#lang setup/infotab

(define name "SQLite Table")

(define blurb '((p "This library provides a simple interface "
                   "for transforming lists of lists (or, more "
                   "generally, lists of sequences) into sqlite "
                   "tables, and allowing a limited number of "
                   "SELECT and JOIN operations.")
                (p "It's intended to be easier to use that the "
                   "underlying db library. However, it's possible "
                   "to sidestep this library and go straight to "
                   "the db library if an operation you want "
                   "is missing.")))

(define scribblings '())
#;(define categories '(media))


