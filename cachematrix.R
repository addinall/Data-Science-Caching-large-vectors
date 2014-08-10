## CAPTAIN SLOG
## vim: set expandtab tabstop=4 shiftwidth=4 autoindent smartindent:
## File         : cachematrix.R
## System       : Assignment 2 (Peer review)
## Date         : 10/08/2014
## Author       : Mark Addinall
## Synopsis     : This file is part of the course work
##                assignments for the Johns Hopkins
##                series of Data Science units.
##                This unit is R Programming
##
## This function demonstrates the ability of the R
## programming environment to store the value of a 
## variable outside of normal lexical scope.
## The operator is
##  <<-
## and is generally referred to as the "superassignment
## operator.  It was introduced in the dim dark past to
## 'get over' R's lack of a POINTER type, that is, a
## function is always given the argument by VALUE, not
## by reference.  
## 
## What this operator can do is modify variables that are enclosed ONE
## step up the ladder.  i.e. the operator can modify
## a variable in the environment spacer of it's PARENT.
##
## If the variable is not found, then the search takes place
## one more environmental step up until it reaches the
## GlobalEnvironment.  If the variable is not found
## there, it is created on the fly!
##
## Eeeeekkkk.  It is hard to think of a worse function
## to build into a loosely-type language that implements
## ad-hoc polymorphism.  Anyway, in this assignment we
## are asked to implement it, and so we shall.  If the gentle
## reader will leave with the notion that <<- is EVIL,
## then well and good. A typing error in one of these
## routines could be nightmare on Elm street.
##
## However, that said, given that matrix inversion can be
## very heavy regarding CPU cycles, storing a very large
## set of matrices globally may be worthwhile.  And being
## LARGE, we may not want to be slinging too may copies of
## the thing around via the STACK.  R must keep ALL DATA
## in memory so space is a consideration.
##
## So, bringing in some OOP paradigms into this effort,
## let us see if we can implement a SHARED memory model,
## perhaps for a game written in R or something equally
## bizzare! ;-)
##
## Oh, I read this as well.   
## Google's R Style Guide
##
## YOU HAVE TO BE KIDDING ME!!!  wimpyCaps ???
## biteMe javaWeenies
## and dots are just STOOPID
##
##
## These are the prototype we were asked to demonstrate.
## I am going to do so, but in a rather different way!
##
## ----------------------------------------
##makeCacheMatrix <- function(x = matrix()) {
##
##}
##
##
## -------------------------------------------------
##cacheSolve <- function(x, ...) {
##
## Return a matrix that is the inverse of 'x'

## The idea of this code is too provide the same level of GLOBAL
## access to "CACHED" data but in a more formal method that follows
## an OOP paradigm.  That is, all data that is SHARED is PRIVATE
## (sounds like a contradiction) but setters() and getter() are
## provided to the application level.  I intend to submit this 
## (working) as my SHARED memory architecture.

    ## -------------------------
    get_global <- function() {
        counter <- 1
        locked <- FALSE
        global_matrix <- matrix(c(1:16),nrow=4,ncol=4) 
        global_inverse_matrix <- matrix(c(16:1),nrow=1,ncol=1) 
        list(

            is_locked       = function() { locked },
            lock            = function() { locked<<-TRUE },
            unlock          = function() { locked<<-FALSE },
            visit           = function() { counter <<- counter + 1 },
            visits,         = function() { counter },
            gsolve          = function() { global_inverse_matrix <<- solve(global_matrix) },
            gsolve_return   = function(x){ return( solve( x )) },
            cache_return    = function() { return( global_matrix ) },
            inverse_return  = function() { return( global_inverse_matrix ) }
        )
    }

    global <- __get_global()


##-----------------------------------------
makeCacheMatrix <- function(x = matrix()) {

## if the GLOBAL matrix isn't in memory somewhere, initiate the build.
## this used to be some complicated munging of memory states and a collection
## of yucky things like collections where the functions and the variables
## had the same identifiers, on-the-fly polymorphism and abrupt context
## switches.  No no no...  KISS.

    
}


## ------------------------------
cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

}
