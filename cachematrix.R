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

## following are utlity functions that should go into a package.  
## After this course is finished I will probably implement it
## for real.

##----------------------------------
spin_lock_contention <- function() {

## wait until the object lock clears before allowing
## MUTATORS
## access to SHARED memory

    while (global$locked() {
        Sys.sleep(1.5)          ## snooze for 1.5 seconds
    }

}



## The idea of this code is too provide the same level of GLOBAL
## access to "CACHED" data but in a more formal method that follows
## an OOP paradigm.  That is, all data that is SHARED is PRIVATE
## (sounds like a contradiction) but setters() and getter() are
## provided to the application level.  I intend to submit this 
## (working) as my SHARED memory architecture.

## The concept is sort of the same as what was presented as
## the traditional method of using the superassignment operator.
## The old way had individual bits of code in functions DIRECTLY
## accessing out of scope variables.  Messy.  Given the way R
## ALREADY manages it's lexical scope, we can allocate memory
## by way of an encapsulated set of methods and properties inside
## a clusure (function) in the global environment WITHOUT
## bending any rules.  This OOD/OOP method has a number of
## benifits.
##
## 1. first and formost, brings OOP structure into the code
## 2. encapsulates the code with our choice of privacy. Methods
##    and properties can be Public or Private.
## 3. being encapsulated we can implement some spin lock contention
##    routines to stop multiple processes changing shared memory
##    concurrently.
## 4. for secure deployments, we can implement function to function
##    access security in the form of shared-secret keys required
## 5. to access the accessors and mutators.  The trivial routine
##    implemented in this offering uses an MD5 hash of THIS source
##    code as an inter-process shared secret.


    ## -------------------------
    get_global <- function() {

        ## Private properties
        ## can ONLY be accessed by accessors and mutators in here
        
        locked                  <- FALSE
        global_matrix           <- NULL 
        global_inverse_matrix   <- NULL 

        ## Private methods
        spin_lock   <- spin_lock_contention() 
        check_id    <- security_manager()

        ## Public methods
        list(
            is_locked       = function() { locked },
            lock            = function() { locked <<- TRUE },
            unlock          = function() { locked <<- FALSE },
            build_matrix    = function(x){ global_matrix <<- x },
            gsolve          = function() { global_inverse_matrix <<- solve(global_matrix) },
        )
    }



##-----------------------------------------
makeCacheMatrix <- function(x = matrix()) {

## if the GLOBAL matrix isn't in memory somewhere, initiate the build.
## this used to be some complicated munging of memory states and a collection
## of yucky things like collections where the functions and the variables
## had the same identifiers, on-the-fly polymorphism and abrupt context
## switches.  No no no...  KISS.

    x <- matrix(trunc(rnorm(512*512)*100), 512,512)
    global$build_matrix(x)
    
}


## ------------------------------
cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

    mat -> global$inverse_return(x, ...)
    if (! is.null(mat)) {
        return(mat)
    } else {
        global$gsolve()
        mat -> global$inverse_return()
    }
}

