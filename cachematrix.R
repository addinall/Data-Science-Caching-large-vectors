## CAPTAIN SLOG
## vim: set expandtab tabstop=4 shiftwidth=4 autoindent smartindent:
## File         : cachematrix.R
## System       : Assignment 2
## Date         : 10/08/2014
## Author       : Mark Addinall
## Synopsis     : 
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
##    concurrently.  Not implemented here.
## 4. for secure deployments, we can implement function to function
##    access security in the form of shared-secret keys required
##    (not implemented here)
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

        ## Public methods
        ##
        ## We see here we are still using the "superassignment"
        ## operator, but only within this encapsulation

        list(
            is_locked       = function() { locked },                        ## we can lock out mutators from
            lock            = function() { locked <<- TRUE },               ## changing the properties at
            unlock          = function() { locked <<- FALSE },              ## the same time
                                                                            ## I didn't implement these routines
                                                                            ## mostly because I couldn't be
                                                                            ## bothered, but a spin lock routine
                                                                            ## and a 'changed' flag is desirable
                                                                            ## when dealing with shared memory
                                                                            ## models.
            build_matrix    = function(x){ global_matrix <<- x },
            build_cache     = function() { global_inverse_matrix <<- solve(global_matrix) },
            get_matrix      = function() { return(global_matrix) },         ## in the R memory management, our scope
            get_cache       = function() { return(global_inverse_matrix) }  ## allows us to read a parent's property
        )
    }



##-----------------------------
makeCacheMatrix <- function() {

## if the GLOBAL matrix isn't in memory somewhere, initiate the build.
## this used to be some complicated munging of memory states and a collection
## of yucky things like collections where the functions and the variables
## had the same identifiers

    x <- matrix(trunc(rnorm(128*128)*100), 64, 64)
    global$build_matrix(x)
    global$build_cache() 
}


## -----------------------
cacheSolve <- function() {

## Return a matrix that is the inverse of
## an encapsulated "global"

    mat <- global$get_cache()           ## see if we can access the cache
    if (! is.null(mat)) {               ## yes, it has been built
        return(mat)                     ## send it back
    } else {
        makeCacheMatrix()               ## nope, we need to build it
        mat <- global$get_cache()       ## and fetch it
        return(mat)                     ## and send it back
    }
}


## instance of the class

global <- get_global()

## now in a real implementation we wouldn't use these external functions,
## cachSolve and makeCacheMatrix as the OBJECT encapsulation makes the
## a bit redundant.  I am keeping them in this display only to fufill
## the instructions of the assignment, viz, "write two functions named ...".


external_solved_matrix <- cacheSolve()
print(external_solved_matrix)


## Output
## -------
## [,1]          [,2]          [,3]          [,4]          [,5]
## [1,] -8.139867e-04  9.064369e-04  1.228950e-03 -9.227504e-04 -3.415110e-04
## [2,] -4.138931e-04  3.747649e-04  9.651470e-04 -1.162267e-04 -7.574828e-05
## [3,] -6.227400e-04 -8.624411e-04 -9.502722e-04 -3.514369e-04 -1.593758e-03
## [4,] -1.977356e-04 -9.827919e-04  2.839535e-04 -5.078519e-04  5.495214e-04
## [5,]  1.166939e-03 -6.027444e-04 -5.847597e-05 -3.631432e-04  6.498612e-04
## [6,]  1.019414e-03  7.579872e-04 -4.548037e-04  1.108597e-04  1.094391e-04
## [7,]  2.132940e-04 -1.018296e-03 -7.344055e-04 -6.028637e-04  1.931251e-04
## [8,]  3.830571e-04 -6.087848e-04 -1.384943e-03  6.076651e-04  2.727960e-04
## [9,]  1.266233e-03  6.659556e-05 -1.918067e-03  5.014345e-04 -3.636925e-04
## [10,] -4.305116e-05  5.738193e-04  1.795968e-03 -4.804082e-04  1.244775e-03
## [11,]  1.129045e-04  3.469866e-04 -5.824048e-04  8.793960e-04 -8.012039e-04
## [12,]  3.451353e-05 -2.389702e-04 -3.040551e-04 -4.577838e-06 -1.326849e-04
## [13,]  3.815057e-04 -1.436095e-03 -1.589357e-03 -1.060371e-03  1.216880e-03
## [14,]  1.109201e-03 -1.252530e-03 -7.185095e-04  4.557229e-04  9.585955e-04
## [15,] -1.897208e-04 -7.870040e-04 -1.290015e-03  1.940433e-04 -1.092985e-04
## [16,]  7.911149e-04 -6.918251e-04  9.542870e-04  4.018577e-05  1.564828e-03
## [17,] -2.156861e-04  1.150476e-04  3.054249e-04 -9.123217e-04 -6.457612e-04
## [18,]  7.114839e-05 -4.562517e-04 -9.967754e-04 -6.590759e-04 -2.061481e-04
## [19,]  7.911419e-04 -2.272848e-04 -1.108369e-03 -8.602229e-05  1.696507e-04
## [20,]  5.009933e-06 -1.169807e-03 -1.036298e-03  3.091265e-04  2.529298e-04
## [21,] -2.828555e-04 -8.954812e-04 -5.491653e-04 -7.404511e-04  4.089178e-04
## [22,]  1.273598e-04 -1.085330e-03 -6.103719e-04 -6.471220e-04  1.418210e-03
## [23,] -1.369164e-03  2.937183e-04  1.870766e-03 -5.357426e-05  1.285163e-04
## [24,] -2.147449e-05  9.579228e-04  2.059540e-03 -7.063672e-04  4.257655e-04
## [25,]  2.694775e-04 -1.657460e-03  9.181187e-05 -8.526691e-04  1.266469e-03
## [26,]  3.018420e-04 -2.343691e-05 -8.090516e-04 -1.705478e-04  5.707970e-04
## [27,] -1.354695e-04  2.677266e-04  1.083183e-03 -1.729623e-04  6.095629e-05
## [28,] -7.904088e-04  2.163595e-03  3.279455e-03 -1.893858e-04 -4.502062e-04
## [29,]  2.255672e-04  3.582741e-04  5.390065e-04  6.414334e-04  4.809330e-04
## [30,]  5.426775e-04 -8.429080e-04 -8.324967e-04  4.923037e-05  1.109143e-03
## [31,] -1.565413e-03  1.629057e-04  1.400855e-03 -3.167788e-04 -7.216993e-04


