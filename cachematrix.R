## These functions permits to store a matrix and its inverse
## in a single object. 
## This object is a list of function that permits to retrive (get) 
## and calculate (set) the values.
##
## The code is highly based on the example given in the R Programming Course
## by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
## available in the original README.md of the git repository :
## https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md
## I have basically only changed the name of the functions and variables,
## switched the function 'mean' with 'solve', and added all comments.

## The makeCacheMatrix Function creates a list of function 
## that will permit to store and retrieve the matrix and its inverse.
##
## The value of the matrix and its inverse are stored inside the regular
## argument 'x' and in the local variable 'i' respectively. 
## These variable resides within the environment of the functions 
## returned as a list by the function.
## 
## Example:
##
## m <- matrix(c(2,0,0,2),nrow=2,ncol=2) ## Creates a simple diagonal 2x2 matrix
## m
##     [,1] [,2]
##[1,]    2    0
##[2,]    0    2
##
## M <- makeCacheMatrix(m) ## Creates the Chache Matrix object and store in the variable 'M' (a list)
## cacheSolve(M)           ## Calculates the Inverse Matrix and (ivisibly) store the value
##     [,1] [,2]           ##  in the 'i' local variable present in the environment of 
##[1,]  0.5  0.0           ##  functions, elements of 'M'
##[2,]  0.0  0.5
##
## env <- environment(M$set)  ## Retrives the enviromnet of the functions in the 'M' list
## env$x                      ## It can be shown that inside this environmente the 'x' and the 
##     [,1] [,2]              ##  'i' variable are presents and contains the data.
##[1,]    2    0
##[2,]    0    2
##
## env$i
##     [,1] [,2]
##[1,]  0.5  0.0
##[2,]  0.0  0.5

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        ## This function permits to create an empy Cache Matrix object
        ## and subsequently add the matrix data, or to reset a previously
        ## created object.
        ## The "superassignment" operator has to be used because both
        ## 'x' and 'i' are free variables for this function, but 
        ## data has to be stored in its parent environment.
        x <<- y
        i <<- NULL
    }
    get <- function() {
        ## This function permit to obtain the original matrix
        ## stored in the 'x' variable in the parent environment.
        return (x)
    }
    set_inv <- function(inv) {
        ## This Function permits to record the value of the inverse matrix
        ## inside the variable 'i'. (the "superassignment" operator has to be)
        ## used beacuse the 'i' whould be a free variable but the function has to 
        ## store the value in its parent environment)
        i <<- inv
    }
    get_inv <- function() {
        ## This function returns the value of the inverse matrix
        ## that has been stored in the variable 'i' 
        # (in its parent environment)
        return (i)
    }
    return(list(set = set, get = get, set_inv = set_inv, get_inv = get_inv))
}


## The cacheSolve function takes as argument the list of function
## created by the makeCacheMatrix function. 
## If the variable 'i' is still "NULL" the function calculates the 
## inverse of matrix x then store the value using the function 
## set_inv() and returns it. 
## If the inverse has been already calculated (and then stored) it 
## returns the stored value.
##
## For efficency resons it is first checked if 'i' is not "NULL", as
## this will be always TRUE, with the only exception of the first time
## the function is called.
##

cacheSolve <- function(x, ...) {
    i <- x$get_inv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    else{
        data <- x$get()
        i <- solve(data, ...)
        x$set_inv(i)
        return(i)
    }
}
