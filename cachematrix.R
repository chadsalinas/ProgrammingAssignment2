##
# File: cachematrix.R
# ----------------------- 
# Inverts a matrix and caches result.
#
#  Author: Chad Salinas
#  e-mail: chad.salinas@gmail.com
#  Version: 6/25/2016
#  Class: Johns Hopkins Coursera R Programming
#  Assignment 2: Lexical Scoping
##

## Function: makeCacheMatrix
#  -------------------------
#  makeCacheMatrix exposes 4 helper functions:
#  1.  User can set a matrix by passing in a matrix
#  2.  User can get the current matrix
#  3.  User can explicitly set a matrix as an inverse
#  4.  User can get the inverse of the current matrix
#  nb: not every square matrix has an inverse
##
makeCacheMatrix <- function(a = matrix()) {
        m <- NULL
        # assume l is list returned from l <- makeCacheMatrix()
        
        #  set Matrix from returned list of functions
        #  usage: l$setMatrix(z <- matrix(1:4, 2, 2))
        setMatrix <- function(b) {
                a <<- b        ## cache current matrix
                m <<- NULL     ## initialize cache
        }
        
        #  get Matrix from returned list of functions
        #  usage: l$getMatrix()
        getMatrix <- function() a
        
        #  Explicitly sets a matrix where semantics are that
        #  this matrix is an inverse
        #  usage: l$setInverse(h)
        #  where h is a matrix
        #  Cache the result
        setInverse <- function(invMat) m <<- invMat
        
        # get inverted matrix from cache only if it exists there already
        getInverse <- function() m
        
        #  return list of functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse= getInverse)
}

## Function: cacheSolve
#  --------------------
#  cacheSolve checks to see whether an inverse matrix has already
#  been cached. If so, it is returned. If not, the list passed in
#  using its helper functions to get an existing matrix and apply
#  solve() to it, set it and return it. Cached result's scope survives 
#  the call to these getter and setter functions.
#  Usage: cacheSolve(l) 
#  where p is a list of functions returned from makeCacheMatrix() 
#  i.e. l <- makeCacheMatrix(matrix(6:9, 2, 2))
#
#  Once cached, a subsequent call to cacheSolve(l) will display a 
#  message that result is coming from cache.
##
cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
        m <- a$getInverse() 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- a$getMatrix()
        # primitive inverse using solve() occurs here
        m <- solve(data, ...)
        a$setInverse(m)
        m
}
