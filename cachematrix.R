## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { # x = an invertible matrix
        
        i <- NULL       # assigns the value NULL to the inverse matrix 'i'
                        # if inverse matrix i is found to be NULL when cacheSolve is called 
                        # then cacheSolve calculates and caches 'i'
                                         
        setmatrix <- function(y) {            
                x <<- y                 # sets a new value for the matrix 'x' in the enclosing environment
                i <<- NULL              # sets the cached value of the previous inverse matrix back to NULL 
        }
        
        getmatrix <- function() {       
                x                       # auto-prints the value of the matrix 'x'
        }             
        
        setinverse <- function(inverse) {       # setinverse is a function called by cacheSolve
                i <<- inverse                   # sets the value of the inverse matrix in 
        }                                       # the enclosing environment after
         
        
        getinverse <- function(){       # getinverse is a function called by cacheSolve
                i                       # gets the cached value of the inverse matrix
        }
        
                        
        list(setmatrix = setmatrix,     # the list of functions returned. 
             getmatrix = getmatrix,     # cacheSolve calls getmatrix, setinverse & getinverse
             setinverse = setinverse,   
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()   # assigns the cached value of inverse matrix to 'inv'           
        
        if(!is.null(inv)) {     # checks to see if inv has already been calculated or if it is NULL. 
                                # If it is not NULL it returns the inverse matrix value from the cache 
                                # and skips the computation.
                message("getting cached data")
                return(inv)
        }
        
        data <- x$getmatrix()   # if the inverse has not been calculated previously for the current matrix (is NULL)
                                # then getmatrix is called to assign the current cached matrix to 'data'
        
        inv <- solve(data, ...) # the inverse matrix is calculated and assigned to 'inv' 
        
        x$setinverse(inv)       # setinverse is called to set the inverse matrix in the cache
        
        inv                     ## Return a matrix that is the inverse of 'x'
}
