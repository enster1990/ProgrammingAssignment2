## Caching the Inverse of a Matrix

## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(matrix = matrix()) {
  
    inverse <- NULL
    
    ## Setter of the matrix
    setMatrix <- function(m) {
        matrix <<- m
        inverse <<- NULL
    }
    
    ## Getter of the matrix
    getMatrix <- function() {
        matrix
    }
    
    ## Setter of the inverse
    setInverse <- function(i) {
        inverse <<- i
    }
    
    ## Getter of the inverse
    getInverse <- function() {
        inverse
    }
    
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## Compute the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(m, ...) {
  
    inverse <- m$getInverse()
    
    if( !is.null(inverse) ) {
        print("Cached inverse exists, just return it")
        return(inverse)
    }
    
    print("Cahched inverse does not exist, computing ...")
    matrix <- m$getMatrix()
    inverse <- solve(matrix) %*% matrix
    m$setInverse(inverse)
    return(inverse)
}