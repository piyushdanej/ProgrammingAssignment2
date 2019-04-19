## Loading matrixcalc to check if matrix is singular
library("matrixcalc")


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a fu nction that creates a "vector" ,
## which is a list for all the getters and setters for the matrix and its cache.
## It basically makes a list of functions(getters and setters).


makeCacheMatrix <- function(x = matrix()) {
    cm <- NULL
    
    set <- function(y){
      x <<- y
    }
    get <- function() x
    setCache <- function(cachedMatrix) {
        cm <<- cachedMatrix
    }
    ## to Check if matrix is invertible or not .
    ## if it is , then set cache to original Matrix
    if(is.singular.matrix(x)){
        setCache(x)
    }
    
    getCache <- function() cm
    list(set = set , get = get , setCache = setCache , getCache = getCache)

}


## argument : makeCacheMatrix(x) where x is a matrix
## cacheSolve inverts the matrix. after checking if its already inverted or not .
## If its already inverted , it returns the matrix.
## else it inverts the matrix and then returns it.

cacheSolve <- function(x, ...) {
    cachedMatrix <- x$getCache()
    originalMatrix <- x$get()
    calculatedInverse <- c()
    
    #checking if matrix is already cached, x$getCache will be NULL if not cached
    if (!is.null(cachedMatrix) && (cachedMatrix == originalMatrix)){
        
        #return with inversed cached matrix
        return(cachedMatrix)
    }    
    else{
        calculatedInverse <-solve(originalMatrix)
    }
    
    x$setCache(calculatedInverse)
    
    ## Return a matrix that is the inverse of 'x'
    calculatedInverse   
}
