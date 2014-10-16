## The first function, makeCacheMatrix, defines
## an object that can store on its environment
## a matrix and the variable "i", which is designed 
## to store the inverse of the matrix.

## The second function, "cacheSolve", returns 
## the inverse  of a "matrix" object as constructed by
## "makeCacheMatrix" by first, 
## checking the existence of a previously
## calculated one stored in the environment.
## If it finds it, it simply uses it.
## If don't, it returns one by calling the "solve" 
## function and stores the result at the environment 
## corresponding to the object.


## Creates an special "matrix" object that can store on
## its environment the inverse as calculated by
## the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get, setInverse = setInverse,
		 getInverse = getInverse)
}


## Returns the inverse of a "matrix" object as defined by
## "makeCacheMatrix".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
		
		if(!is.null(i)){
			message("retrieving cached data")
			return(i)
		}
		
		mat <- x$get()
		i <- solve(mat, ...)
		x$setInverse(i)
		i
}
