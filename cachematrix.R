## This script contains two functions that together allow for the result
## of a matrix inversion calculation to be reused by caching the result the 
## first time it is calculated

## this function takes a matrix and returns a list containing methods to 
## store/retrieve the matrix and its inverse. note that there is no
## guarantee that the matrix the getInverse function returns is actually
## the inverse of the original matrix - that logic is controlled by the
## cacheSolve function.

makeCacheMatrix <- function(originalMatrix = matrix()) {
	inverseMatrix <- NULL
	set <- function(newMatrix){
		originalMatrix <<- newMatrix
		inverseMatrix <<- NULL
	}
	get <- function() originalMatrix
	setInverse <- function(newInverse) inverseMatrix <<- newInverse
	getInverse <- function() inverseMatrix
	list( set = set, 
		get = get, 
		setInverse = setInverse,
		getInverse = getInverse
	)
}


## the cacheSolve function takes a 'matrix' argument as returned from the
## makeCacheMatrix function above and calculates the inverse of the matrix.
## the first time the function is called, the inverse is calculated and the 
## result is cached. subsequent calls to the function with the same matrix
## will return the cached result rather than recalculating.

cacheSolve <- function(m, ...) {
      ## returns a matrix that is the inverse of the matrix contained in 'm'

	inverse <- m$getInverse()
	
	if(is.null(inverse)){
		message("calculating matrix inverse")

		originalMatrix <- m$get()
		inverse <- solve(originalMatrix)
		m$setInverse(inverse)
	}
	else{
		message("using cached inverse")
 	}
	
	inverse		
}
