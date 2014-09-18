## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
      ## Return a matrix that is the inverse of the matrix contained in 'm'

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
