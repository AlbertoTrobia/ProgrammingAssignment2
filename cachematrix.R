## The following functions are used in order to cache the inverse of a matrix 
## The matrix is supposed to be always invertible
## These functions allow the researcher to save time and computations, 
## above all when dealing with big matrices.

## The following function simply creates and lists four functions, 
## in order to set, get, set the inverse, and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	matinv <- NULL
	set <- function(y) { x <<- y 
      matinv <<- NULL }
	get <- function() x
	setInversa <- function(Inversa) matinv <<- Inversa
	getInversa <- function() matinv
	list(set = set, 
           get = get, 
           setInversa = setInversa, 
           getInversa = getInversa)
}      

## The following function calculates the inverse of a matrix. 
## If the calculation has been already carried out, 
## it just gets the result from the cache. 
## Otherwise, it stores the new computation in memory.  

cacheSolve <- function(x, ...) {
	matinv <- x$getInversa()
	if(!is.null(matinv)){
        	message("getting cached data")
		return(matinv)
      }
	data <- x$get()
	matinv <- solve(data)
      x$setInversa(matinv) 
      matinv  
}
