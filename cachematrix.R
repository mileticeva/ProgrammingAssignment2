## I will write a pair of functions for caching an inverse of a matrix. 
## Once it`s cached, every next time we need inverse of the same matrix,
## it will be gotten from the cache, without recomputation.


## makeCacheMatrix function creates a special "matrix" object,
## which is actually a list containing a functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse of the matrix
## and get the value of the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
inversion <- NULL

set <- function(y, ...) {
	x <<- y
	inversion <<- NULL
}

get <- function() x
setinversion <- function(solve,...) inversion <<- solve
getinversion <- function() inversion

list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


## cacheSolve function calculates the inverse of special "matrix", created with makeChacheMatrix
## funcion, but it first checks if inverse of that matrix is already calculated and cached.
## If so, it founds data in cache, prints message "getting cached data" and returns inverse of
## matrix. Otherwise, it calculates the inverse of matrix and prints it.

cacheSolve <- function(x, ...) {
        inversion <- x$getinversion()

if(!is.null(inversion)) {
message("getting cached data")
return(inversion)
}

data <- x$get()
inversion <- solve(data,...)
x$setinversion(inversion)
inversion
}
