## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix
##This function creates a matrix object which caches it's inverse to avoid
#unnecessarily recalculating the inverse 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(newVal){      #function to change matrix value 
		x <<- newVal
		inv <<- NULL
	}
	get <- function() x     #function to get the matrix associated with our object
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list( set = set, get = get,  #build our object 
			setInv = setInv, 
			getInv = getInv)
}


##cacheSolve 
#This function calculates the inverse matrix of our 
#matrix object created by makeCacheMatrix. 
#If the matrix has not changed and the inverse has already been 
#calculated, the cached inverse is returned. Otherwise the inverse is 
#calculated.  
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv  <- x$getInv()
	if(!is.null(inv)){                #if we have a cached value 
		message("getting cached data")
		return(inv)                   #return the cached value
	}
	data <- x$get()
	inv <- solve(data, ...)     #calculate the inverse if needed
	x$setInv(inv)
	inv
}
