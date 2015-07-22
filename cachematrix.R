#makeCacheMatrix function - This function creates a matrix object that can cache (save) its inverse
#cacheSolve function - This function computes the inverse of the matrix object returned by makeCacheMatrix (see above). If the inverse has already been calculated and has not been changed, then the cacheSolve should retrieve the inverse from the cache


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #sets the value of m to NULL so that there is a default
    set <- function(y) { #beginning of set function
        x <<- y # caches the inputed matrix so that that cacheSolve (below) can check for changes
        m <<- NULL #sets m (matrix inverse) to NULL in set function
    }
    get <-function() x #start of get function
    setinverse <- function(inverse) m <<- inverse #sets inverse and tell m to inverse
    getinverse <- function() m # gets inverse
    list(set = set, get = get, #sets list for setinverse and get inverse
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    m <- x$getinverse() #checks for already calculated inverse and gets it
    if(!is.null(m)) { #checks if cacheSolve has already been run
        message("getting cached data") #message to show if it has already been run
       	return(m) #returns m if it has already been run
    }
	#IF NOT
    data <- x$get() #data to look for
    m <- inverse(data, ...) #compute the inverse of the data inverse matrix
    x$setinverse(m) #run set inverse on the inverse in the cache
    m #return the inverse
}
