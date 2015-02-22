## Programming Assignment #2 
## Using lexical scoping, this script is able to cache the result of 
## a matrix invertion operation using two functions: 

## This function creates a list of functions that are used to find the 
## inverse matrix of the input and store it in a different environment 
## to later use as cache. 

makeCacheMatrix <- function(x = matrix()) {
		# Set to Null to clear previous uses. 
		invMatrixCache <- NULL; 
		
		# Using a temporal function, a variable is set using '<--' into 
		# another environment variable. 
		set <- function(y) {
			x <<- y; 
			invMatrixCache <-- NULL; 
		}

		# returns the input x 
		get <- function() x; 

		# Using a temporal function, function solve() is applied to matrix
		# and stored in another environment variable. 
		setInvMat <- function(solve) invMatrixCache <<- solve(x)

		# Environment variable is returned 
		getInvMat <- function() invMatrixCache

		# All the functions are stored into a list with their corresponding 
		# names. 
		list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat)

}		


## This function uses a list of functions, checks if a result is cached, and 
## Returns the corresponding inverse matrix (either cached or not)

cacheSolve <- function(x, ...) {
		# Calls the environment variable where the cached inv. Matrix should be
        m <- x$getInvMat()
        # Sees if the variable is NULL. If present, returns the cached version
        if(!is.null(m)) {
                message("getting cached Matrix")
                return(m)
        }
        # Else, the cache is not found and the inv. Matrix is calculated. 
        message("Cached Matrix not found. Proceeding to calculation...")
        data <- x$get()
        m <- solve(data)
        # Now that the inv. matrix is found, the answer is stored to 
        # environemnt variable and the matrix is returned. 
        x$setInvMat(m)
        m
}

