### This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# initializing null matrix
        i <- NULL
	
	#Defining set matrix function
	  set <- function(y) {
                x <<- y
                i <<- NULL
        }

	#Defining get matrix function
        get <- function() x

	#defining set and get matrix inverse functions
 	  setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i

	#Creating list
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}

### This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve<- function(x, ...) {
	#Get inverse if available in cache
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse matrix data")
                return(i)
        }

	#Else calculate matrix inverse and set in cache
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
  