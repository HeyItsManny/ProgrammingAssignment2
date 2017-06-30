## Requirement
 # Create a pair of functions to create and cache and return the inverse of a matrix

## 1 makeCacheMatrix
  # The makeCacheMatrix takes an arugment x, a matrix
  # Before we create the matrix we set the place holder variable m to null
  # The set function accepts an argument y. 
  # The <<- notation sets the value of the passed argument y to x. x is a free variable defined elsewhere. 
  # The varible m in the set function is set to NULL and applied to the variable m at the parent level
  # within the makeCacheMatrix we define three additional functions.
  # get function to retrieve the value of x
  # set_inverse function to assign the inverse of the matrix (using solve function) 
  # get_inverse function to return the matrix m. 
  # finally we return a list containing the functions to set, get, setinverse and get inverse
 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) m <<- solve
        get_inverse <- function() m
        list(set = set, get = get,
             setinverse = set_inverse,
             getinverse = get_inverse)
}


## 2 cacheSolve 
  # The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then cacheSolve will retrieve the inverse from the cache.
  # we first call the getinverse function on the provided argument x. The result is locally assigned to m.
  # the value of m is checked and if its not null then we provide the cached matrix information. 
  # if the value of m is null that means we need to set the inverse.
  # we first retrieve x from makeCacheMatrix
  # Using solve function we performe the inverse of the returned matrix. 
  # we then set the inverted value of m at the parent level by calling the setinverse function from makeCacheMatrix
  # Finally we return the inverted matrix m 


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}