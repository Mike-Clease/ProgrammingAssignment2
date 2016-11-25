## takes a square matrix (x) and returns a list of functions in order to 
## a) set the matrix (set)
## b) get the matrix (get)
## c) set the inverse of the matrix (setinverse)
## d) get the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL # set z to null
  set <- function(y) {
    # assigns (using "<<-") values y and NULL to objects x & i in a
    # different environment
    x <<- y
    i <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## takes the output of makeCacheMatrix (x), checks getinverse to
## see if the inverse is in the cache and either return it from
## the cache or calculates it and then sets the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i = x$getinverse()
  if(!is.null(i)) { #if inverse is already calculate then...
    message("getting cached data")
    return(i)
  }
  # if inverse wasn't cached then calculate it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
