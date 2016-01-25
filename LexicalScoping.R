makeCacheMatrix <- function(x = matrix()) {
  Inver <- NULL
  set <- function(y) {
    x <<- y
    Inver <<- NULL
  }
  get <- function() x
  setInver <- function(inverse)Inver <<- inverse
  getInver <- function() Inver
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}
cacheSolve <- function(x, ...) {
  Inver <- x$getInver()
  if(!is.null(Inver)) {
    message("getting cached data")
    return(Inver)
  }
  matx <- x$get()
  Inver <- solve(matx, ...)
  x$setInver(Inver)
  Inver
}