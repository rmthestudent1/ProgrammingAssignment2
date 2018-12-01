#MAIN FUNC ONE: Creates matrix for x (actual values) 
#           and im (mean), in the cache and local environment

makeCacheMatrix <- function(x = matrix()) {
  ##prepares an empty matrix for the inverse (local env)
  im <- NULL
  ##function: puts the values of y in x (cache)
  ##          prepares an empty matrix for the inverse (cache)
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  ##function: puts the values of inputted x in x (local env)
  get <- function() x
  ##function: puts the inverse matrix in im (cache)
  setimat <- function(invmat) im <<- invmat
  ##function: puts the values of im in im (local env)
  getimat <- function() im
  ##checks if matrices are similar. four main matrices now 
  ##   1. inputted values (x) in cache, 2. inputted values (x) in loc env,
  ##   3. invmat (im) in cache, 4. invmat (im) in loc env.
  list(set = set, get = get,setimat = setimat,getimat = getimat)
}

#MAIN FUNC TWO: Solves for the inverse matrix if data is not in the cache

cacheSolve <- function(x, ...) {
  ##retrieves the computed inverse matrices for x, puts it in im
  im <- x$getimat()
  ##if im is not empty, meaning it's already been computed, 
  ##     system gets im from cache
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  ## else? solves it
  data <- x$get()
  im <- invmat(data, ...)
  x$setimat(im)
  im
}