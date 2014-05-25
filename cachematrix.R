## makeCacheMatrix will store the inverse (setCache) and return it (getCache)
## cacheSolve computes the inverse of the matrix using solve()

## set() will store the matrix itself being passed in
## get() will return the matrix that has been passed in
## setCache() will set the inverse that was computed in cacheSolve() by the solve() function
## getCache() will return the cached inverse that was computed

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function()
  {
    x
  }
  setCache <- function(cache)
  {
    m <<- cache
  }
  getCache <- function()
  {
    m
  }
  
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}

## this function will check to see if there's a cache stored and if there is it returns the matrix
## data variable will get the matrix from the function above
## then solve will compute the inverse and it gets stored in m
## m is returned

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getCache()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  m
} 
