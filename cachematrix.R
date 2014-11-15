## File cachematrix.R
## 
## implements 2 symbiotic functions to avoid computing a matrix inverse 
## more than once. 
##      makeCacheMatrix - constructs a cached.matrix variable 
##      cacheSolve      - solves matrix inverse. 
##                        use cached.matrix as repository for cached inverse   
## Contraints:
##      Only invertable matrices can be solve. There is no exception handling.
##      Behavior of unexpected values are undefined.
##
## Usage Example:
##    cm <- makeCacheMatrix(matrix(c(2, 2, 3, 2), 2, 2))
##    cacheSolve(cm)  # <- Calculated & Cached
##    cacheSolve(cm)  # <- Cache fetch
##
## NOTE: Google coding style was adopted, when not conflicting with
##       Prof. sytle, or assignment's instructions
##       https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

## Constructs a cached.matrix variable composed of a matrix 
## and an externally solved inverse
##
## Args:
##      matrix - The matrix value ex. matrix(c(1:4), 2, 2)
##               NOTE: ONlY invertible matrices will produce valid result   
## Return value:
##  A list of functions:
##      get - get the matrix value
##      set - reset the matrix value
##      getInverse - get the cached matrix inverse value
##      setInverse - set the cached matrix inverse value
makeCacheMatrix <- function(matrix = matrix()) {
    cached.inverse <- NULL  # Cache is empty on creation
    
    # Set new value to the CacheMatrix. 
    # 
    # Args:
    #   new.matrix - The new matrix Value 
    set <- function(new.matrix) {
        matrix         <<- new.matrix
        cached.inverse <<- NULL  # reset Cache
    }
    
    # Cache matrix inverse
    #
    # Args:
    #   inverse - The inverse value to be cached
    setInverse <- function(inverse) cached.inverse <<- inverse
    
    # -- Getters 
    get <- function()        matrix 
    getInverse <- function() cached.inverse
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Solve cached.matrix inverse and caches it on the first call, any 
## consecutive call re-uses the precalculated cached value.
##
## Args:
##      cached.matrix - matrix wrapper created by makeCachedMatrix
##
## Return value:
##      The cached.matrix inverse
cacheSolve <- function(cached.matrix, ...) {
    inverse <- cached.matrix$getInverse()
    if (is.null(inverse)) { 
        # Solve and cache inverse
        inverse <- cached.matrix$setInverse(solve(cached.matrix$get(),...))
    } else {
        message("fetched from cache")  # Absolutly redundent
    }
    inverse 
}

