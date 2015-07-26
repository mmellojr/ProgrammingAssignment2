# ----------------------------------------------------------------------------
# makeCacheMatrix - Creates 'matrix' object that can cache its inverse
# v01.00 - Fri 2015/07/24
# ----------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    invMatrixMCM <- NULL
    set <- function(y) {
        x <<- y
        invMatrixMCM <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) invMatrixMCM <<- inverse
    getInv <- function() invMatrixMCM
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# ----------------------------------------------------------------------------
# v01.00 - Fri 2015/07/24
# ----------------------------------------------------------------------------
# makeCacheMatrix <- function(inputMatrix, ...)
#   clear matrix to be inverted (NULL)
#   set cache of input matrix [internal environment, via <<-]
#   get input matrix
#   set cache of inverted matrix
#   get inverted matrix
#   return list (cache (new) matrix, get (new) input matrix,
#       cache (inverted) matrix, get (inverted) matrix)
# ----------------------------------------------------------------------------
# Variables - arguments & local - variableNameMCM
# Functions - set, get, setInv, getInv
# ----------------------------------------------------------------------------
#
#
# ============================================================================
#
#
# ----------------------------------------------------------------------------
# cacheSolve - Computes inverse of 'matrix' object
# v01.00 - Fri 2015/07/24
# ----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    invertedMatrixCS <- x$getInv()
    if(is.null(invertedMatrixCS)){
        inputMatrixCS <- x$get()
        invertedMatrixCS <- solve(inputMatrixCS)
        x$setInv(invertedMatrixCS)
    } else {
        message("getting cached data")
    }
    return(invertedMatrixCS)
}

# ----------------------------------------------------------------------------
# v01.00 - Fri 2015/07/24
# ----------------------------------------------------------------------------
# cacheSolve <- function(inputMatrix, ...)
# determine invertedMatrix - can be NULL or result of previous inversion
# if inverteddMatrix is NULL (not previously inverted) then {
#   get inputMatrix to be inverted
#   invert inputMatrix
#   cache invertedMatrix
#   } else (previously inverted) {
#       message("using cached data") 
#   }
# return(invertedMatrix) [either just inverted or cached]
# ----------------------------------------------------------------------------
# Variables - arguments & local - variableNameCS
# Functions - get, setInv, getInv
# ----------------------------------------------------------------------------