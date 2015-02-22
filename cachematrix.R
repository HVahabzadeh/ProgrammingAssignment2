## These functions calculate the inverse of a matrix and store it 
## for repeated use.

## This function provieds the structure to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y){
                x   <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get= get, setinv=setinv, getinv=getinv)
}

## This function either calculates the inverse of a matrix 
## or loads teh pre-calculated inverse

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        tmp_inv <- m$getinv()
        if(!is.null(tmp_inv)){
                print("inv is not null")
                return(tmp_inv)
        }
        else{
                tmp_inv <- solve(m$get())
                m$setinv(tmp_inv)
                tmp_inv
        }
}
