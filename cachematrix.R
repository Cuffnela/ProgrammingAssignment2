## 
## Program intended to reduce run time by caching matrix inverse values
## checks if inverse has already been computed by checking cached values
## pulls from the cache if the matrix has not changed, otherwise computes 
## and stores for next cache check.
## 

## Function makeCacheMatrix takes in a matrix and returns a list with four elements
##
## Returned functions in list:
## set: assigns the value of the matrix and caches values
## get: gets the cached matrix
## setInverse: uses the solve function to set inverse and caches inverse
## getInverse: gets the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set<- function(y){
        x<<-y
        m <<-NULL
    }
    get<-function()x
    setInverse<-function(solve)m<<-solve
    getInverse<-function()m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Function cacheSolve finds the inverse of a matrix. 
##
## First checks to see if the mean has already been calculated.
##
## If the inverse is already calculated pulls the inverse from the cache 
## and skips computation.
##
## If not calculated. Runs the computation and caches the inverse via
## setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setInverse(m)
        m
}
