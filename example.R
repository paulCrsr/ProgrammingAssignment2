makeVector <- function(x = numeric()) {
        
        m <- NULL # Mean
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setmean <- function(mean) m <<- mean
        
        getmean <- function() m
        
        list(set = set, 
             get = get,
             setmean = setmean,
             getmean = getmean)
        
}

cachemean <- function(x, ...) {
        
        m <- x$getmean() # Cached mean
        
        if (!is.null(m)) {
                message("gettting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        
        m
        
}

cachemean(makeVector(c(1,2,3,NaN,4)), na.rm=T)