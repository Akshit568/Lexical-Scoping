# Function to create a special "vector" with caching abilities
makeVector <- function(x = numeric()) {
  m <- NULL
  
  # Set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL # Clear the cached mean when the vector changes
  }
  
  # Get the value of the vector
  get <- function() x
  
  # Set the value of the mean
  setmean <- function(mean) m <<- mean
  
  # Get the value of the mean
  getmean <- function() m
  
  # Return a list of functions
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

# Function to calculate the mean of the vector, caching the result if it's not already cached
cachemean <- function(x, ...) {
  m <- getmean()  # Accessing getmean from the parent environment lexically
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  } else {
    message("Calculating mean")
    data <- get()  # Accessing get from the parent environment lexically
    m <- mean(data, ...)
    setmean(m)  # Accessing setmean from the parent environment lexically
    return(m)
  }
}

