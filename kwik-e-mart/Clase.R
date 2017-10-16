?eliminateUniqueValues <- function(dataFrame) {
 
    columns <- names(dataFrame)
    for (col in columns) {
      if (nrow(dataFrame) == 
          length(unique(dataFrame[,col]))) {
        dataFrame[,col] <- NULL
      }
    }
    return(dataFrame)
}

df <- data.frame(id=c(1,2,3,4,5), val=c(2,3,3,5,6))
df2 <- data.frame(id=c(1,2,3,4,5), val=c(2,3,3,5,6), val2=rep(2,5))


numericColumns <- function(df) {
  
  if (is.data.frame(df)) {
  columns <- names(df)
  result <- NULL
  for (col in columns) {
     if (is.numeric(df[,col])) {
       result <- c(result,col)
     }
  }
  return(result)
  } else {
    print("Parameter should be a data frame")
    return(NULL)
  }
}

factorial <- function(n) {
  if (n == 1) {
    return(1)
  }
  return(n * factorial(n-1))
}

factorial2 <- function(n) {
  result <- 1
  while(n > 1) {
    result <- result * n
    n <- n - 1
  }
  return(result)
}

detectOutliers <- function(df) {
  columns <- numericColumns(df)
  result <- NULL
  for (col in columns) {
    mu <- mean(df[,col], na.rm=T)
    sigma <- sd(df[,col], na.rm=T)
    outliers <- sapply(df[,col],function(x) x > mu+2*sigma || x <mu-2*sigma)
    result <- cbind(result, outliers)
  }
  df <- as.data.frame(result)
  names(df) <- columns
  return(df)
}

findPresidents <- function(output) {
  
  result <- NULL
  for (page in 1:length(output)) {
    for (presidentNum in "1":"46") {
      index <- sapply(output[[page]][,1],function(x) x == presidentNum)
      if (sum(index) > 0) {
         row <- output[[page]][index,]
         result <- rbind(result,row)
      }
    }
  }
  return(result)
}

convertHeights <- function(heights,src="metric") {
  if (src == "metric") {
    heights <- heights * 3.28
  }
  else if (src == "imperial") {
    heights <- heights / 3.28
  }
  else {
    stop("Here's an error")
  }
  return(heights)
}