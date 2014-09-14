
# IS607 Week 3 Assignment - Neil Acampa

#1) Write a function that takes as input the number of missing values


require(stringr)


mvcnt <- function(nvec) {
  # Calculate number of missing values (values == NA)
  mvec <- is.na(nvec)
  cnt <- sum(mvec)
  return(cnt)
}

nvec <- c(1,2,1,NA,11,13,NA,NA)


#2) Take a data frame as input and return a named vector of the number of
#   Missing values in ech column


nmvalcnt <- function(df) {
  # Calculate the number of missing values in each column of the Data Frame
  # return a named vector using the names of each column of the data frame
  # with the number of missing values.
  # Call missingvals function from question 1
  
  result <- vector()
  dfnames <- names(df)
  n <- ncol(df)
  result <- vector()
  
  for (i in 1:n) {
    result[i] <- mvcnt(df[,i])
  }
  
  finalresult <-c(dfnames, MValCnt = result)
  return(finalresult)
}

userid         <- c(1,2,3,4,5,6,7)
Firstname      <- c("Rebecca","Mary","Martha","Elizabeth","Anna","Olga","Jim")
Lastname       <- c("Tomma",NA,"Beckensale",NA,"Gilman","Smith","Roma")
gender         <- c("F","F",NA,"F",NA,F,NA)
maritalStatus  <- c("S","M",NA,"M",NA,NA,"M")
df             <- data.frame(userid,Firstname,Lastname,gender,maritalStatus)


#3) Write a function that takes a numeric vector as input and uses it to determine
#   the minimum, the maxmum, the mean, the median, the first quartile, the third quartile
#   the standard deviation, the number of missing values. Do not use built in functions
#   Return a named list with the 8 desired values in any order.
#
#   (Note: Each task is solved in a seperate function)
#   (The main function named "vstats" will take a vector and return a named list)
#   (using the functions)


# Function to remove missing values
convertvec <- function(vec) {
  n = length(vec)
  mvec <- is.na(vec)
  tvec <- vector()
  idx = 0
  for (i in 1:n) {
    if (mvec[i] == FALSE) {
      idx = idx + 1
      tvec[idx] = vec[i]
    }
  }
  return(tvec)
}

vec <- c(1,2,1,NA,11,13,NA,NA)


# Function to calculate the median
calcmedian <- function(vec) {
  cvec <- convertvec(vec)
  cvec <- sort(cvec)
  n = length(cvec)
  if ((n %% 2) == 0) {
    midpt1 <- floor(n/2)
    midpt2 <- midpt1 + 1
    median <- ((cvec[midpt1] + cvec[midpt2]) /2)
  } else {
    midpt1 <- floor(n/2) + 1
    median <- cvec[midpt1]
  } 
  return(median)
}

vec <-c(7,15,36,39,40,41)
calcmedian(vec)

vec <-c(6,7,15,36,39,40,41,42,43,44,56)
calcmedian(vec)

vec <- c(2, 4, 4, 4, 5, 5 , 7, 9, NA)
calcmedian(vec)

# Quartile 1 and 3
# Using method which includes the midpoint if it is a discrete data point,
# split vector in 2 and call function median passing each half
# otherwise just spilt the vector in half and call meduian 

calcquartile <- function(vec) {
  cvec <- convertvec(vec)
  n = length(cvec)
  cvec <- sort(cvec)
  lvec <- vector()
  uvec <- vector()
  if ((n %% 2) == 0) {
    midpt1 <- floor(n/2)
    midpt2 <- midpt1 + 1
    lvec <- cvec[1:midpt1]
    uvec <- cvec[midpt2:n] 
  } else {
    midpt1 <- floor(n/2) + 1
    lvec <- cvec[1:midpt1]
    uvec <- cvec[midpt1:n]
  } 
  
  qtrl1 = calcmedian(lvec)
  qtrl2 = calcmedian(uvec)
  result= c(qtrl1,qtrl2)
  return(result)
}

result <- calcquartile(vec)
cat("Quartile1 = ", result[1], "\n")
cat("Quartile3 = ", result[2], "\n")


# Function to calculate mean

calcmean <- function(vec) {
  
  sum =0
  result=0
  cvec <- convertvec(vec)
  n = length(cvec)
  for (i in 1:n) {
    sum = sum + cvec[i]
  } 
  result = sum / n
  return(result)
}


# Function for Standard Deviation
calcstd <- function(vec, calcmean) {
  sum = 0
  ssq = 0
  cvec <- convertvec(vec)
  n = length(cvec)
  for (i in 1:n) {
    ssq  = ssq + ((cvec[i] - calcmean)^2)
  }
  result = sqrt(ssq/n)
  return(result)
}

# Function to calculate min/max

calcminmax <- function(vec) {
  
  min = 999999
  max = 0
  cvec <- convertvec(vec)
  n = length(cvec)
  for (i in 1:n) {
    if (cvec[i] >= max) {
      max <- cvec[i]
    }
    if (cvec[i] < min) {
      min <-  cvec[i]
    }
  }
  result = c(min,max)
  return(result)
}



vstats <- function(vec) {
  # Statistics for a numeric vector
  # statlist[1] = number of missing values 
  # statlist[2] = Minimum value
  # statlist[3] = Maximum value
  # statlist[4] = Mean
  # statlist[5] = Median
  # statlist[6] = Quartile1
  # statlist[7] = Quartile3
  # statlist[8] = Standard Deviation
  
  statlist <- vector()
  # Get number of missing values
  statlist[1] <- mvcnt(vec)
  # Get Min/Max
  result      <- calcminmax(vec)
  statlist[2] <- result[1]
  statlist[3] <- result[2]
  # Get Mean
  statlist[4] <- calcmean(vec)
  # Get median
  statlist[5] <- calcmedian(vec)
  # Get quartile 1 and 3
  result      <- calcquartile(vec)
  statlist[6] <- result[1]
  statlist[7] <- result[2]
  # Get standard dev
  statlist[8] <- calcstd(vec, statlist[4])
  result <- list(NumMissingVals = statlist[1], Minimun = statlist[2], Maximum = statlist[3], Mean = statlist[4], Median = statlist[5], Quartile1 = statlist[6], Quartile3 = statlist[7], Stdev = statlist[8])
  return(result)
} 


# 4) Write a function that takes a character vector or factor vector and determines the
#    number of distinct elements, the most common elements, the number of times the
#    most common elements occur and the number of missing vales


charstats <- function(vec) {
  # Stats for either a Character vector or a Factor vector
  # statlist[1] = number of missing values 
  # statlist[2] = number of distinct values
  # statlist[3] = Most common value
  # statlist[4] = Frequency of most common
  # statlist[5] = Ties for most common value
  
  statlist <- vector()
  # Get number of missing values
  statlist[1] <- mvcnt(vec)
  
  resultvec <- vector()
  namevec   <- vector()
  max = 0 
  maxelement = ""
  maxtie = ""
  if (is.character(vec)) {
    # Code for Character vector stats
    cvec <- convertvec(vec)
    n = length(cvec)
    for (i in 1:n) {
      e <- cvec[i]
      temp = match(cvec,e)
      # remove NA and count
      temp <- convertvec(temp)
      resultvec[i] = sum(temp)
      # now test to see if we can add
      
      if (resultvec[i] >= max) {
        max = sum(temp)
        maxelement = e
        if ((resultvec[i] == max) && (maxtie != e) && (maxtie != "")) {
          maxtie = e
        }
      }
      # Now update the number of distinct elements
      if (i == 1) { namevec[i] = e}
      addtest <- match(namevec,e)
      atest   <- sum(convertvec(addtest))
      if (atest < 1) {
        namevec[i] = e
      }
    }
    statlist[2] <- length(namevec)
  } else {
    # code for factor vector
    statlist[2] <- length(levels(vec))
    cvec <- convertvec(vec)
    n = length(cvec)
    for (i in 1:n) {
      valid = length(levels(factor(vec[i])))
      if (valid == 1) {
        e <- vec[i]
        temp = match(vec,e)
        # remove NA and count
        temp <- convertvec(temp)
        resultvec[i] = sum(temp)
        # now test to see if we can add
        if (resultvec[i] >= max) {
          max = sum(temp)
          maxelement = levels(factor(vec[i]))
        }
      }
    }
  }
  statlist[3] <- maxelement
  statlist[4] <- max
  statlist[5] <- maxtie
  result <- list(NumMissingVals = statlist[1], NumDistinctVals = statlist[2], MostCommon = statlist[3], FreqMostCommon = statlist[4])
  return(result)
}

vec <- c("Mary",NA,"Mary","Martha","Elizabeth","Anna","Olga","Jim", NA, NA, "Jim","Mary", "Jim")




# 5) Write a function that takes a logical vector and determins the number of true
#    false and the proportion of true and the number of missing values. The funtion
#    returns a named list with the information in a logical order

logstats <- function(vec) {
  # Statistics for a Logical vector
  # statlist[1] = number of missing values 
  # statlist[2] = number of true values
  # statlist[3] = number of false values
  # statlist[4] = proportion of true values
  
  statlist <- vector()
  namelist <- vector()
  statlist[1] <- mvcnt(vec)
  cvec <- convertvec(vec)
  n <- length(cvec)
  statlist[2] <- sum(cvec) 
  statlist[3] <- n - statlist[2]
  statlist[4] <- statlist[2] / n
  result <- list(NumMissingVals = statlist[1], NumberTrue = statlist[2], NumberFalse = statlist[3], PercentTrue = statlist[4])
  return(result)
}

vec <- c(TRUE,FALSE,TRUE,TRUE,NA,NA,NA)

6)

Userid         <- c(1,2,3,4,5,6,7)
Firstname      <- c("Rebecca","Mary","Martha","Elizabeth","Anna","Olga","Martha")
Lastname       <- c("Tomma",NA,"Beckensale",NA,"Smith","Smith","Roma")
ColledgeGrad   <- c(TRUE,FALSE,TRUE,TRUE,TRUE,NA,FALSE)
ColledgeGPA    <- c(3.2,NA,3.0,3.4,2.9,NA,NA)
Employeed      <- c("Y","N","Y","Y",NA,"Y","Y")
MaritalStatus  <- c("S","S","M",NA,NA,"M","S")
df             <- data.frame(userid,Firstname,Lastname,ColledgeGrad,ColledgeGPA,Employeed,MaritalStatus)


dstats <- function(df) {
  misvalvec    <- vector()
  dfnames     <- names(df)
  workvec     <- vector()
  result      <- list()
  list_all <- list()
  list1    <- list()
  n <- ncol(df)
  # have to work on return
  for (i in 1:n) {
    clist <- list()
    workvec   <- df[,i]
    misvalvec[i] <- mvcnt(workvec)
    if (is.numeric(workvec)) {
      clist <- vstats(workvec)
    }
    if (is.logical(workvec)) {
      clist <- logstats(workvec)
    }
    if (is.character(workvec)) {
      clist <- charstats(workvec)
    }
    if (is.factor(workvec)) {
      clist <- charstats(workvec)
    }
    result[[i]] <- list(clist)
  }
  return(result)
}

t <- dstats(df)






