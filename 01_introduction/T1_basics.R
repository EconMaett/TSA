# Tutorial 1
# Basic commands

2 + 2  # addition
5 * 5 + 2  # multiplication and addition
5 / 5 - 3  # division and subtraction
log(exp(pi))  # log, exponential, pi
sin(pi / 2)  # sinusoids
exp(1) ^ (-2)  # power
sqrt(8)  # square root
1:5  # sequences
seq(1, 10, by = 2)  # sequences
rep(2, 3)  # repeat 2 three times

#> Next, we'll use assignment to make some objects: ----

x <- 1 + 2  # put 1 + 2 in object x
x = 1 + 2  # same as above with fewer keystrokes
1 + 2 -> x  # same
x  # view object x
(z = rnorm(5, 0, 1))  # put 5 standard normals into z and print z

## To list your objects, remove objects, get help, find out which
## directory is current (or to change it) or to quit, use the following commands:

ls()  # list all objects
help(exp)  # specific help (?exp is the same)
getwd()  # get working directory (tells you where R is pointing)
#setwd('C:\\Users')  # change working directory in windows (note double back slash)
#setwd('/home')  # for linux, mac and windows users (note forward slash)
#q()  # end the session (keep reading)

#> To create your own data set, you can make a data vector as follows: ----

mydata = c(1, 2, 3, 2, 1)  # where we have concatenated the numbers
mydata  # display the data
mydata[3]  # the third element
mydata[3:5]  # elements three through five
mydata[-(1:2)]  # everything except the first two elements
length(mydata)  # length of elements
mydata = as.matrix(mydata)  # make it a matrix
dim(mydata)  # provide dimensions of matrix
rownames(mydata) <- paste0("obs", 1:5)  # include row-names
colnames(mydata) <- c("rand_num")  # include column-names
print(mydata)  # print the matrix to the console
matrix(rnorm(16), ncol = 4)  # alternatively
array(rnorm(10*5*4), dim=c(10, 5, 4))  # multi-dimensional array

#> The rule for doing arithmetic with vectors: ----

x = c(1, 2, 3, 4)
y = c(2, 4, 6, 8)
z = c(10, 20)
x * y  # i.e. 1*2, 2*4, 3*6, 4*8
x / z  # i.e. 1/10, 2/20, 3/10, 4/20
x + z  # i.e. 1+10, 2+20, 3+10, 4+20
x %*% y  # matrix multiplication

#> Basic loops: ----

for (id1 in 1:4) {
  print(paste("Execute model number", id1))
}  # for loops are the most common

id2 <- 1
while (id2 < 6) {
  print(id2)
  id2 = id2 + 1
}  # while loops can be used as an alternative

#> The else statement could be used to execute dependent arguments ----

x <- -5  # to test if this is positive or negative

if (x == 0) {
  print("Zero")
} else if (x > 0) {
  print("Positive number")
} else {
  print("Negative number")
}

# One could also vectorise the code and use `sapply` or `tidyverse::map` function to make code run quicker. Additional details regarding the `tidyverse` will follow.

#> Simulating regression data: ----

n <- 50  # number of observations
x <- seq(1, n)  # right-hand side variable
y <- 3 + x + rnorm(n)  # left-hand side variable

plot(x, y)  # show the linear relationship between the variables
plot.ts(y)  # plot time series

#> Estimate a linear regression model: ----

out1 <- lm(y ~ x - 1)  # regress x on y without constant
summary(out1)  # generate output

#> Using packages: ----

# To install and use the functions within a package that is located on the CRAN repository:
#install.packages('strucchange') # to download the package from CRAN
library('strucchange') # to access the routines in this package
break_y <- Fstats(y ~ 1) # F statistics indicate one breakpoint
breakpoints(break_y)

# When making use of multiple packages within a single script and they have a name conflict then use the syntax `<package>::<routine>`. For example:
break_y <-
  strucchange::Fstats(y ~ 1) # F statistics indicate breakpoint
strucchange::breakpoints(break_y)

#> Creating functions: ----

# Rather than repeat a string of commands you may want to create a function
# By way of example to remove the largest and smallest value in vector before taking the mean we could perform the calculation
x <- rnorm(10)
x <- sort(x)[-c(1, length(x))]
mean(x)

# Or alternatively we could create the function `mean_excl_outl` where `x` is both the input and output argument
mean_excl_outl <- function(x) {
  x <- sort(x)[-c(1, length(x))]
  x <- mean(x)
  return(x)
}

# Now whenever we need to perform this calculation
mean_excl_outl(rnorm(20))


