########################################################################
##################### BIOS 507 : Hw #1 Code ############################
########################################################################

### QUESTION 1 ----------------------------------------------------------

#Code for hypergeometric likelihood plot (original scale)
N <- 20:50
y <- choose(N-20,4)/choose(N,17)
plot(N, y, type="p", col="dodgerblue1"
     , main="Plot of Evidence about Hypergeometric Size Index (where M=20, n=17, and x=13)"
     , xlab="N (Total population of fish)", ylab="Likelihood", cex=1.25, pch=19)

#Calculate scaling factor "c" to coerce highest likelihood=1 (occurs at N=26)
c <- 1/(choose(26-20,4)/choose(26,17))

#Rescaled plot 
plot(N, c*y, type="p", col="dodgerblue1"
     , main="Plot of Evidence about Hypergeometric Size Index (where M=20, n=17, and x=13)"
     , xlab="N (Total population of fish)", ylab="Likelihood", cex=1.25, pch=19)

### QUESTION 2 --------------------------------------------------------------

#Poisson likelihood plot (unscaled)
curve((x^40)*exp(-3.2*x), from=1, to=30,col="dodgerblue1", lwd=2, xlab="x", ylab="y")

#Find absolute maximum in likelihood function
y <- function(x) { (x^40)*exp(-3.2*x) }
optimize(y, interval=c(0,30), maximum=TRUE) #Shows highest likelihood occurs at lambda=12.5

#Calculate scaling factor
c<- 1/((12.5^40)*exp(-3.2*12.5))

#New scaled plot
curve(c*((x^40)*exp(-3.2*x)), from=1, to=30,col="dodgerblue1", lwd=2, 
      xlab="lambda", ylab="likelihood", main="Plot of Evidence about Poisson Size Index")

### QUESTION 2 (X4 and X5 TRUNCATED)----------------------------------------------------

#Poisson likelihood plot (unscaled)
curve((x^40)*exp(-2.5*x), from=1, to=30,col="dodgerblue1", lwd=2, xlab="x", ylab="y")

#Find absolute maximum in likelihood function
y <- function(x) { (x^40)*exp(-2.5*x) }
optimize(y, interval=c(0,30), maximum=TRUE) #Shows highest likelihood occurs at lambda=12.5

#Calculate scaling factor
c<- 1/((12.5^40)*exp(-2.5*12.5))

#New scaled plot
curve(c*((x^40)*exp(-2.5*x)), from=1, to=30,col="dodgerblue1", lwd=2, 
      xlab="lambda", ylab="likelihood", main="Plot of Evidence about Poisson Size Index (X4 and X5 Truncated)")


### QUESTION 3A --------------------------------------------------------------

#Exponential likelihood plot (unscaled)
curve((1/x^7)*exp(-41.5/x), from=1, to=30,col="dodgerblue1", lwd=2, xlab="x", ylab="y")

#Find absolute maximum in likelihood function
y <- function(x) { (1/x^7)*exp(-41.5/x) }
opt <- optimize(y, interval=c(0,30), maximum=TRUE) #Shows highest likelihood occurs at mu=5.928574
xmax <- max$maximum

#Calculate scaling factor
c<- 1/((1/xmax^7)*exp(-41.5/xmax))

#New scaled plot
curve(c*((1/x^7)*exp(-41.5/x)), from=1, to=30,col="dodgerblue1", lwd=2, 
      xlab="mu", ylab="likelihood", main="3A: Plot of Evidence about Exponential Size Index")

### QUESTION 3B --------------------------------------------------------------

#Input function
y <- function(x) { (-exp(-4/x)+exp(-3/x))*(-exp(-9/x)+exp(-8/x))*(-exp(-3/x)+exp(-2/x))*(-exp(-11/x)+exp(-10/x))*(-exp(-3/x)+exp(-1/x))*(-exp(-5/x)+exp(-4/x))*(-exp(-11/x)+exp(-10/x))}

#Unscaled Plot
curve(y, from=1, to=30, col="dodgerblue1", lwd=2, xlab="x", ylab="y")

#Find maximum and rewrite likelihood function with 
opt <- optimize(y, interval=c(0,30), maximum=TRUE) 
x <- opt$maximum
c <- 1/( (-exp(-4/x)+exp(-3/x))*(-exp(-9/x)+exp(-8/x))*(-exp(-3/x)+exp(-2/x))*(-exp(-11/x)+exp(-10/x))*(-exp(-3/x)+exp(-1/x))*(-exp(-5/x)+exp(-4/x))*(-exp(-11/x)+exp(-10/x)) )
y <- function(x) { c*(-exp(-4/x)+exp(-3/x))*(-exp(-9/x)+exp(-8/x))*(-exp(-3/x)+exp(-2/x))*(-exp(-11/x)+exp(-10/x))*(-exp(-3/x)+exp(-1/x))*(-exp(-5/x)+exp(-4/x))*(-exp(-11/x)+exp(-10/x))}

#Scaled plot
curve(y, from=1, to=30,col="dodgerblue1", lwd=2, 
      xlab="mu", ylab="likelihood", main="3B: Plot of Evidence about Exponential Size Index")

### QUESTION 3C --------------------------------------------------------------

#Input function
y <- function(x) { (-exp(-4/x)+exp(-3/x))*(-exp(-9/x)+exp(-8/x))*(-exp(-3/x)+exp(-2/x))*(exp(-10/x))*(-exp(-3/x)+exp(-1/x))*(-exp(-5/x)+exp(-4/x))*(exp(-10/x))}

#Unscaled Plot
curve(y, from=1, to=30, col="dodgerblue1", lwd=2, xlab="x", ylab="y")

#Find maximum and rewrite likelihood function with 
opt <- optimize(y, interval=c(0,30), maximum=TRUE) 
x <- opt$maximum
c <- 1/( (-exp(-4/x)+exp(-3/x))*(-exp(-9/x)+exp(-8/x))*(-exp(-3/x)+exp(-2/x))*(exp(-10/x))*(-exp(-3/x)+exp(-1/x))*(-exp(-5/x)+exp(-4/x))*(exp(-10/x)) )
y <- function(x) { c*(-exp(-4/x)+exp(-3/x))*(-exp(-9/x)+exp(-8/x))*(-exp(-3/x)+exp(-2/x))*(exp(-10/x))*(-exp(-3/x)+exp(-1/x))*(-exp(-5/x)+exp(-4/x))*(exp(-10/x))}

#Scaled plot
curve(y, from=1, to=30,col="dodgerblue1", lwd=2, 
      xlab="mu", ylab="likelihood", main="3C: Plot of Evidence about Exponential Size Index")
