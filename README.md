```r
# Example 1

set.seed(1245)
X <- matrix(sample(1:100, 12), 4, 3)
plotTable(X, bg="lightblue", fg="purple", freq=FALSE, xlab="First factor", ylab="Second factor")

# Example 2

# gD on logistic regression
getErr <- function(x, w, y){
  x <- as.matrix(x)
  tmp <- 1/(1+ exp(-cbind(1, x)%*%w))
  fit <- ifelse(tmp<.5, 0, 1)
  return(sum(fit!=y)/length(fit))
}
fitLogit <- function(x, w){return(1/(1+ exp(-cbind(1, x)%*%w)))}

op <- par(no.readonly = TRUE)
set.seed(1244)

n = 1000
x <- seq(-1, 1, len = n)
y <- c(rbinom(n/2, 1, prob = .05), rbinom(n/2, 1, prob = .95))

samp <- sample(1:n, n*.6)
xtrain <- x[samp]; ytrain <- y[samp]
xtest <- x[-samp]; ytest <- y[-samp]
model <- glm(ytrain ~ xtrain, family = binomial)
w <- as.numeric(coef(model))

yFit <- ifelse(fitLogit(xtest, w)<.5, 0, 1)
myTab <- table(fitted=yFit, obs=ytest)
myTab

par(mfrow = c(1, 2))
plot(xtrain, ytrain, col = ytrain+1, xlab="x-values", ylab="Probability")
lines(sort(xtrain), fitLogit(xtrain, w)[order(xtrain)], col = "cyan", lwd = 6)
plotTable(myTab, r=.5, t=.5, fg="darkblue", bg="powderblue", freq=FALSE,
          xlab="Observed", ylab="Fitted", main = "Performance")
par(op)

```