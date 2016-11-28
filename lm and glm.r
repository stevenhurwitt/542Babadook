###STAT 542 Statistical Learning
###Final Project

#Load the data
blog.train = read.csv("blogData_train.csv")
blog.test = read.csv("blogData_test.csv")

n = dim(blog.train)[1]
p = 280
#Check for constant columns
for (i in 1:p){
	if(length(unique(blog.train[,i])) == 1)
	print(i)
}
#Result: 13, 33, 38, 278


const = c(-13, -33, -38, -278)
X = blog.train[,1:p]
X = X[,const]
Y = blog.train[,p+1]

X.test = blog.test[,1:p]
X.test = X.test[,const]

Y.mean = mean(Y)
Y.log = log(Y+1)
Y.log.test = log(blog.test[,p+1]+1)

#Multivariate linear regression
lmfit = lm(Y.log~., data = data.frame(X,Y.log))
summary(lmfit)
#Prediction accuracy
pred.lm = predict(lmfit, newdata = data.frame(blog.test[,1:p]))
mse.lm = mean((Y.log.test - pred.lm)^2)

#k-nearest neighbor
library("kknn")
knnfit = kknn(Y.log~., train = data.frame(x = X, y = Y.log), test = data.frame(x = X.test, y = Y.log.test),
				k = 10, kernel = "rectangular")
pred.knn = knnfit$fitted.values
mse.knn = mean((Y.log.test - pred.knn)^2)

#Generalized Linear Model
#Ridge regression
library("glmnet")
ridgefit = glmnet(y = Y.log, x = as.matrix(X), alpha = 0)
ridgefit.cv = cv.glmnet(y = Y.log, x = as.matrix(X), alpha = 0, lambda = ridgefit$lambda)
plot(ridgefit.cv, main = "Ridge Regression-Cross-Validation MSE")

lam = ridgefit.cv$lambda.min
pred.ridge = predict(ridgefit.cv, newx = as.matrix(X.test), lambda = lam)
mse.ridge = mean((Y.log.test - pred.ridge)^2)

#Lasso
lassofit = glmnet(x = as.matrix(X), y = Y.log, alpha = 1)
#Plot the coefficients against the tuning parameter lambda
plot(lassofit, xvar = "lambda", main = "Lasso coefficients")
#Cross-validation
lassofit.cv = cv.glmnet(x = as.matrix(X), y = Y.log, alpha = 1, lambda = lassofit$lambda)
plot(lassofit.cv, main = "Lasso-Cross-Validation MSE")
#Select the tuning parameter lambda with the smallest cross-validation error
lam = lassofit.cv$lambda.min
pred.lasso = predict(lassofit.cv, newx = as.matrix(X.test), lambda = lam)
mse.lasso = mean((Y.log.test - pred.lasso)^2)
lasso.beta = coef(lassofit.cv, s = "lambda.min")
#Number of nonzero coefficients
sum(lasso.beta!=0)

#MCP
library("ncvreg")
mcpfit = cv.ncvreg(X, Y.log, penalty = "MCP", gamma = 3, lambda = seq(0.01,0.1,0.01))
plot(mcpfit)
mcp.beta = mcpfit$fit$beta[, mcpfit$min]
sum(mcp.beta!=0)
pred.mcp = predict(mcpfit, as.matrix(X.test), which = mcpfit$min)
mse.mcp = mean((Y.log.test - pred.mcp)^2)

#SCAD
scadfit = cv.ncvreg(X, Y.log, penalty = "SCAD", gamma = 3, lambda = seq(0.01,0.1,0.01))
plot(scadfit)
scad.beta = scadfit$fit$beta[, scadfit$min]
sum(scad.beta!=0) #Number of predictors selected
plot(ncvreg(X, Y.log, penalty = "SCAD", gamma = 3.7, lambda = seq(0.01,0.1,0.01)))
pred.scad = predict(scadfit, as.matrix(X.test), which = scadfit$min)
mse.scad = mean((Y.log.test - pred.scad)^2)



