###STAT 542 Statistical Learning
###Final Project
###Author: Steven Hurwitt, Yan Liu, Albert Yu
###Date: 11-25-2016

###########################################################################
#set the working directory

#Load the data
blog.train = read.csv("blogData_train.csv")
blog.test = read.csv("blogData_test.csv")

#Assign column names
descript = c(".avg", ".sd", ".min", ".max", ".med")
n = length(descript)

var = c("TC", "NC24hr", "NCT1T2", "NCafterpub", "24hrT1diff",
        "TC.L", "NC24hr.L", "NCT1T2.L", "NCafterpub.L", "24hrT1diff.L")
next.var = c("time", "length")
m = length(var)

new.var = NULL
for (i in 1:m){
  sub.i = paste(var[i], descript, sep = "")
  new.var = c(new.var,sub.i)
}

new.var = c(new.var, next.var)
new.var = c(new.var, colnames(blog.train)[63:281])
colnames(blog.train) = new.var
colnames(blog.test) = colnames(blog.train)


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

Y.mean = mean(Y)
Y.log = log(Y+1)
Y.log.test = log(blog.test[,p+1]+1)

######Exploratory Data Analysis

#function to plot histograms w/ ggplot using training data
#inputs: variable of interest as a vector (var), 
#a name to use for the title and X axis, also as a character string,
#the number bin width, & the dataset to use
#output: prints the final histogram, to be exported as .png for latex
library("ggplot2")
gg.hist = function(var, var.name, bin.width, my.data){
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  rando.color = sample(cbPalette, size = 1)
  
  gg.hist.plot = ggplot() + geom_histogram(aes(x = var), binwidth = bin.width, fill = rando.color, data = my.data)
  gg.hist.plot = gg.hist.plot + ggtitle(paste("Histogram of ", var.name)) + xlab(var.name) + ylab("Count")
  gg.hist.plot = gg.hist.plot + theme(legend.position = "none")
  print(gg.hist.plot)
}

#look at averages before & after
gg.hist(blog.train$TC.avg, "Avgerage Comments Before", 30, blog.train)
gg.hist(blog.train$NC24hr.avg, "Avgerage Comments 24 hrs Before", 10, blog.train)
gg.hist(blog.train$NCT1T2.avg, "Average Comments 48-24 hrs Before", 10, blog.train)

#avg of comments after publication
gg.hist(blog.train$NCafterpub.avg, "Average Comments After Publication", 40, blog.train)

#...medians
gg.hist(blog.train$TC.med, "Median Comments Before", 70, blog.train)
gg.hist(blog.train$NC24hr.med, "Median Comments 24 hrs Before", 30, blog.train)
gg.hist(blog.train$NCafterpub.med, "Median Comments After Publication", 35, blog.train)
gg.hist(blog.train$NCT1T2.med, "Average Comments 48-24 hrs Before", 30, blog.train)

#...medians (without outliers)
new.med = blog.train$TC.med[blog.train$TC.med < 400]
new.data = blog.train[blog.train$TC.med %in% new.med,]

new.med2 = blog.train$NC24hr.med[blog.train$NC24hr.med < 150]
new.data2 = blog.train[blog.train$NC24hr.med %in% new.med2,]

new.med3 = blog.train$NCafterpub.med[blog.train$NCafterpub.med < 200]
new.data3 = blog.train[blog.train$NCafterpub.med %in% new.med3,]

new.med4 = blog.train$NCT1T2.med[blog.train$NCT1T2.med < 100]
new.data4 = blog.train[blog.train$NCT1T2.med %in% new.med4,]

gg.hist(new.med, "Median Comments Before", 20, new.data)
gg.hist(new.med2, "Median Comments 24 hrs Before", 3, new.data2)
gg.hist(new.med3, "Median Comments After Publication", 10, new.data3)
gg.hist(new.med4, "Median Comments 48-24 hrs Before", 2, new.data4)

#sd's of TC, NC24hr, NCT1T2, NCafterpub
gg.hist(blog.train$TC.sd, "Standard Deviation of Comments Before", 20, blog.train)
gg.hist(blog.train$NC24hr.sd, "Standard Deviation of Comments 24 hrs Before", 20, blog.train)
gg.hist(blog.train$NCT1T2.sd, "Standard Deviation of Comments 48-24 hrs Before", 20, blog.train)
gg.hist(blog.train$NCafterpub.sd, "Standard Deviation of Comments After Publication", 20, blog.train)

#####Correlation Matrix
X = blog.train[,1:p]
X = X[,const]
sds = apply(as.matrix(X), 2, FUN = sd)
nonzero.sd.X = subset(X, sds > 0)
X2 = X[,-(63:262)]

library("corrplot")
cor.mat = cor(X2)
par(cex = .6)
corrplot(cor.mat, type = "lower", method = "color", order = "AOE", tl.col = "black", tl.srt = 45)


#########################################################################
### Data Analysis
#########################################################################
#Multivariate linear regression
X = X[,const]
lmfit = lm(Y.log~., data = data.frame(X,Y.log))
summary(lmfit)
#Prediction accuracy
pred.lm = predict(lmfit, newdata = data.frame(blog.test[,1:p]))
mse.lm = mean((Y.log.test - pred.lm)^2)

#k-nearest neighbor
library("kknn")
#Cross-validation
nfold = 10
infold = sample(rep(1:nfold, length.out = n))
K = 20
Err10FoldCV = matrix(nrow = K, ncol = nfold, data = 0)

for (i in 1:nfold)
{
	for (k in 1:K)
	{
		knnfit = kknn(Y.log~., train = data.frame(x = X, y = Y.log), test = data.frame(x = X.test, y = Y.log.test),
				k = k, kernel = "rectangular")
		pred.knn = knnfit$fitted.values
		Err10FoldCV[k,i] = mean((Y.log.test - pred.knn)^2)
	}
}

#Plot the results
mean_err = apply(Err10FoldCV, MARGIN = 1, FUN = mean)
plot(rep(1:K, nfold), as.vector(Err10FoldCV), pch = 19, cex = 0.5,
	xlab = "choice of k", ylab = "CrossValidation Error",
	main = "MSE of 10-Fold CrossValidation of kNN")
points(1:K, mean_err, col = 2, pch = 19, type = "l", lwd = 3)

#####Generalized Linear Model
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


###########################################################################
###XGBOOST
library("xgboost")
X = blog.train[,1:p]
X = X[,const]
xgb.fit = xgboost(data = data.matrix(X), label = Y.log, max.depth = 4, 
	eta = 0.07, gamma = 0.1, nrounds = 500)
pred.xgb = predict(xgb.fit, data.matrix(X.test[,const]))
mse.xgb = mean((pred.xgb - Y.log.test)^2)


###########################################################################
##### Final Model
###########################################################################
###Random Forests
set.seed(1000)
sample.train = sample(1:n, 5000)

library("randomForest")
rffit = randomForest(x = blog.train[sample.train,1:280], y = log(blog.train[sample.train,281]+1), 
                      ntree = 2000, mtry = 15)
imp = rffit$importance
plot(imp, type = "l", lwd = 2, xlab = "Predictors", ylab = "Importance")
title("Importance of Predictors", cex = 0.5)

#Select the variables with high importance
ind = c()
for(i in 1:p){
     if(imp[i]>20) ind = cbind(ind, i)
     }

###########################################################################
#PCA on the word matrix
word.mat = X[,60:259]
word.pca = princomp(word.mat)
screeplot(word.pca, npcs=20, type = c("barplot","lines"))
pca.loading = word.pca$loadings[,1:55]
word.pc = as.matrix(word.mat)%*%as.matrix(pca.loading)
X.pca = cbind(X[,-(60:259)],word.pc)

###########################################################################
###XGBOOST
library("xgboost")
X = blog.train[,1:p]
X.final = cbind(X[,ind],word.pc)
xgb.fit.pca = xgboost(data = data.matrix(X.final), label = Y.log, max.depth = 4, 
	eta = 0.07, gamma = 0.1, nrounds = 500)

X = X.test[,const]
X.pc = as.matrix(X[,60:259])%*%as.matrix(pca.loading)
X.test.pca = cbind(X.test[,ind],X.pc)
pred.xgb.pca = predict(xgb.fit.pca, data.matrix(X.test.pca))
mse.xgb.pca = mean((pred.xgb.pca - Y.log.test)^2)







