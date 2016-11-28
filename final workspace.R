getwd()
setwd("/Users/stevenhurwitt/Documents/Illinois/Stat 542/FINAL/BlogFeedback")
blog.train = read.csv("blogData_train.csv")

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
colnames(blog.train)[272:281] = c(seq(from=1, to=9, by=1), "y")
colnames(blog.test) = colnames(blog.train)


y = blog.train[,281]
X = blog.train[,-281]

#get sample data
set.seed(314)
data = cbind(X, y)
data.sample = sample_n(data, .01*nrow(X))
X.sample = data.sample[,-281]
y.sample = data.sample[,281]

#resample to test for model robustedness or w/e it's called
#(should do more than twice?)
set.seed(119)
data2 = cbind(X, y)
data.sample2 = sample_n(data2, .01*nrow(X))
X.sample2 = data.sample2[,-281]
y.sample2 = data.sample2[,281]


#models for OG sample:
lars.fit = lars(as.matrix(X.sample), y.sample, type = "lar")
stagewise.fit = lars(as.matrix(X.sample), y.sample, type = "forward.stagewise")
stepwise.fit = lars(as.matrix(X.sample), y.sample, type = "stepwise")
poisson = glm(y~., family = "poisson", data = data.sample)
neg.bin = glm.nb(y~., data = data.sample)

summary(poisson)
summary(neg.bin)

#re-ran models w/ new sample:
poisson2 = glm(y~., family = "poisson", data = data.sample2)
pois.sum2 = summary(poisson)
sig.vars2 = get.sigvars(pois.sum2)

#test code to extract variables w/ p-value < .05
pois.sum = summary(poisson)
sig.vars = get.sigvars(pois.sum)

get.sigvars = function(summary_table){
  betas = coefficients(summary_table)
  cols = colnames(betas)
  cols[4] = "p-value"
  colnames(betas) = cols
  sig.betas = subset(betas, betas[,4] < .05)
  sig.vars = rownames(sig.betas)
  return(sig.vars)
}

sum(sig.vars == sig.vars2)

step(poisson, direction = "forward")

mean(y)
var(y)
hist(y[y < 10])

#zero-inflated poisson would be better than poisson 
#(over-dispersion, excess zeroes, etc)
zero.poiss = zeroinfl(y~., data = data.sample)
zinf.poiss.glm = predict(zero.poiss, blog.test)
summary(zero.poiss)

betas = as.vector(coefficients(poisson))

data.sample.na = data.sample[,!is.na(betas)]
betas.na = betas[!is.na(betas)]

#betas = as.vector(coefficients(poisson))
length(betas.na)
ncol(data.sample.na)

length(data.sample.na)
dim(data.sample.na)
NB = glm.nb(y~., data = data.sample.na, weights = betas.na)
summary(NB)

lin.reg = lm(y~., data = blog.train)
test.preds.lm = predict(lin.reg, blog.test)

#stepwise stuff:
lin.reg.step = step(lin.reg, direction = "forward")


sum(colnames(blog.test) == colnames(blog.train))

beta.step = coefficients(lin.reg.step)
beta.step = beta.step[!is.na(beta.step)]

vars.used.step = names(blog.test) %in% names(beta.step)
#names(vars.used.step)
sum(vars.used.step)
step.test = blog.test[vars.used.step]

beta.step = beta.step[vars.used.step]
X.step = step.test[,-101]
y.step = step.test[,101]
int = rep(1, nrow(X.step))
X.step = cbind(int, X.step)

#evaluation criteria
evaluation = function(pred, actual){
  log.y = log(actual + 1)
  log.pred = log(pred + 1)
  diff = log.y - log.pred
  return(mean(diff^2))
}

NB = glm.nb(y~., data = blog.train)
NB.pred = predict(NB, blog.test)

poisson = glm(y~., family = "poisson", data = blog.train)
poisson.pred = predict(poisson, blog.test)

test.preds.lm[test.preds.lm < 0] = 0
poisson.pred[poisson.pred < 0] = 0

evaluation(poisson.pred, blog.test$y)
evaluation(poisson.pred, blog.test$y)

#13, 33, 38 and 278 have all zeroes, can be dropped
