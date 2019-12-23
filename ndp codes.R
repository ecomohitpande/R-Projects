=IF(AND(ISBLANK(A26),(B26),(C26),(D26),(E26),(F26),(G26),(H26),(I26),(J26)),"x","-")


# Princ

ipal Component Analysis and Factor Analysis in R
# Copyright 2013 by Ani Katchova

mydata<- read.csv("C:/Econometrics/Data/pca_gsp.csv")
attach(mydata)

# Define variables
X <- cbind(Ag, Mining, Constr, Manuf, Manuf_nd, Transp, Comm, Energy, TradeW, TradeR, RE, Services, Govt)

# Descriptive statistics
summary(X)
cor(X)

# Principal component analysis
pca1 <- princomp(X, scores=TRUE, cor=TRUE)
summary(pca1)

# Loadings of principal components
loadings(pca1)
#pca1$loadings

# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

# Biplot of score variables
biplot(pca1)

# Scores of the components
pca1$scores[1:10,]

# Rotation
#varimax(pca1$rotation)
#promax(pca1$rotation)


# Factor analysis - different results from other softwares and no rotation
fa1 <- factanal(X, factor=3)
fa1

fa2 <- factanal(X, factor=3, rotation="varimax")
fa2

fa3 <- factanal(X, factors=3, rotation="varimax", scores="regression")
fa3

library(rdrobust)
attach(rdrobust_senate)
summary(rdrobust_senate)
rdplot(y=vote, x=margin, binselect="es", ci=95, 
       title="RD Plot: U.S. Senate Election Data", 
       y.label="Vote Share in Election at time t+2",
       x.label="Vote Share in Election at time t")


### rdplot with MSE-optimal choice
rdplot(y=vote, x=margin, binselect="es", 
       title="RD Plot: U.S. Senate Election Data", 
       y.label="Vote Share in Election at time t+2",
       x.label="Vote Share in Election at time t")

### rdplot with QS partitioning and mimicking variance choice
rdplot(y=vote, x=margin, binselect="qsmv", 
       title="RD Plot: U.S. Senate Election Data", 
       y.label="Vote Share in Election at time t+2",
       x.label="Vote Share in Election at time t")

### rdrobust 
summary(rdrobust(y=vote, x=margin))

### rdrobust with all estimates
summary(rdrobust(y=vote, x=margin, all=TRUE))

## rdrobust backward compatibility
summary(rdrobust(y=vote, x=margin, h=16.79369, b=27.43745))

## rdplot to show rdrobust estimate
est <- rdrobust(y=vote, x=margin)
rdplot(y=vote, x=margin, subset=-est$h_l<= margin & margin <= est$h_r,
       binselect="esmv", kernel="triangular", h=c(est$h_l, est$h_r), p=1,
       title="RD Plot: U.S. Senate Election Data", 
       y.label="Vote Share in Election at time t+2",
       x.label="Vote Share in Election at time t")


## rdrobust with covariates within the same window (i.e., using same bandwidths)
est1 <- rdrobust(y=vote, x=margin)
len1 <- est1$ci[3,2] - est1$ci[3,1]
est2 <- rdrobust(y=vote, x=margin, covs=cbind(class,termshouse,termssenate), h=c(est$h_l,est$h_r), b=c(est$b_l, est$b_r))
len2 <- est2$ci[3,2] - est2$ci[3,1]
paste("CI length change: ", round((len2/len1-1)*100,2), "%")


## rdrobust with covariates with data-driven optimal bandwidths
est1 <- rdrobust(y=vote, x=margin)
len1 <- est1$ci[3,2] - est1$ci[3,1]
est2 <- rdrobust(y=vote, x=margin, covs=cbind(class,termshouse,termssenate))
len2 <- est2$ci[3,2] - est2$ci[3,1]
paste("CI length change: ", round((len2/len1-1)*100,2), "%")

## rdrobust with useless covariate
est1 <- rdrobust(y=vote, x=margin)
len1 <- est1$ci[3,2] - est1$ci[3,1]
est2 <- rdrobust(y=vote, x=margin, covs=cbind(population))
len2 <- est2$ci[3,2] - est2$ci[3,1]
paste("CI length change: ", round((len2/len1-1)*100,2), "%")

## rdrobust check covariate "balanced"
covs <- cbind(class, termshouse, termssenate, population)
balance <- matrix(NA,4,2)
for (z in 1:ncol(covs)) {
  est <- rdrobust(y=covs[,z], x=margin)
  balance[z,1] = est$Estimate[,"tau.us"]
  balance[z,2] = est$pv[3]
}
rownames(balance) = c("class", "termshouse", "termssenate", "population")
colnames(balance) = c("RD Effect", "Robust p-val")
print(balance)

## rdrobust with clustering
summary(rdrobust(y=vote, x=margin, vce="nn", cluster=state))

## rdrobust with clustering and covariates, and different bandwidth
summary(rdrobust(y=vote, x=margin, vce="nn", bwselect="msetwo", covs=cbind(class,termshouse,termssenate), cluster=state))

## rdbwselect with all estimates
summary(rdbwselect(y=vote, x=margin, all=TRUE))

## Other examples
summary(rdrobust(y=vote, x=margin, kernel="uniform", vce="hc1", cluster=state))
summary(rdrobust(y=vote, x=margin, bwselect="certwo", vce="hc3"))
summary(rdrobust(y=vote, x=margin, h=c(12,15), b=c(18,20)))
summary(rdrobust(y=vote, x=margin, covs=cbind(class), bwselect="cerrd", scaleregul=0, rho=1))
summary(rdbwselect(y=vote, x=margin, kernel="uniform", vce="hc1", cluster=state, all=TRUE))
summary(rdbwselect(y=vote, x=margin, covs=cbind(class), bwselect="msetwo", vce="hc2", all=TRUE))



library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)
library(TeachingDemos)
attach(data)
options(width=280)
par(mar = rep(2, 4))

tryCatch(dir.create("outputs"))


Y = data$Y
X = data$X
T = data$T
T_X = T*X


# Raw comparison of means
pdf("./outputs/Vol-1-R_RDplot-Meyersson-naive-p0.pdf")
rdplot(Y, X, nbins = c(2500, 500), p = 0, col.lines = "red", col.dots = "black", title = "", 
       x.label = "Islamic Margin of Victory", y.label = "Female High School Percentage", y.lim = c(0,70), cex.axis = 1.5,
       cex.lab = 1.5)
dev.off()

# Figure 2.3b
# Local comparison of means
pdf("./outputs/Vol-1-R_RDplot-Meyersson-naive-p4.pdf")
rdplot(Y[abs(X) <= 50], X[abs(X) <= 50], nbins = c(2500, 300), p = 4, col.lines = "red", col.dots = "black", title = "", 
x.label = "Islamic Margin of Victory", y.label = "Female High School Percentage", y.lim = c(0,70), cex.axis = 1.5,
cex.lab = 1.5, cutpoint= -20,-20)
dev.off()

#-----------#
# Section 3 #
# RD Plots  #
#-----------#
# Snippet 3.1 (Figure 3.1)
# Scatter plot
txtStart("./outputs/Vol-1-R_meyersson_rdplot_raw.txt", commands = TRUE, results = FALSE, append = FALSE, visible.only = TRUE)
plot(X, Y, xlab = "Score", ylab = "Outcome", col = 1, pch = 20, cex.axis = 1.5, cex.lab = 1.5)
abline(v=0)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_raw.pdf")
plot(X, Y, xlab = "Score", ylab = "Outcome", col = 1, pch = 20, cex.axis = 1.5, cex.lab = 1.5)
abline(v=0)
dev.off()

# Figure 3.2
# RD plot using 40 bins of equal length
txtStart("./outputs/Vol-1-R_meyersson_rdplot_esmv_20bins.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X, nbins = c(20,20), binselect = 'esmv', y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_esmv_20bins.pdf")
rdplot(Y, X, nbins = c(20,20), binselect = 'esmv', x.label = 'Score', y.label = 'Outcome', title = '',
       y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.2 (Figure 3.3a)
# 40 Evenly-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_es_20bins.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X, nbins = c(20,20), binselect = 'es', y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_es_20bins.pdf")
rdplot(Y, X, nbins = c(20,20), binselect = 'es', x.label = 'Score', y.label = 'Outcome', title = '',
       y.lim = c(0,20), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.3 (Figure 3.3b)
# 40 Quantile-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_qs_20bins.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X, nbins = c(20,20), binselect = 'qs', x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_qs_20bins.pdf")
rdplot(Y, X, nbins = c(20,20), binselect = 'qs', x.label = 'Score', y.label = 'Outcome', title = '', 
       x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.4 (Figure 3.4)
# IMSE RD plot with evenly-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_es.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X,  binselect = 'es', x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_es.pdf")
rdplot(Y, X,  binselect = 'es', x.label = 'Score', y.label = 'Outcome', title = '', 
       x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.5 (Figure 3.5)
# IMSE RD plot with quantile-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_qs.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X,  binselect = 'qs', x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_qs.pdf")
rdplot(Y, X,  binselect = 'qs', x.label = 'Score', y.label = 'Outcome', title = '', 
       x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.6 (Figure 3.6)
# Mimicking variance RD plot with evenly-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_esmv.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X,  binselect = 'esmv', cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_esmv.pdf")
rdplot(Y, X,  binselect = 'esmv', x.label = 'Score', y.label = 'Outcome', title = '', 
       cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.7 (Figure 3.7)
# Mimicking variance RD plot with quantile-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_qsmv.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X,  binselect = 'qsmv', x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_qsmv.pdf")
rdplot(Y, X,  binselect = 'qsmv', x.label = 'Score', y.label = 'Outcome', title = '',
       x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()


txtStart("./outputs/Vol-1-R_meyersson_manualreg_tworegs_uniform_adhoc_p1.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = lm(Y[X < 0 & X >= -20] ~ X[X < 0 & X >= -20])
left_intercept = out$coefficients[1]
print(left_intercept)
out = lm(Y[X >= 0 & X <= 20] ~ X[X >= 0 & X <= 20])
right_intercept = out$coefficients[1]
print(right_intercept)
difference = right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))
txtStop()

# Snippet 4.2
# Using one regression to estimate
txtStart("./outputs/Vol-1-R_meyersson_manualreg_onereg_uniform_adhoc_p1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
T_X = X * T
out = lm(Y[X >= -20 & X <= 20] ~ X[X >= -20 & X <= 20] + T[X >= -20 & X <= 20] + T_X[X >= -20 & X <= 20])
summary(out)
txtStop()

# Snippet 4.3
# Generating triangular weights
txtStart("./outputs/Vol-1-R_meyersson_manualreg_weights_triangular_adhoc_p1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
w = NA
w[X < 0 & X >= -20] = 1 - abs(X[X < 0 & X >= -20] / 20)
w[X >= 0 & X <= 20] = 1 - abs(X[X >= 0 & X <= 20] / 20)
txtStop()

# Snippet 4.4
# Using two regressions and weights to estimate
txtStart("./outputs/Vol-1-R_meyersson_manualreg_tworegs_triangular_adhoc_p1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = lm(Y[X < 0] ~ X[X < 0], weights = w[X < 0])
left_intercept = out$coefficients[1]
out = lm(Y[X >= 0] ~ X[X >= 0], weights = w[X >= 0])
right_intercept = out$coefficients[1]
difference = right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))
txtStop()

# Snippet 4.5
# Using rdrobust with uniform weights
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_uniform_adhoc_p1_rho1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'uniform',  p = 1, h = 20)
summary(out)
txtStop()

# Snippet 4.6
# Using rdrobust with triangular weights
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_adhoc_p1_rho1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, h = 20,)
summary(out)
txtStop()

# Snippet 4.7
# Using rdrobust with triangular weights and p = 2
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_adhoc_p2_rho1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 2, h = 20)
summary(out)
txtStop()

# Snippet 4.8
# Using rdbwselect with mserd bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_mserd_p1_regterm1_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdbwselect(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.9
# Using rdbwselect with msetwo bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_msetwo_p1_regterm1_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdbwselect(Y, X, kernel = 'triangular',  p = 1, bwselect = 'msetwo')
summary(out)
txtStop()

# Snippet 4.10
# Using rdrobust with mserd bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.11
# Using rdrobust to show the objects it returns
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1_namescoefsout_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
rdout = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'mserd')
print(names(rdout)[1:7])
print(names(rdout)[8:15])
print(names(rdout)[16:23])
print(names(rdout)[24:27])
print(rdout$beta_p_r)
print(rdout$beta_p_l)
txtStop()

# Snippet 4.12
# Using rdrobust and showing the associated rdplot
txtStart("./outputs/Vol-1-R_meyersson_rdplot_maineffect.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
bandwidth = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'mserd')$bws[1,1]
out = rdplot(Y[abs(X) <= bandwidth], X[abs(X) <= bandwidth], p = 1, kernel = 'triangular', cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

# Figure 4.4
# Local polynomial RD effect illustrated with rdplot-Meyersson data
pdf("./outputs/Vol-1-R_meyersson_rdplot_maineffect.pdf")
rdplot(Y[abs(X)<=bandwidth], X[abs(X)<=bandwidth], p = 1, kernel = 'triangular',
       x.label = 'Score', y.label = 'Outcome', title = '', y.lim = c(10,22), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 4.13
# Using rdrobust without regularization term
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm0.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular', scaleregul = 0,  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.14
# Using rdrobust with default options
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.15
# Using rdrobust with default options and showing all the output
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd', all = TRUE)
summary(out)
txtStop()

# Snippet 4.16
# Using rdrobust with cerrd bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_cerrd_p1_rhofree_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'cerrd')
summary(out)
txtStop()

# Snippet 4.17
# Using rdbwselect with all the bandwidths
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_all_p1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdbwselect(Y, X, kernel = 'triangular', p = 1, all = TRUE)
summary(out)
txtStop()

# Snippet 4.18
# Using rdbwselect with covariates
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_mserd_p1_regterm1_covariates_noi89.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdbwselect(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.19
# Using rdrobust with covariates
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_covariates_noi89.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.20
# Using rdrobust with clusters
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_clusters.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd', cluster = data$prov_num)
summary(out)
txtStop()

# Snippet 4.21
# Using rdrobust with clusters and covariates
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_covariates_noi89_clusters.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd', cluster = data$prov_num)
summary(out)
txtStop()

#-----------------------------------------------#
# Section 5                                     #
# Validation and Falsification of the RD Design #
#-----------------------------------------------#
# Figure 5.1
# RD plots for predetermined covariates
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_lpop1994.pdf")
rdplot(data$lpop1994, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_partycount.pdf")
rdplot(data$partycount, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_vshr_islam1994.pdf")
rdplot(data$vshr_islam1994, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_i89.pdf")
rdplot(data$i89, X,
       x.label = "Score", y.label = "", title = "", x.lim = c(-100,100), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_merkezp.pdf")
rdplot(data$merkezp, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_merkezi.pdf")
rdplot(data$merkezi, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 5.1
# Using rdrobust on lpop1994
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_lpop1994.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(data$lpop1994, X)
summary(out)
txtStop()

# Snippet 5.2
# Using rdplot to show the rdrobust effect for lpop1994
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_lpop1994.txt",
         commands = TRUE, results = FALSE, append = FALSE, visible.only = TRUE)
bandwidth = rdrobust(data$lpop1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
txtStop()

# Figure 5.2
# Graphical illustration of local linear RD effects for predetermined covariates
bandwidth = rdrobust(data$lpop1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_lpop1994.pdf")
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

bandwidth = rdrobust(data$partycount, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_partycount.pdf")
rdplot(data$partycount[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

bandwidth = rdrobust(data$vshr_islam1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_vshr_islam1994.pdf")
rdplot(data$vshr_islam1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

bandwidth = rdrobust(data$i89, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_i89.pdf")
rdplot(data$i89[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 5.3
# Binomial test
txtStart("./outputs/Vol-1-R_meyersson_falsification_binomial_byhand_adhoc.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
binom.test(53, 100, 1/2)
txtStop()

# Snippet 5.4
# Using rddensity
txtStart("./outputs/Vol-1-R_meyersson_falsification_rddensity.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rddensity(X)
summary(out)
txtStop()

# Figure 5.4a
# Histogram
bw_left = as.numeric(rddensity(X)$h[1]); bw_right = as.numeric(rddensity(X)$h[2]);
tempdata = as.data.frame(X); colnames(tempdata) = c("v1");
plot2 = ggplot(data=tempdata, aes(tempdata$v1)) + theme_bw(base_size = 17) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(-bw_left, 0, 1), fill = "blue", col = "black", alpha = 1) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(0, bw_right, 1), fill = "red", col = "black", alpha = 1) +
  labs(x = "Score", y = "Number of Observations") + geom_vline(xintercept = 0, color = "black")
plot2
ggsave("./outputs/Vol-1-R_meyersson_falsification_lpdensity2.pdf", plot = plot2, width = 6, height = 5, units = "in")

# Figure 5.4b
# Estimated Density
est1 = lpdensity(data = X[X < 0 & X >= -bw_left], grid = seq(-bw_left, 0, 0.1), bwselect = "IMSE",
                 scale = sum(X < 0 & X >= -bw_left) / length(X))
est2 = lpdensity(data = X[X >= 0 & X <= bw_right], grid = seq(0, bw_right, 0.1), bwselect = "IMSE",
                 scale = sum(X >= 0 & X <= bw_right) / length(X))
plot1 = lpdensity.plot(est1, est2, CIshade = 0.2, lcol = c(4, 2), CIcol = c(4, 2), legendGroups = c("Control", "Treatment"))+
  labs(x = "Score", y = "Density") + geom_vline(xintercept = 0, color = "black") +
  theme_bw(base_size = 17)+theme(legend.position = c(0.8, 0.85))
plot1
ggsave("./outputs/Vol-1-R_meyersson_falsification_lpdensity1.pdf", plot = plot1, width = 6, height = 5, units = "in")

# Snippet 5.5
# Using rdrobust with the cutoff equal to 1
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_alternative-cutoff_c1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y[X >= 0], X[X >= 0], c = 1)
summary(out)
txtStop()

# Snippet 5.6
# Using rdrobust for the donut-hole approach
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_donuthole.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y[abs(X) >= 0.3], X[abs(X) >= 0.3])
summary(out)
txtStop()
ndp <- read.csv("C:/Users/mpandey/Downloads/ndp.csv")
attach(ndp)
str(ndp)
library(dplyr)
ndp %>% select(c(-1,-29))-> ndp ### sample to be consider 
ndp$HH_Resp_Gender<- factor(ndp$HH_Resp_Gender, labels=c("Female", "Male", "Not define")) ## be factorise 

library(ggplot1)
library("ggplot2", lib.loc="~/R/win-library/3.6")
ggplot(data=ndp, aes(x=incomeSrc_DairyFarm))+geom_histogram(bin=40)
ggplot(data=ndp, aes(x=incomeSrc_DairyFarm))+geom_histogram(bin=40, 
fill="lightblue", col="blue")

ggplot(data=ndp, aes(y=incomeSrc_DairyFarm, x=HH_Resp_Gender))+geom_histogram, fill="lightblue", col="blue")

ggplot(data=ndp, aes(y=incomeSrc_DairyFarm,x=HH_Resp_Gender, fill=HH_Resp_Gender))+geom_boxplot()

library(caTools)
sample.split(ndp$incomeSrc_DairyFarm,SplitRatio = 0.65)->split_index
sample.split(ndp$incomeSrc_DairyFarm,SplitRatio = 0.65)->split_index
train<-subset(ndp,split_index==T)
train<-subset(ndp,split_index==F)
nrow(train)
nrow(test)
lm(incomeSrc_DairyFarm~ AdultMilchFem_PricePurch_01) 
######## or we can use the plot before the lm command 


mydata<- read.csv("C:/Econometrics/Data/pca_gsp.csv")
attach(ndp)


X <- cbind(incomeSrc_DairyFarm,	incomeSrc_Crops,	incomeSrc_LivStock_NonDairy,	incomeSrc_FishAqua,	
           incomeSrc_FarmLabour,	incomeSrc_NonFarmDailyLabour,	incomeSrc_SkilledLbour,	incomeSrc_regJob,	
           incomeSrc_Remit,	incomeSrc_Pension,	incomeSrc_Business,	incomeSrc_HandCraft,	
           incomeSrc_OtherNonFarm,	incomeSrc_Grants)
)

# Descriptive statistics
summary(X)
cor(X)

############################ codes forthe missing value 
y <- c(1,2,3,NA)
ndp$incomeSrc_DairyFarm[ndp$incomeSrc_DairyFarm==99] <- NA
x <- c(1,2,NA,3)
ndp[!complete.cases(ndp),]
newdata <- na.omit(ndp)

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data3 <- data2[order(data2$incomeSrc_DairyFarm, data2$incomeSrc_Crops),]

#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),]

ndp1[ndp1==0] <- NA
data2<-ndp1[complete.cases(ndp1),]

####################for the exporting the data from R to excell

library(openxlsx)
write.xlsx(data3, 'name-of-your-excel-file.xlsx')

pca1 <- princomp(X, scores=TRUE, cor=TRUE)
summary(pca1)

# Loadings of principal components
loadings(pca1)
#pca1$loadings

# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

# Biplot of score variables
biplot(pca1)

# Scores of the components
pca1$scores[1:10,]

# Rotation
#varimax(pca1$rotation)
#promax(pca1$rotation)


# Factor analysis - different results from other softwares and no rotation
fa1 <- factanal(X, factor=3)
fa1

fa2 <- factanal(X, factor=3, rotation="varimax")
fa2

fa3 <- factanal(X, factors=3, rotation="varimax", scores="regression")
fa3

############################################################################

library(foreign)
library(ggplot2)
library(lpdensity)
library(rddensity)
library(rdrobust)
library(rdlocrand)
library(TeachingDemos)
attach(ndp)
options(width=280)
par(mar = rep(2, 4))

tryCatch(dir.create("outputs"))


Y = ndp$incomeSrc_DairyFarm
X = ndp$AvgMilkProd_Qty_Summer_BETP

T = data$T
T_X = T*X


# Raw comparison of means
pdf("./outputs/Vol-1-R_RDplot-Meyersson-naive-p0.pdf")
rdplot(Y, X, nbins = c(0,200), p = 0, col.lines = "red", col.dots = "black", title = "", 
       x.label = "AvgMilkProd_Qty_Summer_BETP", y.label = "incomeSrc_DairyFarm", y.lim = c(0,70), cex.axis = 1.5,
       cex.lab = 1.5)
dev.off()

# Figure 2.3b
# Local comparison of means
pdf("./outputs/Vol-1-R_RDplot-Meyersson-naive-p4.pdf")
rdplot(Y[abs(X) <= 50], X[abs(X) <= 50], nbins = c(2500, 300), p = 4, col.lines = "red", col.dots = "black", title = "", 
x.label = "Islamic Margin of Victory", y.label = "Female High School Percentage", y.lim = c(0,70), cex.axis = 1.5,
cex.lab = 1.5, cutpoint= -1,1)
dev.off()

#-----------#
# Section 3 #
# RD Plots  #
#-----------#
# Snippet 3.1 (Figure 3.1)
# Scatter plot
txtStart("./outputs/Vol-1-R_meyersson_rdplot_raw.txt", commands = TRUE, results = FALSE, append = FALSE, visible.only = TRUE)
plot(X, Y, xlab = "Score", ylab = "Outcome", col = 1, pch = 20, cex.axis = 1.5, cex.lab = 1.5)
abline(v=0)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_raw.pdf")
plot(X, Y, xlab = "Score", ylab = "Outcome", col = 1, pch = 20, cex.axis = 1.5, cex.lab = 1.5)
abline(v=0)
dev.off()

# Figure 3.2
# RD plot using 40 bins of equal length
txtStart("./outputs/Vol-1-R_meyersson_rdplot_esmv_20bins.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X, nbins = c(20,20), binselect = 'esmv', y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_esmv_20bins.pdf")
rdplot(Y, X, nbins = c(20,20), binselect = 'esmv', x.label = 'Score', y.label = 'Outcome', title = '',
       y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.2 (Figure 3.3a)
# 40 Evenly-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_es_20bins.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X, nbins = c(20,20), binselect = 'es', y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_es_20bins.pdf")
rdplot(Y, X, nbins = c(20,20), binselect = 'es', x.label = 'Score', y.label = 'Outcome', title = '',
       y.lim = c(0,20), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.3 (Figure 3.3b)
# 40 Quantile-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_qs_20bins.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X, nbins = c(20,20), binselect = 'qs', x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_qs_20bins.pdf")
rdplot(Y, X, nbins = c(20,20), binselect = 'qs', x.label = 'Score', y.label = 'Outcome', title = '', 
       x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.4 (Figure 3.4)
# IMSE RD plot with evenly-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_es.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X,  binselect = 'es', x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_es.pdf")
rdplot(Y, X,  binselect = 'es', x.label = 'Score', y.label = 'Outcome', title = '', 
       x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.5 (Figure 3.5)
# IMSE RD plot with quantile-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_qs.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X,  binselect = 'qs', x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_qs.pdf")
rdplot(Y, X,  binselect = 'qs', x.label = 'Score', y.label = 'Outcome', title = '', 
       x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.6 (Figure 3.6)
# Mimicking variance RD plot with evenly-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_esmv.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X,  binselect = 'esmv', cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_esmv.pdf")
rdplot(Y, X,  binselect = 'esmv', x.label = 'Score', y.label = 'Outcome', title = '', 
       cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 3.7 (Figure 3.7)
# Mimicking variance RD plot with quantile-spaced bins
txtStart("./outputs/Vol-1-R_meyersson_rdplot_qsmv.txt", commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdplot(Y, X,  binselect = 'qsmv', x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

pdf("./outputs/Vol-1-R_meyersson_rdplot_qsmv.pdf")
rdplot(Y, X,  binselect = 'qsmv', x.label = 'Score', y.label = 'Outcome', title = '',
       x.lim = c(-100,100), y.lim = c(0,25), cex.axis = 1.5, cex.lab = 1.5)
dev.off()


txtStart("./outputs/Vol-1-R_meyersson_manualreg_tworegs_uniform_adhoc_p1.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = lm(Y[X < 0 & X >= -20] ~ X[X < 0 & X >= -20])
left_intercept = out$coefficients[1]
print(left_intercept)
out = lm(Y[X >= 0 & X <= 20] ~ X[X >= 0 & X <= 20])
right_intercept = out$coefficients[1]
print(right_intercept)
difference = right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))
txtStop()

# Snippet 4.2
# Using one regression to estimate
txtStart("./outputs/Vol-1-R_meyersson_manualreg_onereg_uniform_adhoc_p1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
T_X = X * T
out = lm(Y[X >= -20 & X <= 20] ~ X[X >= -20 & X <= 20] + T[X >= -20 & X <= 20] + T_X[X >= -20 & X <= 20])
summary(out)
txtStop()

# Snippet 4.3
# Generating triangular weights
txtStart("./outputs/Vol-1-R_meyersson_manualreg_weights_triangular_adhoc_p1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
w = NA
w[X < 0 & X >= -20] = 1 - abs(X[X < 0 & X >= -20] / 20)
w[X >= 0 & X <= 20] = 1 - abs(X[X >= 0 & X <= 20] / 20)
txtStop()

# Snippet 4.4
# Using two regressions and weights to estimate
txtStart("./outputs/Vol-1-R_meyersson_manualreg_tworegs_triangular_adhoc_p1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = lm(Y[X < 0] ~ X[X < 0], weights = w[X < 0])
left_intercept = out$coefficients[1]
out = lm(Y[X >= 0] ~ X[X >= 0], weights = w[X >= 0])
right_intercept = out$coefficients[1]
difference = right_intercept - left_intercept
print(paste("The RD estimator is", difference, sep = " "))
txtStop()

# Snippet 4.5
# Using rdrobust with uniform weights
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_uniform_adhoc_p1_rho1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'uniform',  p = 1, h = 20)
summary(out)
txtStop()

# Snippet 4.6
# Using rdrobust with triangular weights
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_adhoc_p1_rho1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, h = 20,)
summary(out)
txtStop()

# Snippet 4.7
# Using rdrobust with triangular weights and p = 2
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_adhoc_p2_rho1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 2, h = 20)
summary(out)
txtStop()

# Snippet 4.8
# Using rdbwselect with mserd bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_mserd_p1_regterm1_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdbwselect(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.9
# Using rdbwselect with msetwo bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_msetwo_p1_regterm1_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdbwselect(Y, X, kernel = 'triangular',  p = 1, bwselect = 'msetwo')
summary(out)
txtStop()

# Snippet 4.10
# Using rdrobust with mserd bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.11
# Using rdrobust to show the objects it returns
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1_namescoefsout_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
rdout = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'mserd')
print(names(rdout)[1:7])
print(names(rdout)[8:15])
print(names(rdout)[16:23])
print(names(rdout)[24:27])
print(rdout$beta_p_r)
print(rdout$beta_p_l)
txtStop()

# Snippet 4.12
# Using rdrobust and showing the associated rdplot
txtStart("./outputs/Vol-1-R_meyersson_rdplot_maineffect.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
bandwidth = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'mserd')$bws[1,1]
out = rdplot(Y[abs(X) <= bandwidth], X[abs(X) <= bandwidth], p = 1, kernel = 'triangular', cex.axis = 1.5, cex.lab = 1.5)
summary(out)
txtStop()

# Figure 4.4
# Local polynomial RD effect illustrated with rdplot-Meyersson data
pdf("./outputs/Vol-1-R_meyersson_rdplot_maineffect.pdf")
rdplot(Y[abs(X)<=bandwidth], X[abs(X)<=bandwidth], p = 1, kernel = 'triangular',
       x.label = 'Score', y.label = 'Outcome', title = '', y.lim = c(10,22), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 4.13
# Using rdrobust without regularization term
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm0.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular', scaleregul = 0,  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.14
# Using rdrobust with default options
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.15
# Using rdrobust with default options and showing all the output
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1_all.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular',  p = 1, bwselect = 'mserd', all = TRUE)
summary(out)
txtStop()

# Snippet 4.16
# Using rdrobust with cerrd bandwidth
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_cerrd_p1_rhofree_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular', p = 1, bwselect = 'cerrd')
summary(out)
txtStop()

# Snippet 4.17
# Using rdbwselect with all the bandwidths
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_all_p1_regterm1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdbwselect(Y, X, kernel = 'triangular', p = 1, all = TRUE)
summary(out)
txtStop()

# Snippet 4.18
# Using rdbwselect with covariates
txtStart("./outputs/Vol-1-R_meyersson_rdbwselect_triangular_mserd_p1_regterm1_covariates_noi89.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdbwselect(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.19
# Using rdrobust with covariates
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_covariates_noi89.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd')
summary(out)
txtStop()

# Snippet 4.20
# Using rdrobust with clusters
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_clusters.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y, X, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd', cluster = data$prov_num)
summary(out)
txtStop()

# Snippet 4.21
# Using rdrobust with clusters and covariates
txtStart("./outputs/Vol-1-R_meyersson_rdrobust_triangular_mserd_p1_regterm1_covariates_noi89_clusters.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
Z = cbind(data$vshr_islam1994, data$partycount, data$lpop1994,
          data$merkezi, data$merkezp, data$subbuyuk, data$buyuk)
colnames(Z) = c("vshr_islam1994", "partycount", "lpop1994",
                "merkezi", "merkezp", "subbuyuk", "buyuk")
out = rdrobust(Y, X, covs = Z, kernel = 'triangular', scaleregul = 1, p = 1, bwselect = 'mserd', cluster = data$prov_num)
summary(out)
txtStop()

#-----------------------------------------------#
# Section 5                                     #
# Validation and Falsification of the RD Design #
#-----------------------------------------------#
# Figure 5.1
# RD plots for predetermined covariates
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_lpop1994.pdf")
rdplot(data$lpop1994, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_partycount.pdf")
rdplot(data$partycount, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_vshr_islam1994.pdf")
rdplot(data$vshr_islam1994, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_i89.pdf")
rdplot(data$i89, X,
       x.label = "Score", y.label = "", title = "", x.lim = c(-100,100), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_merkezp.pdf")
rdplot(data$merkezp, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_merkezi.pdf")
rdplot(data$merkezi, X,
       x.label = "Score", y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 5.1
# Using rdrobust on lpop1994
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_lpop1994.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(data$lpop1994, X)
summary(out)
txtStop()

# Snippet 5.2
# Using rdplot to show the rdrobust effect for lpop1994
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_lpop1994.txt",
         commands = TRUE, results = FALSE, append = FALSE, visible.only = TRUE)
bandwidth = rdrobust(data$lpop1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
txtStop()

# Figure 5.2
# Graphical illustration of local linear RD effects for predetermined covariates
bandwidth = rdrobust(data$lpop1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_lpop1994.pdf")
rdplot(data$lpop1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

bandwidth = rdrobust(data$partycount, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_partycount.pdf")
rdplot(data$partycount[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

bandwidth = rdrobust(data$vshr_islam1994, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_vshr_islam1994.pdf")
rdplot(data$vshr_islam1994[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

bandwidth = rdrobust(data$i89, X)$bws[1,1]
xlim = ceiling(bandwidth)
pdf("./outputs/Vol-1-R_meyersson_falsification_rdplot_rdrobust_i89.pdf")
rdplot(data$i89[abs(X) <= bandwidth], X[abs(X) <= bandwidth],
       p = 1, kernel = 'triangular', x.lim = c(-xlim, xlim), x.label = "Score",
       y.label = "", title = "", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

# Snippet 5.3
# Binomial test
txtStart("./outputs/Vol-1-R_meyersson_falsification_binomial_byhand_adhoc.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
binom.test(53, 100, 1/2)
txtStop()

# Snippet 5.4
# Using rddensity
txtStart("./outputs/Vol-1-R_meyersson_falsification_rddensity.txt", 
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rddensity(X)
summary(out)
txtStop()

# Figure 5.4a
# Histogram
bw_left = as.numeric(rddensity(X)$h[1]); bw_right = as.numeric(rddensity(X)$h[2]);
tempdata = as.data.frame(X); colnames(tempdata) = c("v1");
plot2 = ggplot(data=tempdata, aes(tempdata$v1)) + theme_bw(base_size = 17) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(-bw_left, 0, 1), fill = "blue", col = "black", alpha = 1) +
  geom_histogram(data = tempdata, aes(x = v1, y= ..count..), breaks = seq(0, bw_right, 1), fill = "red", col = "black", alpha = 1) +
  labs(x = "Score", y = "Number of Observations") + geom_vline(xintercept = 0, color = "black")
plot2
ggsave("./outputs/Vol-1-R_meyersson_falsification_lpdensity2.pdf", plot = plot2, width = 6, height = 5, units = "in")

# Figure 5.4b
# Estimated Density
est1 = lpdensity(data = X[X < 0 & X >= -bw_left], grid = seq(-bw_left, 0, 0.1), bwselect = "IMSE",
                 scale = sum(X < 0 & X >= -bw_left) / length(X))
est2 = lpdensity(data = X[X >= 0 & X <= bw_right], grid = seq(0, bw_right, 0.1), bwselect = "IMSE",
                 scale = sum(X >= 0 & X <= bw_right) / length(X))
plot1 = lpdensity.plot(est1, est2, CIshade = 0.2, lcol = c(4, 2), CIcol = c(4, 2), legendGroups = c("Control", "Treatment"))+
  labs(x = "Score", y = "Density") + geom_vline(xintercept = 0, color = "black") +
  theme_bw(base_size = 17)+theme(legend.position = c(0.8, 0.85))
plot1
ggsave("./outputs/Vol-1-R_meyersson_falsification_lpdensity1.pdf", plot = plot1, width = 6, height = 5, units = "in")

# Snippet 5.5
# Using rdrobust with the cutoff equal to 1
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_alternative-cutoff_c1.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y[X >= 0], X[X >= 0], c = 1)
summary(out)
txtStop()

# Snippet 5.6
# Using rdrobust for the donut-hole approach
txtStart("./outputs/Vol-1-R_meyersson_falsification_rdrobust_donuthole.txt",
         commands = TRUE, results = TRUE, append = FALSE, visible.only = TRUE)
out = rdrobust(Y[abs(X) >= 0.3], X[abs(X) >= 0.3])
summary(out)
txtStop()
