library(igraph); library(spdep); library(spatialreg); library(car); library(glmnet); library(tseries); library(sp)
sang_election = read.csv(file = "election_couty_cleaned.csv")
load(file = "full_data.Rdata")

############################### Data Filtering ###############################

full_data = full_data[-632,]
sang_election= sang_election[-623,]
ind = intersect(sang_election$fips, full_data$Id2)
y_ind = sapply(ind, function(x){which(x == sang_election$fips)})
X_ind = sapply(ind, function(x){which(x == full_data$Id2)})
y_ind = y_ind[order(as.integer(names(y_ind)))]
sang_election = sang_election[y_ind, ]
X_ind = sort(X_ind)
full_data = full_data[X_ind, ]
logOdds_dem2016 = log(sang_election$pct_dem_16/(1-sang_election$pct_dem_16))
logOdds_dem2012 = log(sang_election$pct_dem_12/(1-sang_election$pct_dem_12))
y = logOdds_dem2016 - logOdds_dem2012
y = scale(y)
Data = cbind(y, full_data[, -c(1: 4)])
X = as.matrix(Data[, -1])
locs = sang_election[, 1: 5]
# save(Data, file = "Data.RData"); save(locs, file = "locs.Rdata")
load("Data.RData"); load("locs.Rdata")
X = as.matrix(Data[, -1]); y = Data[, 1]
############################### Sptial Structure ############################### 

rawData = read.table("county_adjacency.munged.txt", colClasses=c("character", "character"))
G = graph.edgelist(as.matrix(rawData), directed=FALSE)
G = simplify(G, remove.loops=TRUE);V(G)$name=as.numeric(V(G)$name) 
subFIPSid = V(G)$name %in% locs$fips
countyGraph = induced.subgraph(G, V(G)[subFIPSid]);
graph0 = permute(countyGraph, order(V(countyGraph)$name)); ### this is the dense graph to begin with.
nb.mat = as_adjacency_matrix(graph0)
nb.mat = nb.mat[as.vector(sapply(as.character(locs$fips), function(x){which(x == rownames(nb.mat))})), ]
nb.mat = nb.mat[, as.vector(sapply(as.character(locs$fips), function(x){which(x == colnames(nb.mat))}))]
nb.mat = apply(nb.mat, 1, function(x){x/sum(x)})
# dist_mat = spDists(as.matrix(locs[, 1:2]))
# diag(dist_mat) = 1; dist_mat = 1/dist_mat
# nb.mat = nb.mat*dist_mat
nb.w = mat2listw(nb.mat)

############################### Spatial Lasso on Entire Data ###############################

set.seed(1000)
X_lag = nb.mat %*% as.matrix(X)
colnames(X_lag) = paste("Lag", colnames(X_lag))
X_tilda = as.matrix(cbind(X, X_lag))
fit4 = cv.glmnet(x = X_tilda, y = y, family = "gaussian")
residual = as.vector(y - predict(fit4, X_tilda))
shapiro.test(residual)
durbinWatsonTest(residual)
X_tilda = X_tilda[, which(coef(fit4, s = "lambda.1se")[-1] != 0)]

fit5 = lm(y~X_tilda)
summary(fit5)
par(mfrow = c(2,2))
plot(fit5)
dev.off()
shapiro.test(residuals(fit5))
durbinWatsonTest(fit5)
ncvTest(fit5)
gvlma(fit5)
sqrt(mean((residuals(fit5) - mean(residuals(fit5)))^2))

############################### Regression Analysis on Lassoed Data ##############################

# Crude Analysis 
fit = lm(Data$y~., data = Data)
summary(fit)
par(mfrow = c(2,2))
plot(fit)
dev.off()
shapiro.test(residuals(fit))
durbinWatsonTest(fit)
ncvTest(fit)

# spatial SLX Model
fitA = lmSLX(Data$y~., data = Data, nb.w)
summary(fitA)
shapiro.test(residuals(fitA))
lm.morantest(fitA, nb.w, alternative="two.sided")
durbinWatsonTest(fitA)
ncvTest(fitA)
gvlma(fitA)


X_lag = nb.mat %*% as.matrix(X)
colnames(X_lag) = paste("Lag", colnames(X_lag))
X_tilda = as.matrix(cbind(X, X_lag))
fit6 = lm(y~X_tilda)
summary(fit6)
par(mfrow = c(2,2))
plot(fit6)
shapiro.test(residuals(fit6))
durbinWatsonTest(fit6)
ncvTest(fit6)

# Spatial Lag Model

spdep_test = lm.morantest(fit, nb.w, alternative="two.sided")
print(spdep_test)
fit2 = lagsarlm(Data$y~., data = Data, nb.w)
impacts(fit2, nb.w)
summary(fit2)
shapiro.test(residuals(fit2))
durbinWatsonTest(residuals(fit2))
R_square = 1 - sum((residuals(fit2) - mean(residuals(fit2)))^2)/sum((Data$y - mean(Data$y))^2)
RMSE = sqrt(mean((residuals(fit2) - mean(residuals(fit2)))^2))



