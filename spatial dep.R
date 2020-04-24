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
locs = sang_election[, 1: 5]

fit.lasso = cv.glmnet(x = as.matrix(Data[, -1]), y = y, family = "gaussian")
coef_lasso = coef(fit.lasso, s = "lambda.min")
Data = Data[, c(1, (which(coef_lasso[-1] != 0)+1))]

# save(Data, file = "Data.RData"); save(locs, file = "locs.Rdata")
# load("Data.RData"); load("locs.Rdata") 
############################### Sptial Structure ############################### 

rawData = read.table("county_adjacency.munged.txt", colClasses=c("character", "character"))
G = graph.edgelist(as.matrix(rawData), directed=FALSE)
G = simplify(G, remove.loops=TRUE);V(G)$name=as.numeric(V(G)$name) 
subFIPSid = V(G)$name %in% locs$fips
countyGraph = induced.subgraph(G, V(G)[subFIPSid]);
graph0 = permute(countyGraph, order(V(countyGraph)$name)); ### this is the dense graph to begin with.
nb.mat = as_adjacency_matrix(graph0)
# dist_mat = spDists(as.matrix(locs[, 1:2]))
# nb.mat = nb.mat[as.vector(sapply(as.character(locs$fips), function(x){which(x == rownames(nb.mat))})), ]
# nb.mat = nb.mat[, as.vector(sapply(as.character(locs$fips), function(x){which(x == colnames(nb.mat))}))]
# nb.mat = nb.mat*dist_mat
# nb.mat = dist_mat
nb.mat = apply(nb.mat, 2, function(x){x/sum(x)})
nb.w = mat2listw(nb.mat)

############################### Regression Analysis ##############################

# Crude Analysis 
fit = lm(Data$y~., data = Data)
summary(fit)
par(mforw = c(2,2))
plot()
shapiro.test(residuals(fit))
durbinWatsonTest(fit)
ncvTest(fit)


spdep_test = lm.morantest(fit, nb.w, alternative="two.sided")
print(spdep_test)

fit2 = lagsarlm(Data$y~., data = Data, nb.w)
summary(fit2)
hist(residuals(fit2))
shapiro.test(residuals(fit2))
R_square = 1 - sum((residuals(fit2) - mean(residuals(fit2)))^2)/sum((Data$y - mean(Data$y))^2)
1-((1-R_square)*(nrow(Data)-1)/(nrow(Data)-ncol(Data)-1))
sqrt(mean((residuals(fit2) - mean(residuals(fit2)))^2))

fit3 = errorsarlm(Data$y~., data = Data, nb.w)
summary(fit3)
shapiro.test(residuals(fit3))
1 - sum((residuals(fit3) - mean(residuals(fit3)))^2)/sum((y - mean(y))^2)

sqrt(mean((residuals(fit3) - mean(residuals(fit3)))^2))





