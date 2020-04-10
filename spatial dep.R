library(igraph); library(spdep)
load("data.Rdata"); load("locs.Rdata")
rawData <- read.table("county_adjacency.munged.txt", colClasses=c("character", "character"))
G <- graph.edgelist(as.matrix(rawData), directed=FALSE)
G <- simplify(G, remove.loops=TRUE);V(G)$name=as.numeric(V(G)$name) 
subFIPSid=V(G)$name %in% locs$Id2
countyGraph=induced.subgraph(G, V(G)[subFIPSid]);
graph0=permute(countyGraph, order(V(countyGraph)$name)); ### this is the dense graph to begin with.
nb.mat=as_adjacency_matrix(graph0)
# nb.mat = t(apply(nb.mat, 1, function(x){x/sum(x)}))
nb.w = mat2listw(nb.mat)

fit = lm(data$y~., data = data)
summary(fit)
plot(fit)

spdep_test = lm.morantest(fit, nb.w, alternative="two.sided")
print(spdep_test)

fit.lagrange = lm.LMtests(fit, nb.w, test=c("LMerr", "RLMerr", "LMlag", "RLMlag", "SARMA"))
print(fit.lagrange)
