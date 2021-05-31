#### 110th US Senate Project #####
# Tony Hung

# Note: some figures and outputs might be different to the ones mentioned in the word doc. This is because some of the figures or models are simulated, thus each simulation would produce different results.

# Basic Set-up
#install.packages("intergraph")
#install.packages("igraph")
#install.packages("gplots")
#install.packages("sna")
#install.packages("netcluster")
#install.packages("ape")
#install.packages("ergm")
#install.packages("dplyr")
#install.packages("network")

require(intergraph)
require(igraph)
require(gplots)

sen <- read.csv("~/Desktop/Work/MY461/110_sen.csv", header=TRUE)
bills <- read.csv("~/Desktop/Work/MY461/110_billspon.csv", header=TRUE,row.names = 1)

#### Descriptive Statistics ####
summary(sen)

#### Full and Simplified Networks ####
bill <- as.matrix(bills)
bill1 <- graph.incidence(bill, weighted=TRUE, directed = FALSE)
bill2 = bipartite.projection(bill1)
bill_adj <- as_adjacency_matrix(bill2$proj2, attr = 'weight')
full <- graph.adjacency(bill_adj, mode = 'undirected', weighted = TRUE)
summary(full)
par(mfrow=c(1,1))
dlayout = layout.fruchterman.reingold(bill2$proj2)
V(full)$label.cex = 0.5
full <- set.vertex.attribute(full,"party",index = V(full), value = (sen$Party))
summary(full)
V(full)$color = ifelse(V(full)$party=="1","blue","red")

full <- set.vertex.attribute(full,"gender",index = V(full), value = (sen$Gender))
full <- set.vertex.attribute(full,"religion",index = V(full), value = (sen$Religion))
full <- set.vertex.attribute(full,"class",index = V(full), value = (sen$Class))
full <- set.vertex.attribute(full,"state",index = V(full), value = (sen$State))
full <- set.vertex.attribute(full,"census",index = V(full), value = (sen$CensusRegion))
full <- set.vertex.attribute(full,"prior",index = V(full), value = (sen$PriorExperience))
full <- set.vertex.attribute(full,"educ",index = V(full), value = (sen$Education))
full <- set.vertex.attribute(full,"first",index = V(full), value = (sen$FirstTookOffice))
full <- set.vertex.attribute(full,"born",index = V(full), value = (sen$Born))

summary(E(full)$weight) # After obtaining the summary of the edge weights, we can see that the cut-off for the 3rd quartile is 103
quarter <- subgraph.edges(full, E(full)[E(full)$weight>=103], del=FALSE)
V(quarter)$label.cex = 0.5

# Figure 1 
par(mfrow=c(1,2))
plot(full,layout=dlayout,edge.width=E(full)/max(E(full)),vertex.size=5,edge.color="grey",main="Full Network")
legend("bottomleft", c("Republican","Democrat"), pch=21, col=c("red","blue"), pt.cex=2, cex=.8, bty="n", ncol=1)
plot(quarter,vertex.size=5,edge.color="grey",main="Simplified Network")
par(mfrow=c(1,1))

ecount(full)
vcount(full)

# Table 1: basic graph statistics
table1 <- data.frame(c('Full','Simplified'), 
                     c(ecount(full),ecount(quarter)),
                     c(vcount(full),vcount(quarter)),
                     c(graph.density(full), graph.density(quarter)),
                     c(average.path.length(full), average.path.length(quarter)), 
                     c(transitivity(full), transitivity(quarter)),
                     c(diameter(full, directed = TRUE),diameter(quarter, directed = TRUE))
                     )
colnames(table1) <- c('Number of Edges', 'Number of Nodes','Network', 'Density', 'Average Path Length', 'Transitivity','Diameter')
table1

#### Question 1: Who was the most influential senator? ####

# Table 2: centrality measures
max(degree(quarter)) # Degree Centrality.Norm Coleman 69
max(evcent(quarter)$vector) # Eigenvector Centrality. Hillary Clinton 1
max(betweenness(quarter, weights = 1/(E(full)$weights))) # Norm Coleman 771.2286177
max(closeness(quarter, weights = 1/(E(full)$weights))) # Norm Coleman 7.501875e-04

#### Question 2: Does party membership influence bill co-sponsorship? ####

# Block Model (simplified) and then Louvain Model (full)  

# Block Model for the simplified network
require(intergraph)
detach(package:igraph)
require(sna)

qnet<-asNetwork(quarter)
qbm = blockmodel(qnet,ec=qnet %v% "party",rlabels = c("Democratic", "Republican"))$block.model

summary(qbm)

# fnet<-asNetwork(full)
# blockmodel(fnet,ec=qnet %v% "party",rlabels = c("Democratic", "Republican"))


#Going back to igraph
detach(package:sna)
detach(package:intergraph)
require(igraph)

qkar <- sample_sbm(90, pref.matrix=qbm, block.sizes=c(42,48), directed=FALSE)

# Figure 2: blockmodel vs simplified
par(mfrow=c(1,2))
plot(qkar,vertex.color=c(rep(2,42),rep(1,48)), main='Block Model')
legend("bottomleft", c("Republican","Democrat"), pch=21, col=c("orange","cyan"), pt.cex=2, cex=.8, bty="n", ncol=1)
plot(quarter, main='Simplified',layout=dlayout)
legend("bottomleft", c("Republican","Democrat"), pch=21, col=c("red","blue"), pt.cex=2, cex=.8, bty="n", ncol=1)

dev.off()

assortativity(quarter,factor(V(quarter)$party))
# 0.3345591 
assortativity(qkar,c(rep(1,42),rep(2,48)))
# 0.3390892

# Figure 3: Louvain Community Detection: Full Network
ml <- cluster_louvain(full, weights = E(full)$weight)
plot(ml, full, node.size=0.01, main='Louvain Community Detection',edge.width = 1/E(full)$weight, vertex.label.cex = .5)
legend("bottomleft", c("Republican","Democrat"), pch=21, col=c("orange","cyan"), pt.cex=2, cex=.8, bty="n", ncol=1)


## Note: This section below is not included in the word document, as it is more of my own 'amusement'
# If we use Louvain on the simplified network, it will detect one big chunk, and each isloates individually, thus defeating the purpose... Unless we remove the isloates to do Louvain?  

qq <- subgraph.edges(full, E(full)[E(full)$weight>=103], del=TRUE)
V(qq)$label.cex = 0.5
qml <- cluster_louvain(qq, weights = E(qq)$weight)
plot(qml, qq, node.size=0.01, main='Louvain Community Detection',edge.width = 1/E(qq)$weight, vertex.label.cex = .5,layout=dlayout)
# Very interesting. There are technically three groups, but the overlaps are way too similar... Baucus is an isolate, meaning that he did not get included in any group. 
## Note: This section above is not included in the word document, as it is more of my own 'amusement'

# Table 4
table(V(full)$party,membership(ml)) 

# Comparing the Louvain against other attributes

# full network based on other attributes
fullc <- graph.adjacency(bill_adj, mode = 'undirected', weighted = TRUE)
fullc <- set.vertex.attribute(fullc,"class",index = V(fullc), value = (sen$Class))
fullcr <- graph.adjacency(bill_adj, mode = 'undirected', weighted = TRUE)
fullcr <- set.vertex.attribute(fullcr,"cr",index = V(fullcr), value = (sen$CensusRegion))
fullg <- graph.adjacency(bill_adj, mode = 'undirected', weighted = TRUE)
fullg <- set.vertex.attribute(fullg,"g",index = V(fullg), value = (sen$Gender))
fullr <- graph.adjacency(bill_adj, mode = 'undirected', weighted = TRUE)
fullr <- set.vertex.attribute(fullr,"r",index = V(fullr), value = (sen$Religion))
fullf <- graph.adjacency(bill_adj, mode = 'undirected', weighted = TRUE)
fullf <- set.vertex.attribute(fullf,"f",index = V(fullf), value = (sen$FirstTookOffice))
fullb <- graph.adjacency(bill_adj, mode = 'undirected', weighted = TRUE)
fullb <- set.vertex.attribute(fullb,"b",index = V(fullb), value = (sen$Born))

# Table 5
table3 <- data.frame(c('Party','Class','Census Region','Gender','Religion','First Took Office','Born'), 
                     c(compare(V(full)$party,ml,method="nmi"),
                       compare(V(fullc)$class,ml,method="nmi"),
                       compare(V(fullcr)$cr,ml,method="nmi"),
                       compare(V(fullg)$g,ml,method="nmi"),
                       compare(V(fullr)$r,ml,method="nmi"),
                       compare(V(fullf)$f,ml,method="nmi"),
                       compare(V(fullb)$b,ml,method="nmi")))
colnames(table3) <- c('Attributes','Comparison Scores against Louvain')
table3

#### Question 3: What are the social structures of the senate? ####
# Week 4 seminar

require(NetCluster)
require(gplots)
require(ape)
require(sna)
require(network)

d = as.matrix(bill_adj)

# Figure 4: Dendrogram
cor(d) # Another way to look at 
as.dist(1-cor(d),upper=TRUE)
comp<-hclust(as.dist(1-cor(d),upper=TRUE),method="complete")
plot(comp, cex=0.5, main='Dendogram of the full network',hang=-1)

# Figure 5: Structural Equivalence
#cutting it into four
dev.off()
par(mfrow=c(1,2))
plot(bill2$proj2,edge.width=1/E(bill2$proj2)$weight,vertex.color=cutree(comp, k=4), vertex.label.cex = .5, main='Structural Equivalence',layout=dlayout,vertex.size=5,edge.color="grey")
plot(full,layout=dlayout,edge.width=E(full)/max(E(full)),vertex.size=5,edge.color="grey",main="Full Network")
legend("bottomleft", c("Republican","Democrat"), pch=21, col=c("red","blue"), pt.cex=2, cex=.8, bty="n", ncol=1)

dev.off()

table_class_equivalency<-data.frame(ClassID=cutree(comp, k=4))
row.names(table_class_equivalency)<-sen$Name

eq_class = table_class_equivalency$ClassID
sen=cbind(sen,eq_class)

require(dplyr)
# Table 6
sen %>% group_by(eq_class,Party) %>% tally()
# sen %>% group_by(eq_class,Gender) %>% tally()

# Table 7
q =  sen %>% group_by(eq_class,CensusRegion,Party) %>% tally()
t(q)

#### Question 4: Which characteristics of the senators predict co-sponsorship? ####

# Week 10 ERGM

# Again, the full network cannot be simulated due to the completeness of the graph. If everyone is connected to everyone, then we cannot predict anything anymore. 

# https://rdrr.io/cran/ergm/man/ergm-terms.html
# https://stats.stackexchange.com/questions/149502/regression-model-and-social-network-analysis
# https://www.r-bloggers.com/ergm-tutorial/
# http://badhessian.org/2012/09/lessons-on-exponential-random-graph-modeling-from-greys-anatomy-hook-ups/

require(ergm)

table(qnet %v% "religion")

m1 <- ergm(qnet~edges)
summary(m1)
exp(m1$coef)

# This takes a really long time
m2 <- ergm(qnet ~ edges + nodefactor("party") + nodematch("party") + nodefactor("gender") + nodematch("gender") + nodematch("census") + absdiff("first") + gwesp(0.6, fixed = TRUE))
summary(m2)

#options(scipen=999)
exp(m2$coef)

# calculating the fitted probability
# two women from the same region, who joined in the same year, and who have both cosponsored a bill with one other senator; once, when they are both Democrats
# edges + nodematch.census + absdiff.joined + gwesp.fixed.0.6 + nodematch.party
r = -6.915860 + 0.661768 + 3.810799 + 0.673041 + -0.006935 + 1.861290
exp(r)/(1+exp(r))

# same conditions, but Republicans 
r = -6.915860 + 0.661768 + 3.810799 + 0.673041 + -0.006935 + 1.861290 + 2*-1.199017
exp(r)/(1+exp(r))
