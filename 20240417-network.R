library(igraph)
source("E:/Rwd/res/center_igraph_edges.R")
source("E:/Rwd/res/num-to-color.R")
#genesbycategory<-list(Review, Clinical_standard, Clinical_supp, Basic_research, Population_studies, Clinical_algo_ml)
sums<-sapply(3:139, function(x){sum(ci[x]=="YES")})
names(sums)<-colnames(ci)[3:139]
sums20<-sums[sums>25]
ll<-length(sums20)
sums20<-sums20[order(names(sums20))]
ci20<-ci[colnames(ci) %in% names(sums20)]
ci20<-ci20[order(names(ci20))]
ci20<-ci20[order(sums20, decreasing = T)]
sums20<-sums20[order(sums20, decreasing = T)]

adj<-sapply(1:ll, function(x){sapply(1:ll, function(y){sum(ci20[x]=="YES" & ci20[y]=="YES")})})
colnames(adj)<-colnames(ci20)
adj[adj<25]<-0
network<-graph_from_adjacency_matrix(adj, mode="undirected", diag=F, weighted=T, add.colnames = T)


p<-plot(network, rescale=F, layout=layout.circle, 
     vertex.label=paste(names(sums20), sums20, sep="\n"),
     vertex.size=logb(sums20, b=2), 
     edge.label=E(network)$weight, 
     #edge.label.x=center_igraph_edges(network, "x"),
     #edge.label.y=center_igraph_edges(network, "y"),
     vertex.label.cex=0.5,
     edge.label.cex=0.35,
     ylim=c(-1,1), xlim=c(-1,1), asp=0
     )

mylayout<-layout.circle(network)

png("Rplot002.png", width=5, height=5, units="in", res=600)
p<-plot(network, rescale=F, layout=mylayout, 
        vertex.label=paste(names(sums20), sums20, sep="\n"),
        vertex.size=logb(sums20, b=2), 
        vertex.frame.color=NA,
        edge.label=E(network)$weight, 
        #edge.width=E(network)$weight,
        edge.color=num_to_rb(E(network)$weight, max(E(network)$weight)),
        edge.label.color=num_to_rb(E(network)$weight, max(E(network)$weight)),
        edge.label.x=center_igraph_edges(network, "x"),
        edge.label.y=center_igraph_edges(network, "y"),
        vertex.label.cex=0.5,
        edge.label.cex=0.35,
        ylim=c(-1,1), xlim=c(-1,1), asp=0
)
dev.off()
#ggsave("network_gbm.jpeg", p, device="jpeg", width =8, height=8, units = "in")




