library(igraph)
genesbycategory<-list(Review, Clinical_standard, Clinical_supp, Basic_research, Population_studies, Clinical_algo_ml)
adj<-sapply(1:6, function(x){sapply(1:6, function(y){sum(genesbycategory[[x]] %in% genesbycategory[[y]])})})
colnames(adj)<-names(genesonly)[13:18]
network<-graph_from_adjacency_matrix(adj, mode="undirected", diag=F, weighted=T, add.colnames = T)
p<-plot(network, layout=layout.circle, 
     vertex.label=paste(names(confreqs[-7]), confreqs[-7]),
     vertex.size=confreqs[-7], 
     edge.label=E(network)$weight
     )
ggsave("network_gbm.jpeg", p, device="jpeg", width =8, height=8, units = "in")


