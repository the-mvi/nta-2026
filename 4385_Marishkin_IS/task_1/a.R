library(network)
data_edges <- read.csv("nta-2026\\4385_Marishkin_IS\\task_1\\florentine_business.csv", stringsAsFactors = FALSE)
rownames(data_edges) <- colnames(data_edges)
matrix_edges <- as.matrix(data_edges)
s_matrix <- as.sociomatrix(matrix_edges)
s_matrix

edge_list <- as.matrix(s_matrix, matrix.type = "edgelist")
head(edge_list, 5)

basic_network <- network(s_matrix, directed = FALSE)

library(sna)
par(mfrow = c(1, 2))
gplot(basic_network, 
    mode = "fruchtermanreingold", 
    main = "Фрюхтермана—Рейнгольда",
    vertex.col = "blue", 
    vertex.cex = 1.5)
gplot(basic_network, 
    mode = "kamadakawai", 
    main = " Камада—Каваи",
    vertex.col = "red", 
    vertex.cex = 1.5)
par(mfrow = c(1, 1))

basic_network

net_size <- network.size(basic_network)
network.density(basic_network)
net_my_density <- (network.edgecount(basic_network)*2)/net_size/(net_size-1)
## По графику 5 изолированных и 1 большая группа - итого 6 компонент связности
components(basic_network)
length(basic_network)
gtrans(basic_network)

degrees_norm <- degree(basic_network, gmode = "graph", rescale = TRUE)
not_isolated <- which(degrees_norm > 0)
not_isolated_graph <- get.inducedSubgraph(basic_network, not_isolated)
closeness(not_isolated_graph, gmode = "graph", rescale = TRUE)
betweenness(basic_network, gmode = "graph", rescale = TRUE)
evcent(basic_network, gmode = "graph", rescale = TRUE)


library(network)
data_edges <- read.csv("nta-2026\\4385_Marishkin_IS\\task_1\\krackhardt_advice.csv", stringsAsFactors = FALSE)
rownames(data_edges) <- colnames(data_edges)
matrix_edges <- as.matrix(data_edges)
s_matrix <- as.sociomatrix(matrix_edges)
s_matrix

edge_list <- as.matrix(s_matrix, matrix.type = "edgelist")
head(edge_list, 5)

basic_network <- network(s_matrix)

library(sna)
par(mfrow = c(1, 2))
plot(basic_network, 
    mode = "fruchtermanreingold", 
    main = "Фрюхтермана—Рейнгольда",
    vertex.col = "blue", 
    vertex.cex = 1.5)
plot(basic_network, 
    mode = "kamadakawai", 
    main = " Камада—Каваи",
    vertex.col = "red", 
    vertex.cex = 1.5)

basic_network

net_size <- network.size(basic_network)
network.density(basic_network)
net_my_density <- (network.edgecount(basic_network))/net_size/(net_size-1)
net_my_density
## По графику нереально разобрать...
components(basic_network)
length(basic_network)
gtrans(basic_network)

degrees_norm <- degree(basic_network, gmode = "graph", rescale = TRUE)
not_isolated <- which(degrees_norm > 0)
not_isolated_graph <- get.inducedSubgraph(basic_network, not_isolated)
closeness(not_isolated_graph, gmode = "graph", rescale = TRUE)
betweenness(basic_network, gmode = "graph", rescale = TRUE)
evcent(basic_network, gmode = "graph", rescale = TRUE)

dir_net <- network(s_matrix)
p_deg <- prestige(dir_net, rescale = TRUE)
p_prox <- prestige(dir_net, cmode = "domain.proximity", rescale = TRUE)
p_rank <- prestige(dir_net, cmode = "eigenvector", rescale = TRUE)
