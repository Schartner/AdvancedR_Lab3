dijkstra <- function(graph, init_node){
    #assertion
    stopifnot(length(init_node) <= 1)
    stopifnot(is.numeric(init_node))
    for(i in 1:2){
        stopifnot(any(wiki_graph[i] == init_node))
    }
    
    #implementation
    
}