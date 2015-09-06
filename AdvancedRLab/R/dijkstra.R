#'Dijkstra Algorithm
#'
#'A method for finding the shortest path between the nodes in a graph.
#'
#'@param graph a dataframe consists of three variables 
#'@param init_node a numeric scalar for the first node which also exist in the graph
#'
#'@return The value is a vector. It is the least value (or shortest path) from the starting node to every nodes in a graph.
#'
#'@references \url{https://en.wikipedia.org/wiki/Dijkstra}
#'
#'@examples
#'dijkstra(wiki_graph, 1) #0 7 9 20 20 11
#'dijkstra(wiki_graph, 3) #9 10 0 11 11 2
#'

dijkstra <- function(graph, init_node){
    #assertion for graph
    stopifnot(is.data.frame(graph))
    stopifnot(length(graph) == 3)
stopifnot(all(sort(unique(graph[[1]]))==sort(unique(graph[[2]]))))
    
    #assertion for init_node
    stopifnot(length(init_node) <= 1)
    stopifnot(is.numeric(init_node))
    for(i in 1:2){
        stopifnot(any(graph[i] == init_node))
    }
    
    #define variables
    distance <- numeric(0)
    visit <- numeric(nrow(unique(graph[1])))
    
    ##all.nodes should be declare before this command line 'distance[all.nodes]>-Inf'
    all.nodes <- sort(unique(graph[[1]]))
    distance[all.nodes] <- Inf    
    names(distance) <- all.nodes
    distance[init_node] <- 0
 
    #the vector visit that keeps track of all the nodes that have been visited
    visit[init_node] <- init_node

    #calculate distances from initial point. 
    ##might be possible to implement in while loop. will try later
    distance[graph[2][graph[1] == init_node]] <- graph[[3]][graph[1] == init_node]
    
    #function that determines the next node taking in account the distances already gone and points already visited
    next.node <- function(distance, visit){
        return(as.numeric(names(which.min(distance[names(distance)!=visit]))))
    }
  
    #if not all points have been visited, the while loop will loop for the distances 
    while(!all(all.nodes %in% visit)){
        #determine the next node
        i <- next.node(distance, visit)
        visit[next.node(distance, visit)] <- i
        
        #temporary has the distances from current node to all neighboring
        temporary <- c(rep(Inf,length(all.nodes)))
        temporary[graph[2][graph[1] == i]] <- graph[3][graph[1] == i]
        
        temp <- temporary + distance[i]
        distance[temp < distance] <- temp[temp < distance]
    }
    names(distance) <- NULL
    return(distance)
}