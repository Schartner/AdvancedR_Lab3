dijkstra <- function(graph, init_node){
    #assertion
    stopifnot(length(init_node) <= 1)
    stopifnot(is.numeric(init_node))
    for(i in 1:2){
        stopifnot(any(wiki_graph[i] == init_node))
    }
    distance<-NULL
    #maybe not order
    names(distance)<-order(unique(graph[1]))
    distance[init_node]<-0
    #v1[init_node]<-NA
  #missing
    
    for (i in v1){
      if (i!=init_node){
        distance[i]<- Inf
      }
    }
    #implementation
    #problem not index but number
    visited<-c(init_node)
  #  for (i in 1:length(unique(graph[[3]]))){

  distance[graph[2][graph[1]==init_node]]<-graph[[3]][graph[1]==init_node]
 visited[i+1]<-which.min(distance)
 
} 

}


test_frame<-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                       v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                       w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

##test
