dijkstra <- function(graph, init_node){
    #assertion
    stopifnot(length(init_node) <= 1)
    stopifnot(is.numeric(init_node))
    for(i in 1:2){
        stopifnot(any(graph[i] == init_node))
    }
    
    

    distance[all.nodes]<-Inf
    distance[init_node]<-0
 
    all.nodes<-sort(unique(graph[[1]]))
    names(distance)<-all.nodes
    
   #the vector visit keeps track of all the nodes visited

visit[1]<-init_node

#calculate distances from initial point. might be possible to implement in while loop. will try later
  distance[graph[2][graph[1]==init_node]]<-graph[[3]][graph[1]==init_node]
  
  #function that determins the next node taking in account the distances already gone and points already visited
  #sets NA to 0 but only within the function because it cannot deal with NA
  next.node<-function(distance,visit)
  {if(any(is.na(visit))){visit[is.na(visit)]<-0}
    as.numeric(names(which.min(distance[names(distance)!=visit])))
    }
  
#determining the second node. maybe better implimented within while loop
 i<-next.node(distance,visit)
 visit[next.node(distance,visit)]<-i
 #if not all points have been visited the while loop will loop for the distances 
 while (!all(all.nodes %in% visit)){
 #temporary has the distances from current node to all neighboring
   temporary<-c(rep(Inf,length(all.nodes)))
   temporary[graph[2][graph[1]==i]]<-graph[3][graph[1]==i]
   
   distance[(temporary+distance[i])<distance]<-(temporary+distance[i])[(temporary+distance[i])<distance]
   
   #determine next one
   i<-next.node(distance,visit)
   visit[next.node(distance,visit)]<-i
 }
 distance
} 



test_frame<-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                       v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                       w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(test_frame,1)
