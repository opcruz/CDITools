##################################################
# Global variables
##################################################

if(Sys.info()[["sysname"]] == "Windows"){
  # set global variable path Windows
  Sys.setenv(R_ZIPCMD = paste(getwd(), "bin", "zip.exe" , sep="/"))
  
}else{
  # set global variable path Linux
  Sys.setenv(R_ZIPCMD="/usr/bin/zip")
}


# Shiny Dashboadpage lock dashboardHeader on top
pin_header <- "
/* logo */
.skin-blue .main-header .logo {
  position: fixed; 
  overflow: visible;
}

/* main sidebar */
.skin-blue .main-sidebar {
  position: fixed; 
  overflow: visible;
}
"
#Custom nav-tabs
custom_nav <- "
.nav-tabs {background: #f4f4f4;}
.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs
custom .nav-tabs li.active a {background-color: #fff;
border-color: #fff;
}
.nav-tabs-custom .nav-tabs li.active {border-top-color:
#3c8dbc;
}
"

#Graph
require(igraph)
require(multinet)
require(MUNA)
require(threejs)
require(XML)

# initial graph
# data(ego)

# xsd for validate config.xml file
xsd = xmlTreeParse("schema/schema.xsd", isSchema =TRUE, useInternal = TRUE)
# Parse config.xml file inside .zip
parseConfig <- function(pathZip) {
  
  conn <- unz(pathZip, "config.xml")
  lines <- readLines(conn)
  close(conn)
  
  doc = xmlInternalTreeParse(lines , asText = TRUE)
  t <- xmlSchemaValidate(xsd, doc)
  
  if(t$status != 0) {
    
    stop(t$errors[[1]]$msg)
    
  }
  
  net <- list()
  rootnode <- xmlRoot(doc)
  # topology
  topology <- rootnode[["topology"]]
  
  tp <- list(
    type = xmlValue(topology[["type"]]),
    directed = as.logical(xmlValue(topology[["directed"]])),
    weighted = as.integer(xmlValue(topology[["weighted"]])),
    labelNode = as.integer(xmlValue(topology[["labelNode"]]))
  )
  net <- append(net , list(graphInfo = tp))
  #nodes
  nodes <- rootnode[["nodes"]]
  if(!is.null(nodes)){
    
    nd <- list(
      file = xmlValue(nodes[["file"]]),
      separator = xmlValue(nodes[["separator"]]),
      head = as.logical(xmlValue(nodes[["head"]]))
    )
    net <- append(net , list(nodesInfo = nd))
  }
  #layers
  layers <- list()
  for(layer in xmlChildren(rootnode[["layers"]])){
    
    ly <- list(
      file = xmlValue(layer[["file"]]),
      separator = xmlValue(layer[["separator"]]),
      head = as.logical(xmlValue(layer[["head"]])),
      id = xmlAttrs(layer)[["id"]]
    )
    
    layers <- append(layers , list(ly))
  }
  net <- append(net , list(layersInfo = layers))
  
  #Load Nodes
  if (!is.null(net[["nodesInfo"]])) {
    conn <- unz(pathZip, net$nodesInfo$file)
    nodesData <-
      read.csv(
        conn,
        header = net$nodesInfo$head,
        sep = net$nodesInfo$separator,
        comment.char = "#"
      )
    
    net[["nodesData"]] <- nodesData
  }
  
  #Load Layers
  l <- list()
  for (lr in net$layersInfo) {
    
    conn <- unz(pathZip, lr$file)
    layersData <-
      read.csv(
        conn,
        header = lr$head,
        sep = lr$separator,
        comment.char = "#"
      )
    
    l[[lr$id]] <- layersData
    
  }
  
  net[["layersData"]] <- l
  
  class(net) <- "netInfo"
  return(net)
}

# Simplify graph with community detection for better visualization
simplifyCommunity <-  function(graph ,
                               community_object ,
                               directed = is_directed(graph)) {
  
  community <- membership(community_object)
  
  #Number of communities
  n <- max(community)
  matriz <- matrix(data = 0, nrow = n , ncol = n)
  edgesL <- as_edgelist(graph , names = FALSE)
  edgeCom <- matrix(community[edgesL] , ncol = 2) 
  
  for(i in 1:nrow(edgeCom)){
    
    matriz[edgeCom[i,1], edgeCom[i,2]] <- matriz[edgeCom[i,1], edgeCom[i,2]] + 1
    
  }
  
  # Crear grafo ponderado
  g <- graph_from_adjacency_matrix(matriz ,
                                   mode = if(directed)"directed" else "plus",
                                   weighted = TRUE,
                                   diag = FALSE)
  
  # Atributo cantidad de vertices en cada comunidad
  V(g)$nVertex <- table(as.factor(community))
  
  # Atributo cantidad de aristas entre vertices de una misma comunidad
  V(g)$nEdges <- diag(matriz, names = FALSE)
  
  V(g)$group <- 1:gorder(g)
  return (g)
}

# load object for multiplex network
importMultiLayer <- function(net , simplifyGraph = FALSE) {
  
  mul <- ml_empty() # multinet
  l <- list()
  M <- new("Multiplex") # Muna
  
  # Build igraph objects and add to multinet network
  for (i in 1:length(net$layersData)) {
    
    g <- graph_from_data_frame(d = net$layersData[[i]],
                               vertices = net[["nodesData"]],
                               directed = net$graphInfo$directed)
    
    graph_attr(g, "name") <- names(net$layersData)[i]
    
    if(net$graphInfo$weighted != -1){
      
      nameAttr <- colnames(net$layersData[[i]])[net$graphInfo$weighted]
      edge_attr(g, "weight") <- edge_attr(g , name = nameAttr)
      
    }
    
    if(net$graphInfo$labelNode != -1){
      
      labelAttr <- colnames(net$nodesData)[net$graphInfo$labelNode]
      vertex_attr(g, "label_ID") <- vertex_attr(g , name = labelAttr)
      
    }
    
    if (simplifyGraph) {
      g <- simplify(g)
    }
    
    vertex_attr(g, name = "name", index = V(g)) <- as_ids(V(g))
    
    lay <- list(g)
    names(lay) <- c(names(net$layersData)[i])
    l <- append(l , lay)
    add_igraph_layer_ml(mul, g, names(net$layersData)[i]) # multinet
    M <- add_layer(M , g) # Muna
    
  }
  obj <-list(mulGraph = mul ,
             munaGraph = M ,
             igraphLayers = l
            )
  
  class(obj) <- "MultData"
  return(obj)
}


# save monoplex network (zip file)
save_monoplex_graph <- function(graph , file) {
  
  tmpdir <- tempdir()
  path <- getwd()
  setwd(tempdir())
  
  zfiles <- c("config.xml","edges.txt","nodes.txt")
  
  nodes <- cbind(id = as_ids(V(graph)) , as_data_frame(graph, what="vertices"))
  
  edges <- as.data.frame(as_edgelist(graph, names = TRUE)) 
  names(edges) <- c("from" , "to")
  if(is_weighted(graph)){
    edges <- cbind(edges , weight = E(graph)$weight)
  }
  
  write.table(nodes, file = "nodes.txt" , row.names = FALSE, col.names = TRUE, quote = FALSE, sep = " ")
  write.table(edges, file = "edges.txt" , row.names = FALSE, col.names = TRUE, quote = FALSE, sep = " ")
  
  root = newXMLNode("network")
  
  tp <- newXMLNode("topology" , parent = root)
  newXMLNode(name = "type" , "monoplex", parent = tp )
  newXMLNode(name = "directed" ,
             if(is_directed(graph)) "true" else "false",
             parent = tp)
  newXMLNode(name = "weighted" ,
             if(is_weighted(graph)) "3" else "-1",
             parent = tp)
  newXMLNode(name = "labelNode" ,
             "-1",
             parent = tp)
  
  nodes <- newXMLNode("nodes" , parent = root)
  newXMLNode(name = "file" ,
             "nodes.txt",
             parent = nodes)
  newXMLNode(name = "separator" ,
             " ",
             parent = nodes)
  newXMLNode(name = "head" ,
             "true",
             parent = nodes)
  
  layers <- newXMLNode("layers" , parent = root)
  layer <- newXMLNode("layer" , parent = layers, attrs = c(id = "L1"))
  newXMLNode(name = "file" ,
             "edges.txt",
             parent = layer)
  newXMLNode(name = "separator",
             " ",
             parent = layer)
  newXMLNode(name = "head" ,
             "true",
             parent = layer)
  
  rootDoc <- xmlDoc(root)
  
  saveXML(rootDoc , file="config.xml" , indent = TRUE)
  
  zip(zipfile = file, files = zfiles)
  
  setwd(path)
}


# generic normalize size
normalize_size <- function(vector , min , max) {
  
  minValue <- min(vector)
  maxValue <- max(vector)
  m <- (max - min)/(maxValue - minValue)
  n <- max - (m * maxValue)
  
  return ((m * vector) + n)
  
}


# for visNetwork Graph
max_size_node <- 30
min_size_node <- 6

max_size_edge <- 8
min_size_edge <- 1


################## MUNA MOD ################################

#'  Partition aggregation based community detection algorithm
#'
#' @description This method applies a partition aggregation based community detection algorithm. The idea is to apply a standard community detection algorithm designed for monoplex network (community_algo) to each layer of the multiples. Then an ensemble clustering approach is applied on the obtained clusterings in order to compute the final community structure. The CSPA ensemble clustering approach is used for that purpose. All basic community detection approaches provided in igraph can be used here.
#' @usage community.partition_aggregation(multiplex, community_algorithm, h, alpha)
#' @param community_algorithm : String, the name of classical community detection algorithm. As Licod, Louvain, Infomap, Walktrap,...
#' @param alpha :
#' @param h :
#' @param multiplex : The multiplex object.
#' @return returns an igraph  communities object, please see igraph manual page for details.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' community.partition_aggregation(M)
#' @export

ensemble_partition_CSPA_v2 <-
  function(multiplex,
           community_algorithm = cluster_louvain,
           h = 1,
           alpha = 1 / length(multiplex@layers)) {
    
    if (!is.multiplex(multiplex))
      stop("Is not a multiplex network")
    Threshold <- h * alpha
    nbNodes = length(multiplex@nodes)
    mat <- matrix(c(0), ncol = nbNodes, nrow = nbNodes)
    colnames(mat) <- multiplex@nodes
    rownames(mat) <- multiplex@nodes
    for (i in 1:h) {
      for (graph in multiplex@layers) {
        wt <- community_algorithm(graph)
        comList <- groups(wt)
        for (com in comList) {
          if (length(com) > 1) {
            for (x in seq((length(com) - 1))) {
              for (y in (x + 1):length(com)) {
                mat[com[x], com[y]] <- (mat[com[x], com[y]] + 1)
                mat[com[y], com[x]] <- mat[com[x], com[y]]
              }
            }
          }
        }
      }
    }
    
    # Divide by number of layers
    mat <- mat / length(multiplex@layers)
    
    #Filter results
    mat [mat < Threshold] <- 0
    
    g <- graph.adjacency(mat, mode = "undirected", weighted = TRUE)
    
    t <- system.time(com <- community_algorithm(g))
    com$time <- t[["elapsed"]]
    
    return(list("graph" = g , "community" = com))
  }

#' Multiplex modularity of a given community structure of a multiplex network
#'
#' @description Multiplex modularity of a given community structure of a multiplex network
#' @usage modularity_multiplex(multiplex, partition)
#' @param partition : an igraph  communities object, vertex membership.
#' @param multiplex : The multiplex object.
#' @return A numeric scalar, the multiplex modularity score of the given configuration
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' wt <- community.partition_aggregation(M)
#' modularity_multiplex(M, wt)
#' @export

modularity_multiplex_v2 <- function(multiplex, membership) {
  if (!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  Q <- 0
  u <- sum(ecount.multiplex(multiplex))
  if (!is.null(u)) {
    for (layer in multiplex@layers) {
      Q <- Q + (modularity(layer, membership) * (2 * ecount(layer)))
    }
    Q =  (Q / (2 * u))
  }
  return(round(Q, digits = 4))
}

