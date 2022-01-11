# ---------------------------------------------------------------------------- #
#### Directorio de trabajo ####
setwd()
setwd("~/Dropbox/ETH_courses/2022cts-chile")
rm(list=ls())

# ---------------------------------------------------------------------------- #
#### FREEMAN AND FREEMAN ####
# install.packages("devtools")
# library(devtools)
# devtools::install_github("anespinosa/classicnets")
library(classicnets)
data("eies")
?eies

#### ROSSI AND MAGNANI ####
# install.packages("igraph")
library(igraph)
load('~/Dropbox/ETH_courses/2022cts-chile/cts-chile/gaucs.Rda')

# ---------------------------------------------------------------------------- #
#### Empleados del Departamento de Ciencias de la Computación de la Universidad de Aarhus en Dinamarca ####
matrix <- get.adjacency(gaucs$work, sparse = FALSE)
dim(matrix)
head(matrix)
matrix[1,] # emisor
matrix[,1] # receptor
table(matrix[1,] == matrix[,1]) # Ejercicio: Por qué ambos son TRUE? 

ncol(matrix) # número de nodos
sum(matrix)/2 # número de aristas

vcount(gaucs$work) == ncol(matrix)
ecount(gaucs$work) == sum(matrix)/2 # Ejercicio: Por qué la matrix se divide por dos?

edgelist <- matrix_to_edgelist(matrix, digraph = FALSE)

plot(gaucs$work)

# ---------------------------------------------------------------------------- #

## EGO RED
# Una pequeña función para extraer los vecinos de ego sin considerar a ego!
ego_net <- function(A, actor=NULL){
  A <- as.matrix(A)
  actor <- as.character(actor)
  if(is.null(rownames(A))){
    rownames(A) <- paste(1:nrow(A))
  }
  if(is.null(rownames(A))){
    colnames(A) <- paste(1:ncol(A))
  }
  nameOut <- names(which(A[actor,]!=0))
  nameIn <- names(which(A[,actor]!=0))
  name <- unique(c(nameOut, nameIn))
  return(A[name, name])
}

# Elegir un actor de forma aleatoria!
actor <- sample(V(gaucs$work)$name, 1) 
ego_net(matrix, actor = actor)

# Cuántos vecinos tiene el actor U6?
ego_net(matrix, actor = 'U6')

# Incluyendo a ego y suponiendo que estamos interesados/as en el actor 'U4'
egoU6 <- igraph::ego(gaucs$work, 
                     order=1, 
                     # John Barnes (Escuela de Redes de Manchester)
                     # denominaba esto 'zonas' de ego, 
                     # en donde 0 es ego y 1 son sus alteris
                     nodes= 'U4', # nodo de interés
                     mode = 'all' # considerando tanto emisores como receptores 
)
egoU6 <- igraph::induced_subgraph(gaucs$work, unlist(egoU6))
plot(egoU6)

# Ejercicio: Cuál es la diferencia entre 'ego_net' y 'induced_sugraph' de igraph para este ejemplo?

## DENSIDAD
(sum(matrix))/((ncol(matrix)*(ncol(matrix)-1)))

## DEGREES
# Grados nodales de cada actor
rowSums(matrix)

# Distribución de grados nodales
plot(table(rowSums(matrix))) # Efecto Mateo? (Merton, 1968)

# Promedio de grados
mean(rowSums(matrix)); sum(rowSums(matrix))/ncol(matrix)

# EGO NETS
egoU4 <- ego_net(matrix, actor = 'U4')
redundancy <- mean(rowSums(egoU4))
effective_size <- ncol(egoU4) - redundancy
(efficiency <- effective_size/ncol(egoU4)) # Qué tan redundantes es la red de U4?

## BURT
# install.packages("devtools")
# devtools::install_github("anespinosa/netmem")

library(netmem)
addegoU4 <- rbind(egoU4, U4=rep(1, nrow(egoU4)))
addegoU4 <- cbind(addegoU4, U4=rep(1, nrow(addegoU4)))
diag(addegoU4) <- 0
eb_constraint(addegoU4,
              ego="U4")

# ---------------------------------------------------------------------------- #
#### Intercambio de Investigadores de Redes Sociales a través del Electronic Information Exchange System (EIES) en Estados Unidos ####
# Matriz de adjacencia
matrix <- ifelse(eies$time1 > 2, 1, 0)
dim(matrix)
head(matrix)
matrix[1,] # Emisor
matrix[,1] # Receptor

ncol(matrix) # número de nodos
sum(matrix) # número de aristas

# Lista de relaciones
edgelist <- matrix_to_edgelist(matrix, digraph = TRUE)

# Densidad
sum(matrix)/(ncol(matrix)*(ncol(matrix)-1))

library(netmem)
gen_density(matrix, directed = TRUE)

# Grados nodales
rowSums(matrix)
colSums(matrix)

# Distribuciones de grados nodales
plot(table(rowSums(matrix)))
plot(table(colSums(matrix)))

# Reciprocidad
((1/2)*sum(diag(matrix%*%matrix)))/sum(matrix)
# Es la mutualidad alta en esta red?

# Ego redes
set.seed(12354)
rownames(matrix) <- eies$label
colnames(matrix) <- eies$label
plot(graph.adjacency(matrix, mode = c('directed')),
     edge.arrow.size=0.3, vertex.size=5, vertex.label=NA)

# Elegir un actor de forma aleatoria!
actor <- sample(eies$label, 1) 
ego_net(matrix, actor = actor)

# Explorando algunas redes!
plot(graph.adjacency(ego_net(matrix, actor = 'Lin Freeman'),
                     mode = c('directed')),
     edge.arrow.size=0.3,vertex.size=5,
     main = "Ego-red de Linton Freeman")
plot(graph.adjacency(ego_net(matrix, actor = 'Ev Rogers'),
                     mode = c('directed')),
     edge.arrow.size=0.3,vertex.size=5,
     main = "Ego-red de Everett Rogers")
plot(graph.adjacency(ego_net(matrix, actor = 'Nick Mullins'),
                     mode = c('directed')),
     edge.arrow.size=0.3,vertex.size=5,
     main = "Ego-red de Nicholas Mullins")
plot(graph.adjacency(ego_net(matrix, actor = 'Mark Granovetter'),
                     mode = c('directed')),
     edge.arrow.size=0.3,vertex.size=5,
     main = "Ego-red de Mark Granovetter")

# Matriz mixta
mixMatrix <- function(A, att=NULL){
  if(is.null(att))stop("No attribute has been specified")
  data <- as.data.frame(cbind(label=colnames(A), att=att))
  edgelist <- classicnets::matrix_to_edgelist(A)
  
  edgeFROM <- as.data.frame(edgelist[,1L])
  colnames(edgeFROM) <- 'label'
  edgeFROM$id <- 1:nrow(edgeFROM)
  edgeFROM <- merge(edgeFROM, data, by = "label")
  edgeFROM <- edgeFROM[order(edgeFROM$id),] 
  
  edgeTO <- as.data.frame(edgelist[,2L])
  colnames(edgeTO) <- 'label'
  edgeTO$id <- 1:nrow(edgeTO)
  edgeTO <- merge(edgeTO, data, by = "label")
  edgeTO <- edgeTO[order(edgeTO$id),] 
  
  mixMATRIX <- do.call(table, c(list(From=edgeFROM$att, 
                                     To=edgeTO$att)))
  
  if(all(A[lower.tri(A)] == t(A)[lower.tri(A)])){
    warning("The network is undirected")
    mixMATRIX <- mixMATRIX + t(mixMATRIX)
    diag(mixMATRIX) <- diag(mixMATRIX)%/%2L 
  }
  return(mixMATRIX)
}
mixMatrix(matrix, att=eies$discipline)

# Cómo interpretamos esta tabla? 
# Para ver el detalle de la base: ?eies
  
# EI Index
ei.table <- function(matrix, mixed=TRUE, att=NULL)
{
  if(!mixed){
    matrix <- mixMatrix(matrix, att)
  }
  if(length(dim(matrix)) == 3)
    m <- matrix[,,2]
  else m <- matrix
  pI <- m / sum(m)
  I <- sum(diag(pI))
  diag(pI) <- 0
  E <- sum(pI)
  EIindex <- E - I
  return( E - I )
}
ei.table(matrix, mixed=FALSE, att=eies$discipline) # Hay homofilia en la red?

# Los investigadores tienen atributos similares entre sí?
sim <- abs(outer(eies$discipline,eies$discipline,"=="))
diag(sim) <- 0

# Los lazos que envían los autores son ellos mismos homófilas?
homophily <- ifelse(matrix&sim==1, 1, 0)
homophily[1,] # explorando los datos...
rowSums(homophily) # número de lazos emitidos que son homófilos
rowSums(matrix) # número de grados nodales de salida
rowSums(homophily)/rowSums(matrix) # porcentaje de lazos emitidos que son homófilos

# Ejercicio: Qué pasa en el caso de lazos recibidos?

# Indice de Blau
blau <- function(att, normalized = FALSE){
  att <- as.character(att)
  p <- (table(att)/sum(table(att)))^2
  r <- length(p)
  blau <- 1
  for(i in 1:r){
    blau <- blau - p[i]  
  }
  if(normalized){
    iqv <- blau/(1-1/r)
    return(list(blau = blau, iqv = iqv))
  }else(return(blau))
}
blau(eies$discipline, normalized = TRUE)

# blau(c(rep(1, 4), rep(2, 12), rep(3, 9)), normalized = TRUE)

# Medidas de centralidades
g <- graph.adjacency(matrix)
deg <- degree(g)
clos <- closeness(g) # CUIDADO!
betw <- betweenness(g)
ev <- evcent(g)$vector

sort(betw, decreasing = TRUE)
sort(eigen_centrality(g)$vector, decreasing = TRUE)

op <- par(mfrow = c(1, 3))
plot(deg, betw, xlab="Degree", ylab="Betweenness", col="blue") 
plot(deg, ev, xlab="Degree", ylab="Eigenvector", col="blue") 
plot(betw, ev, xlab="Betweenness", ylab="Eigenvector", col="blue")

cor(ev, deg)
cor(ev, betw)

set.seed(1234)
par(mfrow = c(2, 2))
layout <- layout.fruchterman.reingold(g)
plot(g,layout = layout,
     vertex.label=NA,
     edge.arrow.size=0.1,
     main="The Electronic Information Exchange System (EIES)")
plot(g, layout=layout, 
     vertex.label=NA,
     vertex.size=deg*0.75, 
     edge.arrow.size=0.1,
     vertex.label.cex=0.6, main="Degree Centrality")

plot(g, layout=layout,
     vertex.label=NA,
     vertex.size=betw*0.05, 
     edge.arrow.size=0.1,
     vertex.label.cex=0.6, main="Betweenness Centrality")

plot(g, layout=layout,
     vertex.label=NA,
     vertex.size=ev*20, 
     edge.arrow.size=0.1,
     vertex.label.cex=0.6, main="Eigenvector Centrality")


  