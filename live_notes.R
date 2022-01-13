# ---------------------------------------------------------------------------- #
rm(list=ls())
setwd("~/Dropbox/ETH_courses/2022cts-chile/cts-chile")
list.files()

install.packages("devtools")
devtools::install_github("anespinosa/classicnets")

library(classicnets)
data("eies")
?eies
load('eies.rda')

# ---------------------------------------------------------------------------- #
# install.packages("igraph")
library(igraph)
load('gaucs.Rda')
gaucs$work
class(gaucs$work)

# ---------------------------------------------------------------------------- #
# Empleados del Departamento de Ciencias de la Computación de la Universidad de Aarhus en Dinamarca
matrix <- get.adjacency(gaucs$work, sparse = FALSE)
# edge list 
# attribute list

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
# EGO REDES
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

# DENSIDAD (ELIZABETH BOTT "Knitting")
(sum(matrix))/((ncol(matrix)*(ncol(matrix)-1)))

# Grados nodales de cada actor
rowSums(matrix)

# Distribución de grados nodales
plot(table(rowSums(matrix))) # Efecto Mateo? (Merton, 1968)
# prefential attachment / 1938 (Moreno y Jennings) Dynamic Social Effects

mean(rowSums(matrix)); sum(rowSums(matrix))/ncol(matrix)
mean(colSums(matrix))


egoU4 <- ego_net(matrix, actor = 'U4')
redundancy <- mean(rowSums(egoU4))
effective_size <- ncol(egoU4) - redundancy
(efficiency <- effective_size/ncol(egoU4)) # Qué tan redundantes es la red de U4?

library(netmem)
addegoU4 <- rbind(egoU4, U4=rep(1, nrow(egoU4)))
addegoU4 <- cbind(addegoU4, U4=rep(1, nrow(addegoU4)))
diag(addegoU4) <- 0
eb_constraint(addegoU4,
              ego="U4")
?constraint()

# ---------------------------------------------------------------------------- #

matrix <- ifelse(eies$time1 > 2, 1, 0)
dim(matrix)
head(matrix)
matrix[1,] # Emisor
matrix[,1] # Receptor

table(matrix[1,] == matrix[,1])

ncol(matrix) # número de nodos
sum(matrix) # número de aristas

# Lista de relaciones
edgelist <- matrix_to_edgelist(matrix, digraph = TRUE)

rowSums(matrix) # rangos de salida (outdegree)
colSums(matrix) # rangos de entrada (indegree)

# Distribuciones de grados nodales
plot(table(rowSums(matrix)))
plot(table(colSums(matrix))) # Efecto Mateo


# Reciprocidad
((1/2)*sum(diag(matrix%*%matrix)))/sum(matrix)


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

# netmem
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


g <- graph.adjacency(matrix)
deg <- degree(g)
clos <- closeness(g) # CUIDADO!


