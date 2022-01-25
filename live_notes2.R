# ---------------------------------------------------------------------------- #
# WORKING DIRECTORY
setwd("~/Dropbox/ETH_courses/2022cts-chile/cts-chile")
list.files() # gaucs o eies se encuentran en mi carpeta local?

# ---------------------------------------------------------------------------- #
rm(list=ls())

#### FREEMAN AND FREEMAN ####
# install.packages("devtools")
# library(devtools)
# devtools::install_github("anespinosa/classicnets")
library(classicnets)
data("eies")
?eies

# load("eies.rda") # OPCION 2

#### ROSSI AND MAGNANI ####
# install.packages("igraph")
library(igraph)
load('gaucs.Rda')
gaucs
gaucs$work
class(gaucs$work)

# ---------------------------------------------------------------------------- #
matrix <- get.adjacency(gaucs$work, sparse = FALSE)
matrix
dim(matrix) # dimensiones de la matriz
head(matrix) # primeras seis filas
matrix[1,] # emisor
matrix[,1] # receptor
table(matrix[1,] == matrix[,1]) # Ejercicio: Por qué ambos son TRUE? 

ncol(matrix) # número de nodos
sum(matrix)/2 # número de aristas

vcount(gaucs$work) == ncol(matrix)
ecount(gaucs$work) == sum(matrix)/2 # Ejercicio: Por qué la matriz se divide por dos?

edgelist <- matrix_to_edgelist(matrix, digraph = FALSE)
head(edgelist)

plot(gaucs$work)

# ---------------------------------------------------------------------------- #
# Una pequeña función para extraer los vecinos de ego sin considerar a ego!
# EN netmem
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

(sum(matrix))/((ncol(matrix)*(ncol(matrix)-1)))

# Grados nodales de cada actor
rowSums(matrix)
table(colSums(matrix) == rowSums(matrix))

# Distribución de grados nodales
plot(table(rowSums(matrix))) # Efecto Mateo? (Merton, 1968)

mean(rowSums(matrix))
sum(rowSums(matrix))/ncol(matrix)



