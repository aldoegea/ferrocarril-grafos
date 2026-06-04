# Recorrido de un ferrocarril con grafos en R

Mini proyecto centrado en el uso de grafos para el trazado de recorridos óptimos. En este caso, se quiere construir un ferrocarril metropolitano que conecte 8 barrios de la capital.

[![R](https://img.shields.io/badge/RStudio-R-blue.svg)](https://www.python.org/)

<br>

## Descripción del proyecto

Se quiere construir un ferrocarril metropolitano que conecte 8 barrios de la capital que denotaremos por {A, B, C, D, E, F, G, H}. La duración estimada del viaje directo entre cada dos de los barrios viene dada por la tabla adjunta:

![Captura del video-tracking](https://github.com/aldoegea/ferrocarril-grafos/blob/main/imgs/tabla.png?raw=true)

<br>

## 1. Construcción y visualización del grafo

Importamos la lista de nodos+pesos y lo convertimos en la matriz de adyacencia:


```
#| message: false
#| warning: false

# Leer libros de Excel
library (readxl)
# Manejo de datos
library(tidyverse)
# Mostrar tablas en HTML (datatable)
library(DT)
# Grafos
library(igraph)

# edgelist
edges <- as.data.frame(read_xlsx(path = "./recursos/matriz.xlsx", sheet = "edgelist"))

# cambiamos el index para construir correctamente la matriz
m <- edges[,-1]
rownames(m) <- edges[,1]

# casteamos a matriz
m <- as.matrix(m)

# vista previa rápida
m
```

| A | B | C | D | E | F | G | H |
| -- | -- | -- | -- | -- | -- | -- | -- |
| **A** | 0 | 3 | 5 | 2 | 6 | 3 | 4 | 2 |
| **B** | 3 | 0 | 5 | 4 | 6 | 8 | 3 | 2 |
| **C** | 5 | 5 | 0 | 1 | 4 | 3 | 8 | 2 |
| **D** | 2 | 4 | 1 | 0 | 4 | 2 | 1 | 4 |
| **E** | 6 | 6 | 4 | 4 | 0 | 3 | 5 | 1 |
| **F** | 3 | 8 | 3 | 2 | 3 | 0 | 2 | 4 |
| **G** | 4 | 3 | 8 | 1 | 5 | 2 | 0 | 3 |
| **H** | 2 | 2 | 2 | 4 | 1 | 4 | 3 | 0 |


<br>

Visualizamos el grafo:

```
#| message: false
#| warning: false

# matriz de adyacencia con pesos
ig <- graph_from_adjacency_matrix(m, mode="undirected", weighted=TRUE)

# set seed for reproducibility
set.seed(1)

# create random layout
#layout <- layout_randomly(grafo)

# Colores
V(ig)$color <- rainbow(8, alpha=1)

list <- as.data.frame(as_edgelist(ig))
list <- list[1] |> mutate_each(funs(chartr("ABCDEFGH","12345678",.)))

E(ig)$color <-  apply(X = list, MARGIN = 1, 
                 #FUN = function(x) x[1]) #V(ig)$color[x[1]])
                 FUN = function(x) V(ig)$color[as.numeric(x)])


# plot
plot(ig,
     #layout = layout,
     rescale=TRUE,
     edge.width=2,
     edge.color=E(ig)$color,
     edge.label=E(ig)$weight,
     vertex.size=20,
     vertex.color=V(ig)$color,
     edge.arrow.width=0.7,
     edge.lty= 1,
     margin=0)
```

![Grafo original](https://github.com/aldoegea/ferrocarril-grafos/blob/main/imgs/unnamed-chunk-3-1.png?raw=true)


<br>

## 2. ¿Qué estaciones han de conectarse de forma que la duración del viaje entre el barrio A y cualquier otro barrio sea lo más corto posible?

### PASO INICIAL

Comenzamos indicando la distancia desde el nodo A hasta todos los nodos del siguiente modo y marcando en rojo las etiquetas permanentes:

><b><font style="color:red">uA=0</font>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
uB=3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
uC=5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<font style="color:red">uD=2</font>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
uE=6&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
uF=3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
uG=4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<font style="color:red">uH=2</font></b>

También es importante ir almacenando el predecesor de cada nodo: **pB=pC=pD=pE=pF=pG=pH=A**

### PASO 1 (k=D y k=H, los nodos permanentes del paso previo)

Actualizamos distancias y predecesores del resto:

>uB= min { uB, uD + dDB, uH + dHB } = min { 3, 2+4, 2+2 } = 3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;pB=A

>uC= min { uC, uD + dDC, uH + dHC } = min { 5, 2+1, 2+2 } = 3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;pC=D

>uE = min { uE, uD + dDE, uH + dHE } = min { 6, 2+4, 2+1 } = 3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;pE=H

>uF = min { uF, uD + dDF, uH + dHF } = min { 3, 2+2, 2+4 } = 3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;pF=A

>uG = min { uG, uD + dDG, uH + dHG } = min { 4, 2+1, 2+3 } = 3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;pG=D

### SOLUCIÓN
><b style="color:red">(A, D)	(A, H)	(A, B)	(A, F)	(D, C)	(D, G)	(H, E)</b>

```
#| message: false
#| warning: false
#| echo: false

# edgelist
edges <- data.frame(from=c("A", "A", "A", "A", "D", "D", "H"),
                      to=c("D", "H", "B", "F", "C", "G", "E"),
                      weight=c(2,2,3,3,3,3,3))

# grafo
ig <- graph_from_data_frame(edges, directed = FALSE)

# set seed for reproducibility
set.seed(5)


# Colores
V(ig)$color <- rainbow(8, alpha=1)

list <- as.data.frame(as_edgelist(ig))
list <- list[1] |> mutate_each(funs(chartr("ABCDEFGH","12345678",.)))

#E(ig)$color <-  apply(X = list, MARGIN = 1,
 #                FUN = function(x) V(ig)$color[as.numeric(x)])


# plot
plot(ig,
     #layout = layout,
     rescale=TRUE,
     edge.width=2,
     #edge.color=E(ig)$color,
     edge.label=E(ig)$weight,
     vertex.size=20,
     vertex.color=V(ig)$color,
     edge.arrow.width=0.7,
     edge.lty= 1,
     margin=0)
```

![Conexión óptima entre A y cualquier otro barrio](https://github.com/aldoegea/ferrocarril-grafos/blob/main/imgs/unnamed-chunk-6-1.png?raw=true)

<br>

## 3. Supongamos que los representantes de cada barrio se han reunido con el fin de crear un proyecto que consista en conectar todos los barrios de manera que el coste total de dicho proyecto sea mínimo. Si el coste es directamente proporcional a la duración del viaje entre cada par de barrios.

Se soluciona calculando el árbol de soporte de peso mínimo.

### INICIO

Elegimos un nodo inicial, por ejemplo A, y lo marcamos como “visitado”.

### PASO 1
Entre todas las aristas que salen de A, seleccionamos la de menor coste:

**(A,D) = 2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(A,H) = 2**

Como hay empate la elección es arbitraria, así que escojo el primero: **(A,D) = 2**


### PASO 2

Ahora los nodos visitados son A y D. Se consideran todas las aristas que salgan de dichos nodos a otros no visitados:

**(A,H) = 2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(A,B) = 3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(A,F) = 3&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(A,G) = 4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(A,C) = 5&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(A,E) = 6**

**(D,C) = 1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(D,G) = 1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(D,E) = 4	(D,F) = 2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(D,B) = 4&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(D,H) = 4**

De nuevo tenemos empate, elijo la que primero aparece: **(D,C) = 1**

### PASO 3
Nodos visitados: A, D, C. Se consideran aristas desde esos nodos a otros no visitados; si nos fijamos en el nodo **(D,G) = 1** de antes, lo seleccionamos porque es el menor.

### PASO 4
Nodos visitados: A, D, C, G. Se consideran aristas a no visitados y elegimos **(A,H) = 2** (empate resuelto arbitrariamente).

### PASO 5
Nodos visitados: A, D, C, G, H. Entre las opciones a (B, E, F), seleccionamos **(H,E) = 1** como la menor.

### PASO 6
Nodos visitados: A, D, C, G, H, E. Falta conectar B y F. Entre las conexiones posibles **(H,B) = 2** resulta la menor.

### PASO 7
Nodos visitados: A, D, C, G, H, E, B. Falta F. La menor arista hacia F es **(D,F) = 2**.

### SOLUCIÓN

Al finalizar, el árbol de expansión mínima queda con los nodos:

><b style="color:red">(D, C)=1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(D, G)=1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(H, E)=1&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(A, D)=2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(A, H)=2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(H, B)=2&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(D, F)=2</b>

La suma total se corresponde con el coste mínimo para conectar todos los barrios:

><b style="color:red">1 + 1 + 1 + 2 + 2 + 2 + 2 = 11</b>

```
#| message: false
#| warning: false
#| echo: false

# edgelist
edges <- data.frame(from=c("D", "D", "H", "A", "A", "H", "D"),
                      to=c("C", "G", "E", "D", "H", "B", "F"),
                      weight=c(1,1,1,2,2,2,2))

# grafo
ig <- graph_from_data_frame(edges, directed = FALSE)

# set seed for reproducibility
set.seed(5)


# Colores
V(ig)$color <- rainbow(8, alpha=1)

list <- as.data.frame(as_edgelist(ig))
list <- list[1] |> mutate_each(funs(chartr("ABCDEFGH","12345678",.)))

#E(ig)$color <-  apply(X = list, MARGIN = 1,
 #                FUN = function(x) V(ig)$color[as.numeric(x)])


# plot
plot(ig,
     #layout = layout,
     rescale=TRUE,
     edge.width=2,
     #edge.color=E(ig)$color,
     edge.label=E(ig)$weight,
     vertex.size=20,
     vertex.color=V(ig)$color,
     edge.arrow.width=0.7,
     edge.lty= 1,
     margin=0)
```

![Árbol de expansión mínima](https://github.com/aldoegea/ferrocarril-grafos/blob/main/imgs/unnamed-chunk-10-1.png?raw=true)
