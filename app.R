library(readxl)
library(igraph)
library(hues)

nodeData <- read_excel(path = "fhact50data.xlsx", sheet = "Nodes")
edgeData <- read_excel(path = "fhact50data.xlsx", sheet = "Edges")
netData <- graph_from_data_frame(d = edgeData, 
                                 vertices = nodeData,
                                 directed = TRUE)
V(netData)$id <- seq.int(from = 1,to = length(V(netData)))
nodePal <- iwanthue(n = length(unique(V(netData)$type)),0,360,23,100,60,100)

E(netData)$color <- ifelse((E(netData)$type == "Natural"),"darkgreen","NULL")
E(netData)$color <- ifelse((E(netData)$type == "Physical"),"brown4",E(netData)$color)
E(netData)$color <- ifelse((E(netData)$type == "Financial"),"forestgreen",E(netData)$color)
E(netData)$color <- ifelse((E(netData)$type == "Social"),"cadetblue",E(netData)$color)
E(netData)$color <- ifelse((E(netData)$type == "Cultural"),"darkgoldenrod",E(netData)$color)
E(netData)$color <- ifelse((E(netData)$type == "Human"),"coral",E(netData)$color)
E(netData)$color <- ifelse((E(netData)$type == "Political"),"purple",E(netData)$color)

kk <- layout_with_kk(netData)
plot(netData,
     layout=kk,
     main = "Franklinton, Ohio Affordable Housing Network",
     vertex.frame.color=NA,
     vertex.size=4,
     vertex.color=nodePal[as.numeric(as.factor(vertex_attr(netData,"type")))],
     vertex.label=V(netData)$id,
     vertex.label.cex = .6,
     vertex.label.family = "Helvetica",
     edge.arrow.size=.2,
     edge.color=E(netData)$color)
legend(x = -.9,y = .8,
       legend = unique(V(netData)$type),
       pt.bg = nodePal,
       pch = 21,
       bty = "n",
       cex = .6,
       title = "Node Type")
legend(x = -.9,y = 0.5,
       legend = unique(E(netData)$type),
       pt.bg = unique(E(netData)$color),
       pch = 21,
       bty = "n",
       cex = .6,
       title = "Edge Type")
legend("bottom",
       legend = paste(V(netData)$id,V(netData)$name,sep=" - "),
       bty = "n",
       title = "Node Names",
       ncol = 4,
       cex = .5,
       y.intersp = .9)
