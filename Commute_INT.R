library(data.table)
library(xlsx)
library(dplyr)
library(networkD3)
rm(list=ls())

df <- data.table(read.csv("Commute_Data_2016.csv",  header = T))

From_StJohns <- df[df$From == "St. John's",]
From_Paradise <- df[df$From == "Paradise",]
From_MountPearl <- df[df$From == "Mount Pearl",]
From_Torbay <- df[df$From == "Torbay",]
From_CBS <- df[df$From == "Conception Bay South",]
From_PC <- df[df$From == "Portugal Cove-St. Philip's",]

To_StJohns <- df[df$To == "St. John's",]
To_Paradise <- df[df$To == "Paradise",]
To_MountPearl <- df[df$To == "Mount Pearl",]
To_Torbay <- df[df$To == "Torbay",]
To_CBS <- df[df$To == "Conception Bay South",]
To_PC <- df[df$To == "Portugal Cove-St. Philip's",]



draw_from_sankey <- function(table_from){
  table_from$INT <- paste(table_from$To, "(INT)")
  sourceNodes <- sort(as.vector(unique(table_from$From)))
  targetNodes <- sort(as.vector(unique(table_from$INT)))
  nodes <- 1:length(c(sourceNodes, targetNodes))-1
  names(nodes) <- c(sourceNodes, targetNodes)
  
  edges <- data.frame(source = nodes[as.vector(table_from$From)],
                      target = nodes[as.vector(table_from$INT)],
                      value = as.vector(table_from$Total_Commute))
  nodes <- data.frame(names = c(sourceNodes, targetNodes))
  sankeyNetwork(Links=edges, Nodes=nodes, Source='source',
                Target = 'target', Value = 'value', NodeID = 'names', units = 'Person(s)',
                fontSize = 10, nodeWidth = 20, height = '400px', width = '100%')
}

draw_to_sankey <- function(table_to){
  table_to$INT <- paste(table_to$From, "(INT)")
  sourceNodes <- sort(as.vector(unique(table_to$INT)))
  targetNodes <- sort(as.vector(unique(table_to$To)))
  nodes <- 1:length(c(sourceNodes, targetNodes))-1
  names(nodes) <- c(sourceNodes, targetNodes)
  
  edges <- data.frame(source = nodes[as.vector(table_to$INT)],
                      target = nodes[as.vector(table_to$To)],
                      value = as.vector(table_to$Total_Commute))
  nodes <- data.frame(names = c(sourceNodes, targetNodes))
  sankeyNetwork(Links=edges, Nodes=nodes, Source='source',
                Target = 'target', Value = 'value', NodeID = 'names', units = 'Person(s)',
                fontSize = 10, nodeWidth = 20, height = '400px', width = '100%')
}


draw_from_sankey(From_StJohns)
draw_from_sankey(From_Paradise)
draw_from_sankey(From_MountPearl)
draw_from_sankey(From_Torbay)
draw_from_sankey(From_CBS)
draw_from_sankey(From_PC)
draw_to_sankey(To_StJohns)  # in the draw_to_sankey, frontsize = 6, height = '900px'
draw_to_sankey(To_Paradise)
draw_to_sankey(To_MountPearl)
draw_to_sankey(To_Torbay)
draw_to_sankey(To_CBS)
draw_to_sankey(To_PC)
