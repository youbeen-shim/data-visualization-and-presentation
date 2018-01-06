library(tidyverse)

#data curated by Cosma Shalizi at CMU
load('nytimes.Rdata')


nytimes.pca = prcomp(nytimes[,-1])

signif(head(sort(nytimes.pca$rotation[,1], decreasing = TRUE), 30), 2)

signif(head(sort(nytimes.pca$rotation[,1], decreasing = FALSE), 30), 2)

nytibble <- tibble(PCA1=nytimes.pca$x[,1], PCA2=nytimes.pca$x[,2], labels=nytimes[,1])

ggplot(nytibble, aes(x=PCA1, y=PCA2))+geom_point(aes(color=labels))
