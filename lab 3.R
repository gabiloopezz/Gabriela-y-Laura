library(ape)
library(phangorn)
library(phytools)


fraxatin <- read.phyDat(file = "fraxatin_aligned.fasta",format = "FASTA", type ="AA")
matrizdist <- as.AAbin(fraxatin)
matrizdist <- dist.aa(matrizdist)
print(matrizdist)
install.packages("phangorn")
library(phangorn)

fraxatin <- read.phyDat(file = "fraxatin_aligned.fasta", 
                        format = "FASTA", type = "AA")
print(fraxatin)

matrizdist <- as.AAbin(fraxatin)

matrizdist <- dist.aa(matrizdist)
print(matrizdist)

 install.packages("phangorn")  
library(phangorn)
 arbolUPGMA <- upgma(matrizdist)


 install.packages("phangorn")
 library(phangorn)
 

 fraxatin <- read.phyDat(file = "fraxatin_aligned.fasta", format = "FASTA", type = "AA")
 
 matrizdist <- as.AAbin(fraxatin)
 
 matrizdist <- dist.aa(matrizdist)

 arbolUPGMA <- upgma(matrizdist)
 
 plot(arbolUPGMA) 

arbolNJ <- nj(matrizdist)
plot(arbolNJ)

library(ape)
 

 matrizdist <- as.dist(matrix(c(0, 5, 10, 15,
                                5, 0, 20, 25,
                                10, 20, 0, 30,
                                15, 25, 30, 0), 
                              nrow = 4, byrow = TRUE))
 
plot(arbolUPGMA, type = "phylogram", cex = 0.8, edge.width = 2, edge.color = "red", font = 3)
plot(arbolUPGMA, type = "cladogram", cex = 0.8, edge.width = 2, edge.color = "blue", font = 2)
 arbolNJ <- nj(matrizdist)
 plot(arbolNJ)
 
plot(arbolUPGMA, type = "fan", cex = 0.8, edge.width = 1.5, edge.color = "green", font = 1)

plot(arbolUPGMA, type= "p", label.offset=0.0005, edge.lty=1, node.pos=2, cex=0.8, edge.width=2, edge.color="purple", font=3)

plotTree(arbolNJ) 

plotTree(arbolNJ, ftype="b", fsize=0.8, offset=1, color="orange", lwd=2) 

plotTree(ladderize(arbolNJ))

arbolNJraiz <-root(arbolNJ, outgroup = "Ornitorrinco", r = TRUE)
plot(arbolNJraiz)

arbolUPGMAraiz <-root(arbolUPGMA, outgroup = "Ornitorrinco", r=TRUE)
plot(arbolUPGMAraiz)

layout(matrix(c(1,2)), height=c(10,10))
par(mar=c(1,1,1,1))
plot(arbolUPGMAraiz, label.offset=0.0005, main="ARBOL UPGMA", cex=0.4)
plot(arbolNJraiz, label.offset=0.0005, main="ARBOL NJ", cex=0.4)

parsimony(arbolUPGMAraiz, fraxatin)

parsimony(arbolUPGMA, fraxatin)

mejorUPGMA <- optim.parsimony(arbolUPGMAraiz, fraxatin)
 mejorNJ <- optim.parsimony(arbolNJraiz, fraxatin)
 fraxatin_parsimonia <- pratchet(fraxatin, all = TRUE)
 fraxatin_parsimonia
 fraxatin_parsimoniaR <- root(phy = fraxatin_parsimonia, outgroup = "Ornitorrinco")
 plot(fraxatin_parsimoniaR, cex = 0.6)
 estrictode100 <- consensus(fraxatin_parsimoniaR, p = 1)
 plot(estrictode100, cex = .6)
 estrictode30 <- consensus(fraxatin_parsimoniaR, p = 0.3)
 plot(estrictode30, cex = .6)
 arbolesbootstrap <- bootstrap.phyDat(fraxatin, FUN = pratchet, bs = 10)
 plot(arbolesbootstrap, cex = .6)
 estricto60 <- consensus(arbolesbootstrap, p = 0.6)
 plot(estricto60, cex = .6)
 arbolazar <- rtree(n = 11, tip.label = names(fraxatin))
 plot(arbolazar, cex = .5)
 arbolazarR <- root(phy = arbolazar, outgroup = "Ornitorrinco")
 plot(ladderize(arbolazarR), cex = .5); add.scale.bar()
 ajustado <- pml(arbolazarR, fraxatin)
 ajustado
 ajustadoconDay <- optim.pml(object = ajustado, model = "Dayhoff", rearrangement = "ratchet")
 ajustadoconDay$tree
 ajustadoconDayraíz <- root(ajustadoconDay$tree, outgroup = "Ornitorrinco")
 plot(ladderize(ajustadoconDayraíz), cex = .5); add.scale.bar()
 ajustadoconBlo <- optim.pml(object = ajustado, model = "Blosum62", rearrangement = "ratchet")
 ajustadoconJTT <- optim.pml(object = ajustado, model = "JTT", rearrangement = "ratchet")
 AIC(ajustadoconDay, ajustadoconBlo, ajustadoconJTT)
 mejorarbol <- optim.pml(
   object = ajustadoconDay, 
   model = "JTT", 
   rearrangement = "ratchet")
 mejorarbol
 mejorarbolR <- root(mejorarbol$tree, outgroup = "Ornitorrinco")
 plot(ladderize(mejorarbolR), cex = 0.5); add.scale.bar()