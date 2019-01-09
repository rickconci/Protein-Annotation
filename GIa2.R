library(tidyverse)


methods <- c("Number of proteins", "swissprot annotated",  "Pfam annotated",  "UniProt annotated") 
species <- c(rep("D.melanogaster" , length(methods)) , rep("D.simulans" , length(methods)) , rep("D.sechellia" , length(methods)) , rep("D.yakuba" , length(methods)) )
counts <- c(30507, 20733, 17085, 30336, 24119, 15644, 13249, 21618, 16469, 10123, 7438, 16125, 23304, 15116, 12947, 23152)


data =data.frame(species,methods,counts)

ggplot(data, aes(fill=methods, y=counts, x=species)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("#504A4B",  "#F9966B", "#41A317","#3090C7" )) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(legend.text = element_text(size = rel(1.1)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_blank(),
        title = element_text(size=rel(1.5)),
        axis.text = element_text(size = rel(1.1)))



methods <- c("swissprot GOs", "Pfam GOs", "UniProt GOs")
species <- c(rep("D.melanogaster" , length(methods)) , rep("D.simulans" , length(methods)) , rep("D.sechellia" , length(methods)) , rep("D.yakuba" , length(methods)) )
counts <- c(252302, 37909, 155997, 188280, 29505, 28819, 101537, 16016, 41729, 180048, 28522, 53657)

data =data.frame(species,methods,counts)

ggplot(data, aes(fill=methods, y=counts, x=species)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(legend.text = element_text(size = rel(1.1)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_blank(),
        title = element_text(size=rel(1.5)),
        axis.text = element_text(size = rel(1.1)))



methods <- c("swissprot GOs with parents", "Pfam GOs with parents", "UniProt GOs with parents")
species <- c(rep("D.melanogaster" , length(methods)) , rep("D.simulans" , length(methods)) , rep("D.sechellia" , length(methods)) , rep("D.yakuba" , length(methods)) )
counts <- c(1675537, 289336, 1098409, 1242065, 221015, 235419, 705049, 122431, 337318, 1192227, 211152, 433149)

data=data.frame(species,methods,counts)

ggplot(data, aes(fill=methods, y=counts, x=species)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(legend.text = element_text(size = rel(1.1)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_blank(),
        title = element_text(size=rel(1.5)),
        axis.text = element_text(size = rel(1.1)))


methods <- c("unique swissprot GOs", "unique Pfam GOs", "unique UniProt GOs")
species <- c(rep("D.melanogaster" , length(methods)) , rep("D.simulans" , length(methods)) , rep("D.sechellia" , length(methods)) , rep("D.yakuba" , length(methods)) )
counts <- c(11548, 1217, 8559, 11595, 1213, 4700, 11690, 1192, 6003, 11541, 1205, 6191)

data2=data.frame(species,methods,counts)

ggplot(data2, aes(fill=methods, y=counts, x=species)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(legend.text = element_text(size = rel(1.1)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_blank(),
        title = element_text(size=rel(1.5)),
        axis.text = element_text(size = rel(1.1)))




methods <- c("unique swissprot GOs with parents", "unique Pfam GOs with parents", "unique UniProt GOs with parents")
species <- c(rep("D.melanogaster" , length(methods)) , rep("D.simulans" , length(methods)) , rep("D.sechellia" , length(methods)) , rep("D.yakuba" , length(methods)) )
counts <- c(15993, 2668, 11951, 16091, 2670, 7872, 16269, 2643, 9325, 15977, 2653, 9511)

data2=data.frame(species,methods,counts)

ggplot(data2, aes(fill=methods, y=counts, x=species)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(legend.text = element_text(size = rel(1.1)),
        axis.title.y = element_text(size = rel(1.1)),
        axis.title.x = element_blank(),
        title = element_text(size=rel(1.5)),
        axis.text = element_text(size = rel(1.1)))





#"Swissprot Gos w/o parents", "Swissprot GOs w/ parents", "Swissprot unique GOs w/o parents", "Swissprot unique GOs w/ parents",
#"PFAM GOs w/o parents", "PFAM GOs w/ parents", "PFAM unique GOs w/ parents", "PFAM unique GOs w/o parents",
#"Uniprot total GOs w/ parents", "Uniprot GOs w/o parents", "unique Uniprot GOs w/o parents", "unique Uniprot w/ parents

d.mel.values <- c(30507, 20733, 252302, 1675537, 11548, 15993, 17085, 37909, 289336, 1217, 2668, 0, 155997, 0, 8559, 0)
d.sim.values <- c(24119, 15644, 188280, 1242065, 11595, 16091, 13249, 29505, 221015, 1213, 1670, 0, 28819, 0, 4700, 0)
d.sec.values <- c( 16469, 10123, 101537, 705049, 11690, 16269, 7438, 16016, 122431, 1192, 2643, 0, 41729, 0, 6003, 0)
d.yak.values <- c(23304, 15116, 180048, 1192227, 11541, 15977, 12947, 28522, 211152, 1205, 2653, 0, 53657, 0, 6191, 0)

dmel.norm <- d.mel.values/d.mel.values *100
dsim.norm <- d.sim.values/d.mel.values * 100
dsec.norm <- d.sec.values/d.mel.values * 100
dyak.norm <- d.yak.values/d.mel.values * 100

values.norm <- c(dmel.norm, dsim.norm, dsec.norm, dyak.norm)
