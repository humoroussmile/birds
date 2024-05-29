#https://googlesheets4.tidyverse.org/

library(googlesheets4)
library(dplyr)
library(tidyr)
library(igraph)
library(GGally)
library(highcharter)

#read in google form from google sheets
#googlesheet.txt is not stored on github for security
companion <- read_sheet(read.delim("googlesheet.txt",header=FALSE)$V1) %>%
  select(c(`Crop`,`Companion Plants`)) %>%
  mutate(`Companion Plants` = paste(Crop, `Companion Plants`, sep = ",")) %>%
  separate_rows(`Companion Plants`, sep = "\\s*,\\s*") %>%
  count(`Crop`, `Companion Plants`) %>%
  na.omit() %>%
  spread(`Companion Plants`, n, fill = 0, drop = FALSE)

# Convert to matrix for plotting
companion.m <- as.matrix(companion)

#get column names of veggies only
matrix <- data.frame(Crop = colnames(companion.m)[-1])

#merge names with matrix to get blank rows
companion.m2 <- merge(matrix, companion.m, by="Crop", all=T) %>%
  mutate(across(everything(), .fns = ~replace_na(.,'0'))) 

#convert all character (number values) to numeric
companion.m2[, 2:ncol(companion.m2)] <- lapply(2:ncol(companion.m2), 
                                               function(x) as.numeric(companion.m2[[x]]))

#change diagonal to 0s
for (i in 2:51) {
  if (companion.m2$Crop[i] == names(companion.m2[i+1])) {
    companion.m2[i,i+1] <- 0
  }
}

#convert all character to numeric
companion.m3 <- companion.m2 %>%
  select(-c("Crop"))
companion.m3 <- as.matrix(as.data.frame(lapply(companion.m3, as.numeric)))

#GGally plot
ggnet2(companion.m3, node.size = 6, node.color = "orange", edge.size = 1, edge.color = "grey", 
       label=TRUE)

#igraph plot
test<-graph_from_adjacency_matrix(companion.m3,weighted=TRUE)
plot(test,
     edge.arrow.size=.4, edge.curved=.1, vertex.color='orange',
     vertex.label.font=4, vertex.label.color="black", vertex.size= 7,
     vertex.label.cex=.9, edge.color="grey40")


#highcharter plot
df <- igraph::as_data_frame(test, what = "edges")
highchartzero() %>%
  hc_title(text = 'Companion Plants') %>%
  hc_subtitle(text = 'Sample Data') %>%
  hc_plotOptions(networkgraph = list(
    keys = c('from', 'to'),
    layoutAlgorithm = list(enableSimulation =  TRUE, friction = -0.9)
    )) %>% 
  hc_add_series(
    df, 
    "networkgraph",
    dataLabels = list(enabled = TRUE, linkFormat = ''),
    hcaes(from = from, to = to, weight = weight),
    layoutAlgorithm = list(enableSimulation = TRUE)
  ) %>% 
  hc_add_dependency("modules/networkgraph.js") %>%
  hc_exporting(enabled = TRUE)
