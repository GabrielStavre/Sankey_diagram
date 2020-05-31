############################
### Build Sankey Diagram ###
############################

###################################################################

# Housekeeping
rm(list = ls())

# Set working directory
setwd("~/Documents/Plots/Sankey diagram/_GitHub version")

# Libraries
library(readxl)
library(networkD3)
library(tidyverse)

# Load data
legend <- read_xlsx("DB.xlsx", sheet = "legend")
matrix_1 <- read_xlsx("DB.xlsx", sheet = "matrix_1")
matrix_2 <- read_xlsx("DB.xlsx", sheet = "matrix_2")

# Make setup to long format
DB_1 <- matrix_1 %>%
  gather(key = 'key', value = 'value', -index)
colnames(DB_1) <- c("source", "target", "value")

DB_2 <- matrix_2 %>%
  gather(key = 'key', value = 'value', -index)
colnames(DB_2) <- c("target", "source", "value")

# Checks
print(unique(DB_1$source))
print(unique(DB_1$target))
print(unique(DB_2$source))
print(unique(DB_2$target))

# Create DFs for Sankey diagram
links <- rbind.data.frame(DB_1,DB_2)
links$source <- as.numeric(links$source)
links$target <- as.numeric(links$target)
nodes <- data.frame(legend$Sector)
colnames(nodes) <- c("name")

# Some insights
hist(links$value)
summary(links$value)

# Create groups by setting some flags
###################################################################
nodes$position <- seq.int(nrow(nodes))
nodes$group <- c("same")

# IC - intermediate consumption
nodes[nodes$name == "IC",]$group <- "IC"
nodes$group <- as.factor(nodes$group)

sectors1 <- c(
  "C",
  "D",
  "Food industry (C10-12)",
  "Auto industry (C29)",
  "Energy (D)"
  )

sectors2 <- c(
  "J",
  "Telecom (J61)",
  "IT (J62-63)"
  )

sectors3 <- c(
  "I",
  "HoReCa (I)"
  )

sectors4 <- c(
  "F",
  "Construction (F)"
  )

sectors5 <- c(
  "K",
  "M",
  "N",
  "Financial (K64)",
  "Legal, accounting, consultancy (M69-70)",
  "Support & other services (N80-82)"
  )

# Make a vector with corresponding positions for each sector
s1 <- nodes[nodes$name %in% sectors1,]$position-1
s2 <- nodes[nodes$name %in% sectors2,]$position-1
s3 <- nodes[nodes$name %in% sectors3,]$position-1
s4 <- nodes[nodes$name %in% sectors4,]$position-1
s5 <- nodes[nodes$name %in% sectors5,]$position-1

# Add flag
links$group <- "N"
links[links$source %in% s1,]$group <- "s1"
links[links$source %in% s2,]$group <- "s2"
links[links$source %in% s3,]$group <- "s3"
links[links$source %in% s4,]$group <- "s4"
links[links$source %in% s5,]$group <- "s5"



# Add other features (optional)
# ( !!! )
###################################################################
# # separate IC from FC, INV & EXP
# links <- rbind(links,c(24,25,10,"blank"))
# links <- rbind(links,c(24,26,10,"blank"))
# links <- rbind(links,c(24,27,10,"blank"))
# 
# # add a rewind arrow
# # to go from IC to each sector
# for (i in 0:3) {
# 
#   links <- rbind(links,c(24,i,1,"N"))
# 
# }
###################################################################



# Drop links with small flows
links <- links[links$value>=1,]


# Give a color for each group
###################################################################
my_color <- 'd3.scaleOrdinal() .domain(["N","s1","s2","s3","s4","s5","same","IC","blank"]) 
.range(["grey","#00ADEF","#BD0071","#4B51A3","#F16524","#B3B20F","#C5442D","#006957","white"])'


# Format columns
links$source <- as.numeric(links$source)
links$target <- as.numeric(links$target)
links$value <- as.numeric(links$value)
links$group <- as.factor(links$group)
nodes$group <- as.factor(nodes$group)


# Sankey diagram
###################################################################
# interations = 0 to keep order
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 10, nodeWidth = 10,
              colourScale=my_color, LinkGroup="group", NodeGroup="group",iterations = 0)


