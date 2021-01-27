rsconnect::setAccountInfo(name='sean-conroy', token='BF1B412FC32FB289B60F6F19E4715E85', secret='tBz2CyN1rOqSUAK+sYGixo/vAGkm1x9QppmxjKdj')


# Setup Shiny
setwd("C:/Users/sconroy/Documents/meystingray.github.io/ShinyMurderMap")
setwd("C:/Users/sconroy/Documents/meystingray.github.io/ShinyTraffic")
setwd("C:/Users/sconroy/Documents/meystingray.github.io/ShinyCrimeExplorer")

library(shiny)
library(rsconnect)

runApp("C:/Users/sconroy/Documents/meystingray.github.io/ShinyMurdermap")
runApp("C:/Users/sconroy/Documents/meystingray.github.io/ShinyTraffic")
runApp("C:/Users/sconroy/Documents/meystingray.github.io/ShinyCrimeExplorer")

rsconnect::deployApp("C:/Users/sconroy/Documents/meystingray.github.io/ShinyMurderMap")
rsconnect::deployApp("C:/Users/sconroy/Documents/meystingray.github.io/ShinyTraffic")

#library(rdrop2)
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")

