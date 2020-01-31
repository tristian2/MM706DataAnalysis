library(jsonlite)
library(tidyverse)
data.frame(do.call("rbind", json_file))

dataFolder <- "data/recipes" 
# or 
#dataFolder <- "data/recipesSample" #during development

json_files<-list.files(path =dataFolder,pattern="*.json",full.names = TRUE) #combine all the json files from a folder
json_files

recipeText <- data.frame(id=character()
                      ,name=character()
                      ,ingredients=character()
                      ,stringsAsFactors = FALSE)

pages <- list()
row <-0

for(i in json_files){
  for(j in i){
    row<-row+1
    mydata <- fromJSON(i,flatten = TRUE)
    message("Retrieving page ", row, " ", i)
    message("Found ", mydata$id)
    ingredients<-""
    ingredients<-mydata$ingredientLines
    result = tryCatch({
      recipeText[nrow(recipeText) + 1,] = c(id=mydata$id
                                      , name=mydata$name
                                      , ingredients=ingredients
      )
    }, warning = function(w) {
      #warning-handler-code
    }, error = function(e) {
      message("error processing ", mydata$id)
    }, finally = {
      #cleanup-code
    })
    
  }
}

view(recipeText)
nrow(recipeText)
colnames(recipeText)
