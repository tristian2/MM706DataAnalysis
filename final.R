library(jsonlite)
library(tidyverse)
data.frame(do.call("rbind", json_file))

dataFolder <- "data/recipes" 
# or 
#dataFolder <- "data/recipesSample" #during development

json_files<-list.files(path =dataFolder,pattern="*.json",full.names = TRUE) #combine all the json files from a folder
json_files

recipes <- data.frame(id=character()
                          ,name=character()
                          ,cuisine=character()
                          ,source=character()               
                          ,sodium=double()
                          ,vitc=double()
                          ,protein=double()
                          ,carbs=double()
                          ,sugar=double()
                          ,energy=double()
                          ,vegetarian=logical()
                          ,allergens=character()
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
    #pages[[i]] <- recipe
    #recipes %>% add_row(id=mydata$id, name=mydata$name, cuisine=mydata$attributes$cuisine)
    #add_row(recipes, id = "xxx")
    sodium <- match(c('NA'),mydata$nutritionEstimates$attribute)
    valSodium <- mydata$nutritionEstimates$value[sodium]
    vitc <- match(c('VITC'),mydata$nutritionEstimates$attribute)
    valvitc <- mydata$nutritionEstimates$value[vitc]
    protein <- match(c('PROCNT'),mydata$nutritionEstimates$attribute)
    valprotein <- mydata$nutritionEstimates$value[protein]
    carbs <- match(c('CHOCDF'),mydata$nutritionEstimates$attribute)
    valcarbs <- mydata$nutritionEstimates$value[carbs]
    sugar <- match(c('SUGAR'),mydata$nutritionEstimates$attribute)
    valsugar <- mydata$nutritionEstimates$value[sugar] 
    energy <- match(c('ERNERC_KCAL'),mydata$nutritionEstimates$attribute)
    valenergy <- mydata$nutritionEstimates$value[energy]   
    source<-""
    vegetarian <- FALSE;
    allergens<-""
    ingredients<-""
    
    #allergens
    if (any(str_detect(mydata$ingredientLines, "egg"))) {
      allergens<-
      paste0(allergens,"egg", sep = ", ")
    }
    if (any(str_detect(mydata$ingredientLines, "chicken"))) {
      allergens<-
        paste0(allergens,"chicken", sep = ", ")
    }
    if (any(str_detect(mydata$ingredientLines, "wheat"))) {
      allergens<-
        paste0(allergens,"wheat", sep = ", ")
    }
    if (any(str_detect(mydata$ingredientLines, "peanuts"))) {
      allergens<-
        paste0(allergens,"peanuts", sep = ", ")
    }
    if (any(str_detect(mydata$ingredientLines, "seafood"))) {
      allergens<-
        paste0(allergens,"seafood", sep = ", ")
    }
    
    #ingredients
    if (any(str_detect(mydata$ingredientLines, "chili"))) {
      ingredients<-
        paste0(ingredients,"chili", sep = ", ")
    }
    if (any(str_detect(mydata$ingredientLines, "garlic"))) {
      ingredients<-
        paste0(ingredients,"garlic", sep = ", ")
    }
    if (any(str_detect(mydata$ingredientLines, "soy") || str_detect(mydata$ingredientLines, "glutam")  )) {
      ingredients<-
        paste0(ingredients,"umami", sep = ", ")
    }
    
    #vegetarian
    if (any(str_detect(mydata$attribution$text, "vegetarian"))) {
      vegetarian<-TRUE
    }
    
    #salt
    result = tryCatch({
      recipes[nrow(recipes) + 1,] = c(id=mydata$id
                                      , name=mydata$name
                                      , cuisine=tail(mydata$attributes$cuisine, n=1)
                                      , source=mydata$source$sourceDisplayName
                                      , sodium=valSodium
                                      , vitc=valvitc
                                      , protein=valprotein
                                      , carbs=valcarbs
                                      , sugar=valsugar
                                      , energy=valenergy
                                      , vegetarian=vegetarian
                                      , allergens=allergens
                                      , ingredients=ingredients
           )
    }, warning = function(w) {
      #warning-handler-code
    }, error = function(e) {
      soldium = NA
    }, finally = {
      #cleanup-code
    })
  
  }
}



view(recipes)
nrow(recipes)
colnames(recipes)
