#558 Project 2 Susan Hajmohammad
#Script for API query functions: 


#load necessary packages
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyverse)
library(rlang)
library(purrr)
#plantsm.art API, understand the data: 

#Function All plants:  get all the plant data into a data frame format: 
get_all_plants <- function() {
  #location of plant data assigned to plant_url
  plant_url <- "https://plantsm.art/api/plants.json"
  #download JSON data and convert to object called plant_data
  plant_data <- jsonlite::fromJSON(plant_url, flatten = TRUE)
  #convert JSON data to R df format
  plant_df <- as.data.frame(plant_data)
  
return(plant_df)
  }


#Function 1: This function allows users to specify animal affected, symptom from plant exposure/ consumption, if the data includes an image, and the plant family.  With this function users can potentially identify plants in their environment and the risk posed to a given species of animal, perhaps a pet.  Inversely, if an animal is exhibiting symptoms the user may be able to identify what plant was consumed and look research further. This function also allows users to search by name or part's of a name and see if there is a Wikipedia page for that plant.  This is particularly useful if the user knows vaguely what the plant's name was (i.e. maple) and wants more info. 

#function with 4 variable input options.  If not specified, function will not filter for that column.  
filter_plants <- function(data, animal = NULL, symptom = NULL, has_image = FALSE, plant_family = NULL,  name_keyword = NULL, has_wiki = FALSE, safe = FALSE) {
  
  #make new data object that is filtered for end result
  filtered <- data
  
  #if statements for "safe" tab
  #if animal is not null
  if (!is.null(animal)) {
    #if safe
    if (safe) {
      filtered <- filtered %>%
      #sapply to loop through each row in the animals column
        #keep only rows that do not (!) contain user input
        filter(!sapply(animals, function(x) tolower(animal) %in% tolower(x)))} 
    else {
      #for the toxic tab keep the rows that matches user's input
      filtered <- filtered %>%
        filter(sapply(animals, function(x) tolower(animal) %in% tolower(x)))}}
  
  #keep only rows that match user's input for symptom
  if (!is.null(symptom)) {
    filtered <- filtered %>%
      filter(str_detect(tolower(symptoms), tolower(symptom)))}
  
  #keep only rows that match user's input for family
  if (!is.null(plant_family)) {
    filtered <- filtered %>%
      filter(tolower(family) == tolower(plant_family))}
  
  #keep only rows that have image(s) 
  if (has_image) {
    filtered <- filtered %>%
      filter(lengths(images) > 0)}
  
  #keep only rows that contains users input for name
  if (!is.null(name_keyword)) {
    filtered <- filtered %>%
      filter(str_detect(tolower(common), tolower(name_keyword)))}
  
  #keep only rows that have a wiki link 
  if (has_wiki) {
    filtered <- filtered %>%
      filter(!is.na(wikipedia_url) & wikipedia_url != "")}
  
  return(filtered)}

#test filter_plants
filter_plants(plants, animal = "dog", symptom = "vomiting", name_keyword = "fern", has_image = TRUE)


filter_plants(plants, animal = "horse")


safe_filter <- function(data, animal, has_image = FALSE, has_wiki = FALSE) {
  data %>%
    filter(
      is.na(animals) |
        !sapply(animals, function(x) tolower(animal) %in% tolower(x))
    ) %>%
    { if (has_image) filter(., lengths(images) > 0) else . } %>%
    { if (has_wiki) filter(., !is.na(wikipedia_url) & wikipedia_url != "") else . }
}



