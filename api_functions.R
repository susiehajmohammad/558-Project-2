#─ Dependencies ───────────────────────────────────────────────────────────────
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

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

#get rid of duplicate symptoms
library(stringr)  

get_available_symptoms_for_animal <- function(animal, plants = get_all_plants()) {
  plants %>%
    filter(map_lgl(animals, ~ tolower(animal) %in% tolower(.x))) %>%
    pull(symptoms) %>%
    flatten_chr() %>%
    str_to_title() %>%   # unify casing
    unique() %>%
    sort()
}

#─ 1) List all animal types ──────────────────────────────────────────────────
get_available_animals <- function(plants = get_all_plants()) {
  plants %>%
    pull(animals) %>%
    flatten_chr() %>%
    unique() %>%
    sort()
}
# (used to populate every animal dropdown) :contentReference[oaicite:6]{index=6}

#─ 2) Safe plants for a given animal ─────────────────────────────────────────
get_safe_plants_for_animal <- function(animal, plants = get_all_plants()) {
  animal <- tolower(animal)
  if (!animal %in% tolower(get_available_animals(plants))) {
    stop("'", animal, "' is not a valid animal")
  }
  plants %>%
    filter(!map_lgl(animals, ~ animal %in% tolower(.x)))
}
# (for Tab 2: non‐toxic list) :contentReference[oaicite:7]{index=7}

#─ 3) List symptoms for a given animal ────────────────────────────────────────
get_available_symptoms_for_animal <- function(animal,
                                              plants = get_all_plants()) {
  animal <- tolower(animal)
  plants %>%
    filter(map_lgl(animals, ~ animal %in% tolower(.x))) %>%
    pull(symptoms) %>%
    flatten_chr() %>%
    unique() %>%
    sort()
}
# (to drive the checkbox menu in Tab 3) :contentReference[oaicite:8]{index=8}

#─ 4) Toxic plants for a given animal ─────────────────────────────────────────
get_toxic_plants_for_animal <- function(animal, plants = get_all_plants()) {
  animal <- tolower(animal)
  plants %>%
    filter(map_lgl(animals, ~ animal %in% tolower(.x)))
}
# (base filter for Tab 3 & 4) :contentReference[oaicite:9]{index=9}

#─ 5) Further filter by symptoms ─────────────────────────────────────────────
get_toxic_plants_by_animal_and_symptoms <- function(animal,
                                                    symptoms,
                                                    plants = get_all_plants()) {
  # first subset to that animal…
  df_animal <- get_toxic_plants_for_animal(animal, plants)
  # then keep only those whose symptom vector contains *all* selected symptoms
  df_animal %>%
    filter(map_lgl(symptoms, ~ all(tolower(.x) %in% tolower(symptoms))))
}
# (Tab 3: multi‐select symptom filter) :contentReference[oaicite:10]{index=10}

#─ 6) Count toxic plants by severity (for viz) ────────────────────────────────
get_toxic_severity_counts_by_animal <- function(animal,
                                                plants = get_all_plants()) {
  get_toxic_plants_for_animal(animal, plants) %>%
    count(severity_label, name = "plant_count")
}
# (Tab 4: bar‐chart of counts per severity) :contentReference[oaicite:11]{index=11}

