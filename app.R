# app.R
# Susan Hajmohammad
# 558 Summer 2025

##############################################
# 0) Load packages 
##############################################

library(shiny)
library(dplyr)
library(purrr)
library(ggplot2)
library(gganimate)
library(gifski)
library(tidyr)
library(stringr)
library(jsonlite)
library(shinythemes)


###################################################
# 1) helper functions 
###################################################


#api querying functions: 
# put here so user can run with one click 

#function to get all data from api
get_all_plants <- function() {
  #flattening data because there are may lists in columns
  fromJSON("https://plantsm.art/api/plants.json", flatten = TRUE) %>%
    #make tibble to work with
    as_tibble() %>%
    #renaming these for convenience only
    rename(
      severity_label = severity.label, #word level
      severity_level = severity.level #numeric level
    )
}

#get the whole list of animals function 
get_available_animals <- function(plants = get_all_plants()) {
  plants %>%
    #pull all the lists of animals
    pull(animals) %>%
    #flatten it into one big vector
    flatten_chr() %>%
    #used unique() because there were duplicates
    unique() %>%
    sort()
}

#function to get all the symptoms for each animal
get_available_symptoms_for_animal <- function(animal, plants = get_all_plants()) {
  plants %>%
    #filter for only the rows that match user's animal input
    #to lower to make all lower case
    filter(map_lgl(animals, ~ tolower(animal) %in% tolower(.x))) %>%
    #pulling the list
    pull(symptoms) %>%
    #flattening
    flatten_chr() %>%
    #remove extra spaces
    str_trim() %>%
    #make each word capitalized
    str_to_title() %>%
    #get rid of duplicates
    unique() %>%
    #put in alphabetical order
    sort()
}

#function that returns the NOT TOXIC plants given the animal input
get_safe_plants_for_animal <- function(animal, plants) {
  #take animal input
  animal <- tolower(animal)
  #keep all the rows that dont contain the user's animal input (!)
  plants %>% filter(!map_lgl(animals, ~ animal %in% tolower(.x)))
}
#function that returns the  TOXIC plants given the animal input
get_toxic_plants_for_animal <- function(animal, plants) {
  #take animal input
  animal <- tolower(animal)
  #keep all the rows that contain the user's animal input
  plants %>% filter(map_lgl(animals, ~ animal %in% tolower(.x)))
}

# This function displays the plant images from their URL. 
#get the first source_url from each data frame of image URLs
get_image_url <- function(images_entry) {
  if (is.data.frame(images_entry) && "source_url" %in% names(images_entry)) {
    #return just the first entry with indexing [1]
    return(as.character(images_entry$source_url[1]))
  }
  #otherwise just return NA
  return(NA_character_)
}


###############################################
# 2) Code for UI: 
###############################################


#navigation bar
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
  #navbar title
  title = "Explore Plant Safety for Animals",
#__________________________________________________________  
  # Home Tab: introduction and instructions for user
  #the navbar button to get to this tab
  tabPanel("Home", icon = icon("home"),
    fluidRow(
      column(8,
             h2("🪴 You've arrived at the Plant Safety and Identification App!"),
             p("This app helps you explore which plants are safe or toxic for your dog, cat, and reptile friends. You can search by plant family, symptoms, and see which species are pet-safe or dangerous."),
             #spacing
             br(),
             
             #title for instructions
             h4("🔍 How this app works:"),
             tags$ul(tags$li(HTML("<b>Find Safe Plants</b> tab allows you to browse plants that are non-toxic to pets.")),
                     tags$li(HTML("<b>Find Toxic Plants</b> tab filters by animal and symptom.")),
                     tags$li(HTML("<b>Compare Families</b> tab will help you visualize which plant families are most toxic.")),
                     tags$li(HTML("Select <b>At a Glance</b> for a quick look at how different plant families are harmful to different animals. ")),
                     tags$li(HTML("<b>Symptoms</b> tab will show you the top 10 symptoms exhibited by each animal across all plant type exposure.")),
                     tags$li(HTML("More about this project in the <b>About</b> tab"))),
             br(),
             #info about app build
             p("This data is sourced from a public plant toxicity API. App was built in R with Shiny.")),
      
      #new column
             column(4,
                    fluidRow(
                      #upload saved pet pics in project file
                      column(4, tags$img(src="dog.png",     width="100%", style="border-radius:12px;box-shadow:0 0 6px gray;")),
                      column(4, tags$img(src="cat.png",     width="100%", style="border-radius:12px;box-shadow:0 0 6px gray;")),
                      column(4, tags$img(src="reptile.png", width="100%", style="border-radius:12px;box-shadow:0 0 6px gray;"))
                    ),
                    br(),
                    tags$ul(tags$li("Inspired by Jackie, Luca, Olive"))
             )
           )
  ),
#___________________________________________________________  
  # Safe Plants Tab
  tabPanel("Safe Plants", icon = icon("heart"),
           sidebarLayout(
             sidebarPanel(
               h4("Select Animal"),
               #dropdown menu using available animal choices
               selectInput("safe_animal", NULL, choices = get_available_animals())
             ),
             mainPanel(
               #Title
               h4("Plants Safe for Selected Animal"),
               #save a spot for the list widget
               uiOutput("safe_plants_list")
             ))),
#_________________________________________________________ 
  # Toxic Plants Tab
  tabPanel("Toxic Plants", icon = icon("skull-crossbones"),
           sidebarLayout(
             sidebarPanel(
               h4("Select Animal & Symptoms"),
               #dropdown with available animal choices
               selectInput("toxic_animal", "Animal:", choices = get_available_animals()),
               #Spot for relevant symptoms widget
               uiOutput("symptoms_ui")
             ),
             mainPanel(
               #title
               h4("Toxicity Levels"),
               #spot for plot
               plotOutput("toxic_severity_plot", height = "200px"),
               #title
               h4("Toxic Plants"),
               #spot for list
               uiOutput("toxic_plants_list")
             ))),
#_________________________________________________________  
  # Compare Families Tab
  tabPanel("Compare Families", icon = icon("leaf"),
           sidebarLayout(
             sidebarPanel(
               h4("Select Animal"),
               #dropdown for animal selection
               selectInput("viz_animal", NULL, choices = get_available_animals()),
               #spacing for textbox
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               #well panel for textbox
               wellPanel(
                 style = "margin-top: 1em; padding: 0.5em; background: #f7f7f7; border-radius: 4px;",
                 "🛒🌵 Going plant shopping?  Find plant families to avoid for your fur and scale babies."
               )
               ),
             mainPanel(
               h4("Toxic Plants by Family"),
               #spot for plot
               plotOutput("severity_plot", height = "500px")
             )
           )
  ),
#______________________________________________________  
  # At a Glance Tab
  tabPanel("At a Glance", icon = icon("eye"),
           fluidRow(
             column(12,
                    #title
                    h4("Top 10 Toxic Families per Animal"),
                    #spot for animated plot
                    imageOutput("bar_race", height = "600px")))),
#_________________________________________________________  
  # Symptoms Tab
  tabPanel("Symptoms", icon = icon("stethoscope"),
           sidebarLayout(
             sidebarPanel(
               #header
               h4("Select Animal"),
               #dropdown
               selectInput("sym_animal", NULL, choices = get_available_animals())
             ),
             mainPanel(
               #header
               h4("Top 10 Symptoms"),
               #space for symptoms plot
               plotOutput("symptom_freq", height = "400px")))),
#_________________________________________________________  
  # About Tab
  tabPanel("About", icon = icon("book"),
    fluidRow(
      column(8,
             #text information for users
      h3("About This Project"),
                    
      p("This app was created for users to explore which plants may be toxic for the animals in their lives!"),
                    
      p("All data is retrieved from a public API on plant toxicity called plantsm.art. Using this app, you can filter plants by toxicity, species, and symptoms, and compare toxic plant families visually."),
                    
      p("This was built using R and the Shiny package as well!")),
      column(4,
      h4("Author"),
      p("Susan Hajmohammad"),
      p("Graduate Student, Statistics Department at NC State "),
      p("Email: sahajmoh@ncsu.edu"),
      br(),
      h4("Sources"),
      #reference links for user
      tags$ul(tags$li(a("Plantsmart API", href = "https://plantsm.art/api/plants.json", target = "_blank")),
                            
      tags$li(a("Emojipedia (for emojis on app)", href = "https://emojipedia.org/", target = "_blank"))
           ))))))


###########################################################
# 3) Server code 
###########################################################


server <- function(input, output, session) {
  
  # Load the data and flatten it.  
  all_plants <- reactive({
    get_all_plants() %>%
      mutate(
        #attach symptoms not duplicated by capitals
        symptoms  = map(symptoms, ~ as.character(unlist(.x))),
        #include only the first image url for each row
        image_url = map_chr(images, get_image_url)
      )
  })
#______________________________________________________  
  # Safe Plants List
  #make a list of the "safe" plants for animals 
  output$safe_plants_list <- renderUI({
    #don't run unless animal is selected
    req(input$safe_animal)
    #make data frame of plants safe for that animal using function
    df <- get_safe_plants_for_animal(input$safe_animal, all_plants())
    #if output has no rows display message none found
    validate(need(nrow(df)>0, "No safe plants found."))
    #loop through each row of safe plants and add to scrolling list
    #keep plant name, image and wiki link only
    entries <- lapply(seq_len(nrow(df)), function(i) {
      #get image for this plant
      url    <- df$image_url[i]
      #if no image url, display message
      imgTag <- if (!is.na(url) && nzchar(url))
        img(src=url, height="100px")
      else
        p("No image available")
      
      #make one taglist with plant name, image and wikilink
      tagList(
        h5(df$name[i]),
        imgTag,
        a("Wiki", href=df$wikipedia_url[i], target="_blank"),
        br(), 
        #horizontal lines to separate rows
        tags$hr()
      )
    })
    #formatting
    div(style="height:400px;overflow-y:auto;", 
    do.call(tagList, entries))
  })
#___________________________________________________  
  # Symptoms selector for toxic plants
  output$symptoms_ui <- renderUI({
    req(input$toxic_animal)
    #get list of all symptoms associated with selected animal
    syms <- get_available_symptoms_for_animal(input$toxic_animal, all_plants())
    #if there aren't any symptoms, display message
    validate(need(length(syms)>0, "No symptom data."))
    #make checkbox selector for all the available symptoms per animal
    checkboxGroupInput("selected_symptoms", "Symptoms:", choices=syms)
  })
#____________________________________________________  
  # Toxic Plants severity plot
  output$toxic_severity_plot <- renderPlot({
    req(input$toxic_animal)
    #make df from plants that are toxic to selected animal
    df <- get_toxic_plants_for_animal(input$toxic_animal, all_plants()) %>%
      #count how many plants for each severity level
      count(severity_label, name="n")
    # if none for that animal display message
    validate(need(nrow(df)>0, "No data."))
    #bar chart for display severity type counts
    ggplot(df, aes(severity_label, n)) +
      geom_col(fill="#B7410E") +
      labs(x="Severity", y="Count") +
      theme_minimal()
  })
#_____________________________________________________  
  # Toxic Plants List
  output$toxic_plants_list <- renderUI({
    req(input$toxic_animal)
    #df for all plants that are toxic to selected animal
    df <- get_toxic_plants_for_animal(input$toxic_animal, all_plants())
    #if any symptoms are checked proceed
    if (!is.null(input$selected_symptoms)) {
      #make lower case
      sel <- tolower(input$selected_symptoms)
      #filter and keep the rows where the selected symptoms appear for that plant
      df  <- df %>% filter(map_lgl(symptoms, ~ all(sel %in% tolower(.x))))
    }
    
    #if there are no plants, display message
    validate(need(nrow(df)>0, "No toxic plants match criteria."))
    #create an entry for each plant that meet criteria
    entries <- lapply(seq_len(nrow(df)), function(i) {
      #plant first image
      url    <- df$image_url[i]
      #if none display message
      imgTag <- if (!is.na(url) && nzchar(url))
        img(src=url, height="100px")
      else
        p("No image available")
      
      #make scrollable list with name, image, wiki link
      tagList(
        h5(df$name[i]),
        imgTag,
        a("Wiki", href=df$wikipedia_url[i], target="_blank"),
        br(), tags$hr()
      )
    })
    #formatting for entries in list
    div(style="height:400px;overflow-y:auto;", do.call(tagList, entries))
  })
#______________________________________________________  
  # Compare Families Plot
  output$severity_plot <- renderPlot({
    req(input$viz_animal)
    #df of only toxic plants for animal
    df <- get_toxic_plants_for_animal(input$viz_animal, all_plants()) %>%
      count(family, name="plant_count") %>%
      arrange(desc(plant_count))
    validate(need(nrow(df)>0, "No data."))
    #reorder so graph is by amount
    ggplot(df, aes(reorder(family, plant_count), plant_count)) +
      #color and horizontal
      geom_col(fill="#D8BFD8") + coord_flip() +
      labs(x="Family", y="Count") +
      theme_minimal()
  })
#________________________________________________________  
  # data for advanced level chart
  #prepare data for race chart
  race_data <- reactive({
    get_all_plants() %>%
      #expand so each animal is a row, not nested
      unnest(animals) %>%
      #animal names to lower case
      mutate(animal = tolower(animals)) %>%
      #group and count amount of toxic plants
      group_by(animal, family) %>%
      summarise(plant_count = n(), .groups="drop") %>%
      #group into animal
      group_by(animal) %>%
      #make descending count for chart
      arrange(desc(plant_count)) %>%
      #only top 10
      slice_head(n=10) %>%
      ungroup()
  })
#______________________________________________________  
  # Race Chart Animation
  output$bar_race <- renderImage({
    #ggplot with prepared data, color for ea family
    p <- ggplot(race_data(),
                aes(x=plant_count, y=reorder(family, plant_count), fill=family)) +
      #horizontal bar chart
      geom_col(show.legend=FALSE) +
      labs(title='Top 10 Toxic Families for {closest_state}',
           x='Count', y='Family') +
      theme_minimal() +
      #animate by transitioning through each animal as a separate state
      #variable and #of frames
      transition_states(animal, transition_length=4, state_length=1)
    #make animation into gif
    anim <- animate(p, nframes=80, fps=10, width=800, height=600,
    #loop so repeats gif
    renderer=gifski_renderer(loop=TRUE))
    
    #temporary save
    outfile <- tempfile(fileext='.gif')
    anim_save(outfile, anim)
    
    list(src=outfile, contentType='image/gif')
  }, deleteFile=TRUE)
#________________________________________________________  
  # Symptoms Frequency Plot
  output$symptom_freq <- renderPlot({
    req(input$sym_animal)
    #df of plants toxic to animal then get symptoms
    df <- get_toxic_plants_for_animal(input$sym_animal, all_plants()) %>%
      unnest(symptoms) %>%
      #count number of occurences
      count(symptoms, sort=TRUE) %>%
      #keep only top 10 freq symptoms  
      slice_head(n=10) %>%
      #convert to Title case 
      mutate(symptoms=str_to_title(symptoms))
    validate(need(nrow(df)>0, "No data."))
    
    #bar chart with ggplot
    ggplot(df, aes(reorder(symptoms, n), n)) +
      #flip axis so labels stack vertical
      geom_col(fill="#C55C8A") + coord_flip() +
      labs(x="Symptom", y="Count") +
      theme_minimal()
  })
}


##################
# 4) Launch  app 
###################

shinyApp(ui, server)
