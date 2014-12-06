library(shiny)
library(ggvis)

m <- read.csv("aapr.csv", stringsAsFactors = TRUE)

## Define the overall UI
shinyUI(

    ## Use a fluid Bootstrap layout
    fluidPage(

        ## Give the page a title
        titlePanel("Reconstructed AAPR task force scatterplots "),

        ## Generate a row with a sidebar
        sidebarLayout(

            ## Define the sidebar with one input
            sidebarPanel(

                textInput("department_word", label = "Word in Department"),

                selectInput("category", "", c("Academic", "Administrative")),

                uiOutput("faculties_list"),

                ## h3("Faculty"),
                ## checkboxGroupInput("selected_faculties", label = "", choices = levels(m$Faculty), selected = levels(m$Faculty)),

                h3("Level"),
                checkboxGroupInput("selected_levels", label = "", choices = levels(m$Level), selected = levels(m$Level)),

                ## Quality slider
                sliderInput("quality_range", "Quality", min = 1, max = 9, value = c(1, 9)),

                ## Sustainability slider
                sliderInput("sustainability_range", "Sustainability", min = 1, max = 9, value = c(1, 9)),

                tags$p("Reconstructed by William Denton (wdenton@yorku.ca).",
                       tags$a(href="https://github.com/wdenton/aapr/", "Source and data.")
                       ),

                tags$p("Known bug: The Department list includes superfluous entries in the Administrative view.")

            ),

            ## Where the big chart goes
            mainPanel(
                ## uiOutput("ggvis_ui"),
                ggvisOutput("scatterplot")
            )

        )
    )
)
