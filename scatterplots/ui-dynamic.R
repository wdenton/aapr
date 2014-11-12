library(shiny)
library(ggvis)

m <- read.csv("aapr.csv")
m$Faculty <- as.factor(m$Faculty)
m$Department <- as.factor(m$Department)
m$Level <- as.factor(m$Level)

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

                selectInput("category", "Category", c("Academic", "Administrative")),

                uiOutput("faculties_list"),

                ## h3("Faculty"),
                ## checkboxGroupInput("selected_faculties", label = "", choices = levels(m$Faculty), selected = levels(m$Faculty)),

                h3("Level"),
                checkboxGroupInput("selected_levels", label = "", choices = levels(m$Level), selected = levels(m$Level)),

                ## Quality slider
                sliderInput("quality_range", "Quality", min = 1, max = 9, value = c(1, 9)),

                ## Sustainability slider
                sliderInput("sustainability_range", "Sustainability", min = 1, max = 9, value = c(1, 9)),

                textInput("department_word", label = "Word in Department"),

                p("Reconstructed by William Denton (wdenton@yorku.ca). Raw data available.")

            ),

            ## Where the big chart goes
            mainPanel(
                ## uiOutput("ggvis_ui"),
                ggvisOutput("scatterplot")
            )

        )
    )
)
