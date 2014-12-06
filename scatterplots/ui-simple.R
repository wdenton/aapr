library(shiny)
library(ggvis)
library(dplyr)

m <- read.csv("aapr.csv", stringsAsFactors = TRUE)
m <- m %>% filter(Program_Type %in% c("Academic", "Research"))

## Hardcode this for now to stop things showing up in production that shouldn't be there
faculties <- c("Education", "Env Studies", "Fine Arts", "Glendon", "Health", "LA&PS", "Lassonde", "MISC - VPAP", "MISC - VPRI", "Osgoode", "Schulich", "Science")

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

                h3("Faculty"),
                ## checkboxGroupInput("selected_faculties", label = "", choices = levels(m$Faculty), selected = levels(m$Faculty)),
                checkboxGroupInput("selected_faculties", label = "", choices = faculties, selected = faculties),
                tags$p("ORUs are mostly under MISC - VPRI, but some are in faculties."),

                h3("Level"),
                checkboxGroupInput("selected_levels", label = "", choices = levels(m$Level), selected = levels(m$Level)),

                ## Quality slider
                sliderInput("quality_range", "Quality", min = 1, max = 9, value = c(1, 9)),

                ## Sustainability slider
                sliderInput("sustainability_range", "Sustainability", min = 1, max = 9, value = c(1, 9)),

                textInput("department_word", label = "Word in Department"),

                tags$p("Reconstructed by William Denton (wdenton@yorku.ca).",
                       tags$a(href="https://github.com/wdenton/aapr/blob/master/aapr.csv", "Raw data available."),
                       "Currently showing only Academic category; Administrative coming soon.")

            ),

            ## Where the big chart goes
            mainPanel(
                ## uiOutput("ggvis_ui"),
                ggvisOutput("scatterplot")
            )

        )
    )
)
