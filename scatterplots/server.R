library(shiny)
library(dplyr)
library(ggvis)

m <- read.csv("aapr.csv", stringsAsFactors = TRUE)

department_tooltip <- function(p) {
    if (is.null(p)) return(NULL)
    program <- m[m$Program_Code == p$Program_Code,]
    paste0(program$Program, " (", program$Includes_Degree_Types, ")<br>",
           "Faculty: ", program$Faculty, "<br>",
           "Department: ", program$Department, "<br>",
           program$Program_Code)
}

## Define a server for the Shiny app
shinyServer(function(input, output, session) {

    output$faculties_list <- renderUI({

        if (input$category == "Academic") {
            f <- m %>% filter (Program_Type %in% c("Academic", "Research"))
            f <- droplevels(f)
            ## print("Academic")
            ## print(levels(f$Faculty))
            return(checkboxGroupInput("selected_faculties", label = "", choices = levels(f$Faculty), selected = levels(f$Faculty)))
        }

        if (input$category == "Administrative") {
            f <- m %>% filter (Program_Type == "Administrative")
            f <- droplevels(f)
            # print("Administrative")
            # print(levels(f$Department))
            return(checkboxGroupInput("selected_faculties", label = "", choices = levels(f$Department), selected = levels(f$Department)))
        }

    })

    points <- reactive({

        f <- data.frame()

        if (input$category == "Academic") {

            f <- m %>% filter(Program_Type %in% c("Academic", "Research"),
                           Faculty %in% input$selected_faculties, ## Faculty is the column to use for Academic
                           Level %in% input$selected_levels,
                           Quality >= input$quality_range[1],
                           Quality <= input$quality_range[2],
                           Sustainability >= input$sustainability_range[1],
                           Sustainability <= input$sustainability_range[2]
                           )

        } else {

            f <- m %>% filter(Program_Type == "Administrative",
                           Department %in% input$selected_faculties, ## Department is the right one for Administrative
                           Level %in% input$selected_levels,
                           Quality >= input$quality_range[1],
                           Quality <= input$quality_range[2],
                           Sustainability >= input$sustainability_range[1],
                           Sustainability <= input$sustainability_range[2]
                           )
        }

        if (! is.null(input$department_word)) {
            f <- f %>% filter(grepl(input$department_word, Department, ignore.case = TRUE))
        }

        droplevels(f)
        f

    })

    vis <- reactive({

        if (input$category == "Academic") {
            points %>% ggvis(~Quality, ~Sustainability, fill = ~Faculty, key := ~Program_Code) %>% add_tooltip(department_tooltip, on = "hover") %>% layer_points()
        } else {
            points %>% ggvis(~Quality, ~Sustainability, fill = ~Department, key := ~Program_Code) %>% add_tooltip(department_tooltip, on = "hover") %>% layer_points()
        }

    })

    vis %>% bind_shiny("scatterplot")

    ## ggplot(q, aes(x = Quality, y = Sustainability, label = Program)) +
    ##   geom_point(aes(colour= Faculty)) +
    ##   geom_text(aes(colour = Faculty)) +
    ##   scale_x_continuous(limits=input$quality_range, breaks = input$quality_range) +
    ##   scale_y_continuous(limits=input$sustainability_range, breaks = input$sustainability_range) +
    ##   ## scale_y_continuous(limits=c(1,9), breaks = 1:9) +
    ##   geom_segment((aes(x = 4.1, y = 4.5, xend = 4.1, yend = 7.5)), linetype = 2, colour = "grey") + geom_segment((aes(x = 4.1, y = 7.5, xend = 6.75, yend = 7.5)), linetype = 2, colour = "grey") + geom_segment((aes(x = 6.75, y = 7.5, xend = 6.75, yend = 4.5)), linetype = 2, colour = "grey") + geom_segment((aes(x = 6.75, y = 4.5, xend = 4.1, yend = 4.5)), linetype = 2, colour = "grey")

})
