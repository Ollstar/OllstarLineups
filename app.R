library(shiny)
library(ggplot2)
library(stringr)
library(teamcolors)
library(shinyWidgets)
library(scales)

teamnames = read.csv("rosters.csv")

ui = shinyUI(
    fluidPage(
        tags$head(
            tags$style(
                HTML(
                    "
        .well {
            border-radius: 0px 0px 0px 0px;
            background-color: white;
        }
        "
                )
            )
        ),
        titlePanel(title=div(img(src="AMF-Logo.png"))),
        setBackgroundColor("#275"),
        sidebarLayout(
            sidebarPanel(
                selectizeInput(inputId = "contestPicker", 
                               label = 'Select Contest',
                               selected = "September 9, 2019", 
                               choices = c("select" = "", c("September 9, 2019","September 10, 2019"))),
                uiOutput("userSelect"),
                uiOutput("multiSelect"),
                checkboxInput(inputId= "multiUsers", "Multiple Users", TRUE)
            ),

            mainPanel(
                plotOutput("distPlot")
            )
        )
    )

)

# Define server logic required to draw a histogram
server = shinyServer(function(input, output, session) {

    dataInput <- reactive ({
        read.csv(paste(input$contestPicker,".csv", sep = ""))
    })
    output$userSelect <- renderUI({
        usrs = sort(as.character(unique(gsub("\\s*\\([^\\)]+\\)","",dataInput()$EntryName))))
        pickerInput(
            inputId = "myPicker", 
            label = "User Names", 
            choices = usrs, 
            options = list(`actions-box` = TRUE, size = "auto",`selected-text-format` = "count > 3"), 
            multiple = input$multiUsers
        )
        
    })

    output$distPlot <- renderPlot({
        
        u = paste(input$myPicker,collapse="|")
        tble = subset(dataInput(),str_detect(dataInput()$EntryName, u))
        b = "PG|SG|SF|PF|C |UTIL|F |G "
        all = str_replace_all(tble$Lineup, b,",")
        all = all[!is.na(all)||!""]
        all = unlist(str_split(all, ","))
        all = str_trim(all)
        dat = sort(table(all), decreasing = TRUE)
        if (dat[1] == 1) { 
            str1 = "Based on the"
            str2 = "lineup by" 
        } else {
            str1 = "Based on all"
            str2 = "lineups by" 
        }
        
        dat = as.data.frame(dat)

        withteam = merge(teamnames,dat[-1,],by.x = "namePlayer",by.y ="all")
        withteam$namePlayer <- with(withteam, reorder(namePlayer,Freq))
        if(length(input$myPicker) == 2) {
            str3 = paste(input$myPicker,collapse=" and ")
        } else if (length(input$myPicker) > 4 || is.null(input$myPicker)) {
            str3 = "multiple users"
        } else str3 = paste(input$myPicker,collapse=",")
        
        integer_breaks <- function(n = 5, ...) {
            fxn <- function(x) {
                breaks <- floor(pretty(x, n, ...))
                names(breaks) <- attr(breaks, "labels")
                breaks
            }
            return(fxn)
        }
        ggplot(withteam, aes(x = namePlayer, y = Freq, fill = nameTeam, width = 0.75)) +
            theme_bw() +
            scale_y_continuous(breaks = integer_breaks()) +
            geom_col() +
            scale_fill_teams(guide = FALSE) + 
            labs(title = paste(str1,dat[1,2],str2,"\n",str3),caption = "By: Ollstar") + 
            xlab("") + ylab("Entries") + theme(legend.position = "none") + 
            theme(plot.title = element_text(hjust = 0.5,size=15,family= "sans"),
                  axis.text=element_text(size=10,family = "sans"),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank()) + coord_flip()

        
    },height = 650)
})
shinyApp(ui = ui, server = server)