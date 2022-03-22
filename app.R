#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

importedData <- read.csv(url("https://docs.google.com/spreadsheets/d/1mKubRkZh3xFE9iBhnohvLqDLldT194mz0M29yxWzK2k/export?format=csv"))
#importedData <- read.csv("local.csv")
num=nrow(importedData)

#write.csv(importedData,"local.csv")

tr <- function(name) return(paste("TR:", name))

ui<-dashboardPage(title="Hola",skin="red",
    dashboardHeader(title="Semestral QM230-S-2021",
         dropdownMenu(type="messages",
                      messageItem(from="Pedro","Inicio del Examen"),
                      messageItem(from="Pedro","Fin del Examen")
                      ),
         dropdownMenu(type="notifications",
                      notificationItem(text="debes iniciar tu examen"),
                      notificationItem(text="finalizar tu examen en 3 min")
                      ),
         dropdownMenu(type="tasks",
                      taskItem(value=50, text="Avance del examen",color="red"),
                      taskItem(value=20, text="Avance del examen",color="blue"),
                      taskItem(value=5, text="Avance del examen",color="green")
                      )
    ),
    dashboardSidebar(
      sidebarSearchForm("searchText","buttonSearch","buscar",icon=shiny::icon("apple")),
      sidebarMenu(id="sidebarID",
            menuItem("Primera Ventana"),
            menuItem("Segunda sub-ventana",id="chartsID",
                 menuSubItem("Sub-Ventana1"),
                 menuSubItem("Sub-Ventana2"),
                 menuSubItem("Sub-Ventana3")
                     )
                  )
      
    ),
    dashboardBody(
      fluidRow(
        # A static infoBox
        infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
        # Dynamic infoBoxes
        infoBoxOutput("progressBox"),
        infoBoxOutput("approvalBox")
         ),
         
      fluidRow(
        uiOutput("radio"),
        textOutput("txt"),
        actionButton("submit", "Submit", class = "btn-primary")
        
      ),

    )
)

server<-function(input,output){
  n=sample(1:num,1)
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(n, " - ", num), icon = icon("list"),
      color = "purple"
    )

  })
  output$txt <- renderText({
    paste("Pregunta:", importedData[n,2])
  })
  
  output$radio <- renderUI({
    opt <- c("Normal" = "norm",
             "Uniform" = "unif",
             "Log-normal" = "lnorm",
             "Exponential" = "exp"
                )
    s=sample(3:6)
    names(opt) <- importedData[n,s]
    label <-  tr("Distribution type:")
    
    radioButtons("dist", label, opt)
  })  
  
 
}

shinyApp(ui = ui, server = server)
