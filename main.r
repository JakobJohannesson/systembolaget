library(shinydashboard)
library(shiny)
library(shinycssloaders)
library(dplyr)
library(DT)
library(borsdata)
library(ggplot2)
library(stringr)
library(shinymanager)
library(httr)
library(XML)
library(methods)



#### Inloggning ####
credentials <- data.frame(
  user = c("bam"), # mandatory
  password = c("borsgruppen"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA),
  admin = c(FALSE),
  comment = "Inlåst dashboard för börsdata api",
  stringsAsFactors = FALSE
)

#### Sidebar ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Aktier", icon = icon("th"), tabName = "utforskare", badgeLabel = "new",
             badgeColor = "green"
    ),
    menuItem("Utvecklat av", icon = icon("file-code-o"),
             href = "https://www.jakobj.se"
    )
  )
)

#### Body ####

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            fluidRow(
              column(12,
                     h2("Välkommen till Börsboard, en dashboard med Börsdatas API"))
            ),
            fluidRow(column(12,
                            withSpinner(dataTableOutput(("insid")))
            )
            )
    ),
    tabItem("utforskare",
            )
))


header <- dashboardHeader(
  title = "Systembolaget"
)

ui <- dashboardPage(header, sidebar, body)
#ui <- secure_app(ui, enable_admin = FALSE)

#### Server ####
server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  skr<-GET(url = "https://www.systembolaget.se/api/assortment/products/xml", type="basic")
  hej<-content(skr, "text")
  xmldataframe <- xmlToDataFrame(hej)
  
  df<-xmldataframe[-c(1,2),]
  
  df<- df %>% select(Namn,Namn2,Prisinklmoms,PrisPerLiter,Volymiml,Varugrupp,Typ,
                     Stil,Forpackning, Forslutning, Ursprung, Ursprunglandnamn,
                     Producent, Leverantor, Argang, Alkoholhalt, SortimentText,
                     Ekologisk, Pant)
  
  
  
  df$Prisinklmoms<-as.numeric(df$Prisinklmoms)
  df$PrisPerLiter<-as.numeric(df$PrisPerLiter)
  df$Volymiml<-as.numeric(df$Volymiml)
  df$Volymiml<-df$Volymiml/1000
  
  df$Alkoholhalt<-str_replace_all(df$Alkoholhalt, pattern = "%", "")
  df$Alkoholhalt<-as.numeric(df$Alkoholhalt)
  df$Alkoholhalt<-df$Alkoholhalt/100
  df$APK<-((df$Volymiml*1000)*df$Alkoholhalt)/df$Prisinklmoms
  
  instable<-datatable(df,rownames = FALSE, class = 'cell-border stripe',
              filter = 'bottom', options = list(
                pageLength = 5))
  
  output$insid <- renderDataTable(instable)
  
  
}

#### Kör appen ####
shinyApp(ui, server)



