library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(pool)


source("C:\\shiny_apps\\test\\dsh\\dsh\\module\\insertRow.R", local=TRUE)


global_pool <- dbPool(RSQLite::SQLite(), dbname = "C:\\sqlite\\sqlite-tools\\skiniyaCRM.db")


ui <- dashboardPage(skin = 'purple',
  dashboardHeader(title='Atelier CRM'),
  dashboardSidebar(
    sidebarMenu(
                menuItem("Клиенты", tabName = "clients", icon = icon("address-book")),
                menuItem("Заказы", tabName = "orders", icon = icon("archive"))
    )
  ),
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = 'clients',
              fluidRow(box(title = "Новый клиент", width = 12, status = "primary", collapsible = TRUE,
                column(width=4, textInput("name", "имя клиента:")),
                column(width=4, textInput("address", "адрес клиента:")),
                column(width=4, textInput("email", "email клиента:")),
                actionButton("create_client", ' добавить клиента', icon = icon('address-book'))
              )), 
              fluidRow(box(title='Мерки', width = 12, status = 'primary', collapsible=TRUE, collapsed = TRUE,
                           column(width=4, uiOutput("customer_selector1")),
                           column(width=2, textInput("height", "рост:")),
                           column(width=2, textInput("chest_girth", "обхват груди:")),
                           column(width=2, textInput("sleeve_length", "длина рукава:")),
                           column(width=2, textInput("neck_girth", "обхват шеи:")),
                           column(width=2, textInput("waist", "талия:")),
                           column(width=2, textInput("biceps_girth", "обхват бицепса:")),
                           column(width=2, textInput("head_girth", "обхват головы:")),
                           column(width=2, textInput("wrist_girth", "обхват запястья:")),
                           column(width=2, textInput("shoulder", "плечо:")),
                           column(width=2, textInput("notes", "заметки:"))
                           )),
              fluidRow(box(title = 'Клиенты', width = 12, status = "info", 
                  DT::dataTableOutput("client_table")))
      ),
      tabItem(tabName = 'orders',
              fluidRow(box(title = 'Новый заказ',  width = 12, status = 'warning', collapsible = TRUE,
                           column(width=4, uiOutput("customer_selector2")),
                           column(width=2, selectInput('item', label='вещь', choices = c('облачение', 'покровцы'))),
                           column(width=2, selectInput('material', label='материал', choices = c('шелк', 'парча'))),
                           column(width=2, selectInput('color', label='цвет', choices = c('зеленый', 'красный'))),
                           column(width=2, textInput('price', label='цена'))
                           )
              ),
              fluidRow(box(width = 4, status = 'warning', 
                  actionButton("create_order", ' добавить заказ', icon = icon('bitcoin')))),
              fluidRow(box(title = 'Заказы', width = 12, status = "info", collapsible = TRUE,
                  DT::dataTableOutput("orders_table")))
      )
    )
  )
)

server <- function(input, output) {
  
  client_list <- c(global_pool %>% tbl("clients") %>% select(name) %>% collect(),recursive = TRUE, use.names=FALSE)
  
  output$customer_selector1 <- renderUI({
    selectInput('customer1', label='имя клиента', choices = client_list)
  })
  
  output$customer_selector2 <- renderUI({
    selectInput('customer2', label='имя клиента', choices = client_list)
  })
  
  output$client_table <- DT::renderDataTable(DT::datatable({
    #insertRow(pool, 'clients', input)
    data <- mpg
    data
  }), options = list(scrollX = TRUE))
  
  output$orders_table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    data
  }), options = list(scrollX = TRUE))
    
  observeEvent(input$create_client, {
      print("here")
      insertRow(global_pool, 'clients', input)
  })
  
}

shinyApp(ui, server)