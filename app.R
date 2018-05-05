library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(pool)


source("C:\\AtelierCRM\\module\\insertRow.R", local=TRUE)
source("C:\\AtelierCRM\\module\\updateSelector.R", local=TRUE)

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
                           fluidRow(
                             column(width=4, uiOutput("customer_selector1")),
                             column(width=2, textInput("height", "рост:")),
                             column(width=2, textInput("chest_girth", "обхват груди:")),
                             column(width=2, textInput("sleeve_length", "длина рукава:")),
                             column(width=2, textInput("neck_girth", "обхват шеи:"))),
                           fluidRow(column(width=2, textInput("waist", "талия:")),
                             column(width=2, textInput("biceps_girth", "обхват бицепса:")),
                             column(width=2, textInput("head_girth", "обхват головы:")),
                             column(width=2, textInput("wrist_girth", "обхват запястья:")),
                             column(width=2, textInput("shoulder", "плечо:")),
                             column(width=2, textInput("notes", "заметки:")))
                           )),
              fluidRow(box(width = 4, status = 'primary', 
                           actionButton("create_measurements", ' добавить мерки', icon = icon('user-edit')))),
              fluidRow(box(title = 'Клиенты', width = 12, status = "info", 
                  DT::dataTableOutput("client_table")))
      ),
      tabItem(tabName = 'orders',
              fluidRow(box(title = 'Новый заказ',  width = 12, status = 'warning', collapsible = TRUE,
                           column(width=4, uiOutput("customer_selector2")),
                           column(width=2, uiOutput("item_selector")),
                           column(width=2, uiOutput("material_selector")),
                           column(width=2, uiOutput("color_selector")),
                           column(width=2, textInput('price', label='цена'))
                           )
              ),
              fluidRow(box(width = 4, status = 'warning', 
                  actionButton("create_order", ' добавить заказ', icon = icon('cart-arrow-down')))),
              fluidRow(box(title = 'Заказы', width = 12, status = "info", collapsible = TRUE,
                  DT::dataTableOutput("orders_table")))
      )
    )
  )
)

server <- function(input, output) {
  
  client_list <- updateSelector('clients', 'name') 
  order_list <- updateSelector('orders', 'none', all=TRUE)
  
  output$customer_selector1 <- renderUI({ # need to change name according to table
    selectInput('client_name', label='имя клиента', choices = client_list)
  })
  
  output$customer_selector2 <- renderUI({ # need to change name according to table
    selectInput('user_name', label='имя клиента', choices = client_list)
  })
  
  output$item_selector <- renderUI({
    selectizeInput('item', label='вещь', choices = c('', order_list$item), options = list(create = TRUE))
  })
  
  output$material_selector <- renderUI({
    selectizeInput('material', label='материал', choices = c('', order_list$material), options = list(create = TRUE))
  })
  
  output$color_selector <- renderUI({
    selectizeInput('color', label='цвет', choices = c('', order_list$color), options = list(create = TRUE))
  })
  
  output$client_table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    data
  }), options = list(scrollX = TRUE))
  
  output$orders_table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    data
  }), options = list(scrollX = TRUE))
    
  observeEvent(input$create_client, {
      insertRow(global_pool, 'clients', input)
      client_list <<- updateSelector('clients', 'name')
  })
  
  observeEvent(input$create_order, {
    insertRow(global_pool, 'orders', input)
    order_list <<- updateSelector('orders', 'none', all=TRUE)
  })
  
  observeEvent(input$create_measurements, {
    if (input$height != '' & input$chest_girth != ''){
      insertRow(global_pool, 'measurements', input)
    }
  })
  
}

shinyApp(ui, server)