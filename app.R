library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(pool)


source("C:\\AtelierCRM\\module\\insertRow.R", local=TRUE)
source("C:\\AtelierCRM\\module\\updateSelector.R", local=TRUE)

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

global_pool <- dbPool(RSQLite::SQLite(), dbname = "C:\\sqlite\\sqlite-tools\\skiniyaCRM.db")


ui <- dashboardPage(skin = 'purple',
  dashboardHeader(title='Atelier CRM'),
  dashboardSidebar(
    sidebarMenu(
                convertMenuItem(menuItem("Статистика", tabName = "stat", icon = icon("calculator"),
                         uiOutput("group_field"),
                         dateRangeInput(inputId="period",
                                        label = "Period:",
                                        start = "2018-05-01",
                                        end = NULL,
                                        min = "2018-05-01",
                                        max = NULL,
                                        separator = "")
                         
                ), "stat"),
                menuItem("Клиенты", tabName = "clients", icon = icon("address-book")),
                menuItem("Заказы", tabName = "orders", icon = icon("archive"))
    )
  ),
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = 'stat',
              box(title = 'выручка', width = 12, status = "warning", collapsible = TRUE, solidHeader = TRUE,
                  style = "background-color:  #fffae6;",
                  plotlyOutput("revenue_plot")),
              fluidRow(box(title = 'Заказы', width = 12, status = "info", collapsible = TRUE,
                           DT::dataTableOutput("orders_table"))),
              fluidRow(box(title = 'Клиенты', width = 12, status = "info", collapsible = TRUE,
                           DT::dataTableOutput("client_table")))
              ),
      
      tabItem(tabName = 'clients',
              fluidRow(box(title = "Новый клиент", width = 7, status = "primary", collapsible = TRUE, solidHeader = TRUE,
                           style = "background-color:  #fffae6;",
                column(width=4, textInput("name", "имя клиента:")),
                column(width=4, textInput("address", "адрес клиента:")),
                column(width=4, textInput("email", "email клиента:")),
                actionButton("create_client", ' добавить клиента', icon = icon('address-book'),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              )), 
              fluidRow(box(title='Мерки', width = 7, status = 'primary', collapsible=TRUE, collapsed = TRUE, solidHeader = TRUE,
                           style = "background-color:  #fffae6;",
                           uiOutput("customer_selector1"),
                           textInput("height", "рост:"),
                           textInput("chest_girth", "обхват груди:"),
                           textInput("sleeve_length", "длина рукава:"),
                           textInput("neck_girth", "обхват шеи:"),
                           textInput("waist", "талия:"),
                           textInput("biceps_girth", "обхват бицепса:"),
                           textInput("head_girth", "обхват головы:"),
                           textInput("wrist_girth", "обхват запястья:"),
                           textInput("shoulder", "плечо:"),
                           textInput("notes", "заметки:"),
                           actionButton("create_measurements", ' добавить мерки', icon = icon('user-edit'),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
      ),
      tabItem(tabName = 'orders',
              fluidRow(column(6, box(title = 'Новый заказ',  width = 7, status = 'warning', collapsible = TRUE, solidHeader = TRUE,
                           style = "background-color:  #fffae6;",          
                           uiOutput("customer_selector2"),
                           uiOutput("item_selector"),
                           uiOutput("material_selector"),
                           uiOutput("color_selector"),
                           textInput('price', label='цена'),
                           br(),
                           actionButton("create_order", ' добавить заказ', icon = icon('cart-arrow-down'),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           )))
      )
    )
  )
)

server <- function(input, output, session) {
  
  group_ptr <- list('клиент'='user_name', 'вещь'='item', 'дата'='created_at')
  
  client_list <- updateSelector('clients', 'name') 
  order_list <- updateSelector('orders', 'none', all=TRUE)
  
  output$customer_selector1 <- renderUI({
    selectInput('client_name', label='имя клиента', choices = client_list)
  })
  
  output$customer_selector2 <- renderUI({
    selectInput('user_name', label='имя клиента', width='400px', choices = client_list)
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
  
  output$group_field <- renderUI({
    selectInput('grouper', label='поле группировки', choices = c('клиент', 'вещь', 'дата'), selected = 'клиент')
  })

  output$client_table <- DT::renderDataTable(DT::datatable({
    data <- global_pool %>% tbl('clients') %>% select(everything()) %>% collect()
    data
  }, rownames = FALSE, filter = "top"), options = list(scrollX = TRUE))
  
  output$orders_table <- DT::renderDataTable(DT::datatable({
    data <- global_pool %>% tbl('orders') %>% select(everything()) %>% collect()
    data
  }, rownames = FALSE, filter = "top"), options = list(scrollX = TRUE))
  
  
  output$revenue_plot <- renderPlotly({
    
    leftb <- as.character(input$period[1])
    rightb <- as.character(input$period[2])
    grp <- as.name(group_ptr[[input$grouper]])
    
    data <- global_pool %>% tbl('orders') %>% filter(created_at >= leftb & created_at <= rightb) %>% group_by(grp) %>% summarise(revenue = sum(price)) %>% collect()
    
    if (grp == 'created_at') {
      
      data$created_at <- as.Date(as.POSIXct(data$created_at, format="%Y-%m-%d", tz = "GMT"))
      graph = plot_ly(data, x = data$created_at, y = data$revenue, mode = 'lines', type = "scatter") %>%
        layout(yaxis = list(title = 'Выручка по дням', autosize = FALSE))
      graph$elementId = NULL
      
    } else if (grp == 'item') {
      
      graph <- plot_ly(x = data$item, y = data$revenue, type = "bar")
      
    } else {
      
      graph <- plot_ly(x = data$user_name, y = data$revenue, type = "bar")
      
    }
    
    graph
  })
  
    
  observeEvent(input$create_client, {
      insertRow(global_pool, 'clients', input)
      client_list <<- updateSelector('clients', 'name')
      updateTextInput(session, "name", value = '')     
      updateTextInput(session, "address", value = '')
      updateTextInput(session, "email", value = '') 
  })
  
  observeEvent(input$create_order, {
    if (input$price != ''){
      insertRow(global_pool, 'orders', input)
      order_list <<- updateSelector('orders', 'none', all=TRUE)
      updateTextInput(session, "price", value = '')
      }
  })
  
  observeEvent(input$create_measurements, {
    if (input$height != '' & input$chest_girth != ''){
      insertRow(global_pool, 'measurements', input)
    }
  })
  
}

shinyApp(ui, server)