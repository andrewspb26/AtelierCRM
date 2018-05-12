library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
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
                         uiOutput("customer_stat"),
                         uiOutput("item_stat"),
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
  
  output$customer_stat <- renderUI({
    selectInput('customer', label='клиент', choices = client_list, selected = '')
  })
  
  output$item_stat <- renderUI({
    selectInput('itemz', label='вещь', choices = order_list$item, selected = '')
  })
  
  
  #
  output$client_table <- DT::renderDataTable(DT::datatable({
    data <- global_pool %>% table('clients') %>% select(everything()) %>% collect()
    data
  }), options = list(scrollX = TRUE))
  
  output$orders_table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    data
  }), options = list(scrollX = TRUE))
    
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