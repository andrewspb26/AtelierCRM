library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(pool)


source("C:\\AtelierCRM\\module\\insertRow.R", local=TRUE)
source("C:\\AtelierCRM\\module\\updateSelector.R", local=TRUE)
source("C:\\AtelierCRM\\module\\updateRow.R", local=TRUE)
source("C:\\AtelierCRM\\module\\validateUpdatingFields.R", local=TRUE)

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

                         selectInput('grouper', label='поле группировки', 
                                     choices = c('клиент', 'вещь', 'дата'), 
                                     selected = 'клиент'),
                         dateRangeInput(inputId="period",
                                        label = "Period:",
                                        start = "2018-05-01",
                                        end = NULL,
                                        min = "2018-05-01",
                                        max = NULL,
                                        separator = "")
                         
                ), "stat"),
                menuItem("Клиенты", tabName = "clients", icon = icon("address-book")),
                menuItem("Мерки", tabName = "measurements", icon=icon("arrows-alt")),
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
                           textInput("name", "имя клиента:"),
                           textInput("address", "адрес клиента:"),
                           textInput("email", "email клиента:"),
                           radioButtons(inputId = "update_client", "обновить клиента?", 
                                        c("да", "нет"), inline = TRUE, selected = "нет"),
                           br(),
                           actionButton("create_client", 'добавить клиента', icon = icon('address-book'),
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
                           radioButtons(inputId = "update_measurements", "обновить мерки?", 
                                        c("да", "нет"), inline = TRUE, selected = "нет"),
                           br(),
                           actionButton("create_measurements", ' добавить мерки', icon = icon('user-edit'),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
      ),
      
      tabItem(tabName = 'measurements',
              fluidRow(box(title='мерки клиентов', width = 12, status= 'primary',style = "background-color:  #fffae6;",
                           DT::dataTableOutput("measurements_table")))),
      
      tabItem(tabName = 'orders',
             box(title = 'Новый заказ', width = 5, status = 'warning', collapsible = TRUE, solidHeader = TRUE,
                           style = "background-color:  #fffae6;",          
                           uiOutput("customer_selector2"),
                           uiOutput("item_selector"),
                           uiOutput("material_selector"),
                           uiOutput("color_selector"),
                           textInput('quantity', label='количество'),
                           textInput('price', label='цена'),
                           textInput('notes', label='заметки'),
                           br(),
                           actionButton("create_order", ' добавить заказ', icon = icon('cart-arrow-down'),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           ),
             box(title = 'обновить заказ',  width = 5, status = 'warning', collapsible = TRUE, solidHeader = TRUE,
                 style = "background-color:  #fffae6;",          
                 uiOutput("order"),
                 selectInput('status', label='выбрать статус', 
                             choices = c('готов', 'ждет оплаты', 'в производстве', 'отправлен'), 
                             selected = 'ждет оплаты'),
                 radioButtons(inputId = "delete", "удалить заказ?", 
                              c("да", "нет"), inline = TRUE, selected = "нет"),
                 br(),
                 actionButton("update_order", ' обновить заказ', icon = icon('cart-arrow-down'),
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # GLOBAL VARS
  group_ptr <- list('клиент'='user_name', 'вещь'='item', 'дата'='created_at')
  client_list <- updateSelector('clients', 'name') 
  order_list <- updateSelector('orders', 'none', all=TRUE)
  
  
  # UI RENDERING
  output$customer_selector1 <- renderUI({
    selectInput('client_name', label='имя клиента', choices = client_list)
  })
  
  output$customer_selector2 <- renderUI({
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
  
  output$order <- renderUI({
    selectInput('order_id', label='идентификатор заказа', choices = order_list$order_id)
  })

  output$client_table <- DT::renderDataTable(DT::datatable({
    leftb <- as.character(input$period[1])
    rightb <- as.character(input$period[2])
    data <- global_pool %>% tbl('clients') %>% filter(created_at >= leftb & created_at <= rightb) %>%
      select(everything()) %>% arrange(desc(created_at)) %>% collect() 
    data
  }, rownames = FALSE, filter = "top"), options = list(scrollX = TRUE))
  
  output$orders_table <- DT::renderDataTable(DT::datatable({
    leftb <- as.character(input$period[1])
    rightb <- as.character(input$period[2])
    data <- global_pool %>% tbl('orders') %>% filter(created_at >= leftb & created_at <= rightb) %>% 
      select(everything()) %>% arrange(desc(created_at)) %>% collect()
    data
  }, rownames = FALSE, filter = "top"), options = list(scrollX = TRUE))
  
  
  output$measurements_table <- DT::renderDataTable(DT::datatable({
    data <- global_pool %>% tbl('measurements') %>% select(everything()) %>% collect()
    data
  }, rownames = FALSE, filter = "top"), options = list(scrollX = TRUE))
  
  
  output$revenue_plot <- renderPlotly({
    
    leftb <- as.character(input$period[1])
    rightb <- as.character(input$period[2])
    grp <- as.name(group_ptr[[input$grouper]])
    data <- global_pool %>% tbl('orders') %>% filter(created_at >= leftb & created_at <= rightb & status != 'ждет оплаты') %>% 
      group_by(grp) %>% summarise(revenue = sum(price*quantity)) %>% collect()

    if (grp == 'created_at') {
      
      data$created_at <- as.Date(as.POSIXct(data$created_at, format="%Y-%m-%d", tz = "GMT"))
      graph = plot_ly(data, x = data$created_at, y = data$revenue, mode = 'lines', type = "scatter") %>%
                      layout(yaxis = list(title = 'Выручка по дням', autosize = FALSE))
      graph$elementId = NULL
      
    } else {
      
      graph <- plot_ly(x = data[[grp]], y = data$revenue, type = "bar")
      
    }
    
    graph
  })
  
  
  # LOGIC HANDLING 
  observeEvent(input$create_client, {
    if(input$update_client != 'да') {
      
      insertRow(global_pool, 'clients', input)
      client_list <<- updateSelector('clients', 'name')
      updateTextInput(session, "name", value = '')     
      updateTextInput(session, "address", value = '')
      updateTextInput(session, "email", value = '')
      showModal(modalDialog(
        title = "создан новый клиент",
        easyClose = TRUE, footer = NULL
      ))
      
    } else if (input$name != '') {
      
      notEmptyFields <- validateUpdatingFields(global_pool, 'clients', input)
      updateRow(input, global_pool, 'clients', id_field='name', to_update=notEmptyFields)
      showModal(modalDialog(
        title = "данные клиента обновлены",
        easyClose = TRUE, footer = NULL
      ))
      
    } else {
      
      showModal(modalDialog(
        title = "необходимо ввести имя клиента",
        easyClose = TRUE, footer = NULL
      ))
      
    }
      
  })
  
  observeEvent(input$create_order, {
    
    if (input$price != ''){
      listed_input <- reactiveValuesToList(input)
      listed_input <- listed_input[names(listed_input) != 'order_id']
      insertRow(global_pool, 'orders', listed_input)
      order_list <<- updateSelector('orders', 'none', all=TRUE)
      updateTextInput(session, "price", value = '')
      updateTextInput(session, "quantity", value = '')
      showModal(modalDialog(
        title = "создан новый заказ",
        easyClose = TRUE, footer = NULL
      ))
      }
  })
  
  observeEvent(input$create_measurements, {
    
    if(input$update_measurements != 'да') {
      
      if (input$height != '' & input$chest_girth != ''){
        insertRow(global_pool, 'measurements', input)
        showModal(modalDialog(
          title = "мерки добавлены",
          easyClose = TRUE, footer = NULL
        ))
      }
    } else {
      notEmptyFields <- validateUpdatingFields(global_pool, 'measurements', input)
      updateRow(input, global_pool, 'measurements', id_field='client_name', to_update=notEmptyFields)
      showModal(modalDialog(
        title = "мерки обновлены",
        easyClose = TRUE, footer = NULL
      ))
    }
  })
  
  observeEvent(input$update_order, {
    
    if (input$delete == 'нет'){
      updateRow(input, global_pool, 'orders', id_field='order_id', to_update=c('status'))
      showModal(modalDialog(
        title = "статус заказа обновлен",
        easyClose = TRUE, footer = NULL
      ))
    } else {
      updateRow(input, global_pool, 'orders', id_field='order_id', delete=TRUE)
      showModal(modalDialog(
        title = "заказ удален",
        easyClose = TRUE, footer = NULL
      ))
    }
  })
  
}

shinyApp(ui, server)