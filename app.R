library(digest)
library(dplyr)
library(shiny)
library(shinydashboard)
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

global_pool <- dbPool(RSQLite::SQLite(), dbname = "C:\\AtelierCRM\\backup\\skiniyaCRM.db")


ui <- dashboardPage(skin = 'purple',
                    dashboardHeader(title='Atelier CRM',
                                    dropdownMenuOutput("reminder")),
                    dashboardSidebar(
                      sidebarMenu(
                        convertMenuItem(menuItem("Статистика", tabName = "stat", icon = icon("calculator"),
                                                 
                                                 selectInput('grouper', label='поле группировки', 
                                                             choices = c('клиент', 'вещь', 'дата'), 
                                                             selected = 'клиент'),
                                                 dateRangeInput(inputId="period",
                                                                label = "Period:",
                                                                start = Sys.Date() - 28,
                                                                end = Sys.Date() + 1,
                                                                min = "2018-05-01",
                                                                max = NULL,
                                                                separator = "")
                                                 
                        ), "stat"),
                        menuItem("Клиенты", tabName = "clients", icon = icon("address-book")),
                        menuItem("Мерки", tabName = "measurements", icon=icon("arrows-alt")),
                        menuItem("Заказы", tabName = "orders", icon = icon("archive")),
                        menuItem("Напоминалки", badgeLabel = "new", tabName = "reminder", icon = icon("paper-plane"))
                      )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        
                        tabItem(tabName = 'stat',
                                fluidRow(infoBoxOutput("orderBox"),
                                         infoBoxOutput("revenueBox"),
                                         infoBoxOutput("clientBox")),
                                box(title = 'выручка', width = 13, status = "warning", collapsible = TRUE, solidHeader = FALSE,
                                    plotlyOutput("revenue_plot")),
                                fluidRow(box(title = 'Заказы', width = 12, status = "info", collapsible = TRUE,
                                             DT::dataTableOutput("orders_table"))),
                                fluidRow(
                                  box(title = 'Клиенты', width = 6, status = "info", collapsible = TRUE,
                                             DT::dataTableOutput("client_table")),
                                  box(title = 'Посылки', width = 6, status = "info", collapsible = TRUE,
                                      DT::dataTableOutput("sent_table"))
                                  )
                        ),
                        
                        tabItem(tabName = 'clients',
                                fluidRow(
                                  box(title = "Поиск", width = 12, status = "primary", collapsible = TRUE, solidHeader = FALSE,
                                      fluidRow(
                                        column(12, align="center",
                                               uiOutput("customer_selector3"),
                                               actionButton("search", 'найти', icon = icon('address-book'),
                                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                        )
                                      ),
                                      br(),
                                      box(status = "primary", width = 12,
                                        DT::dataTableOutput("address"),
                                        br(),
                                        DT::dataTableOutput("client_orders")
                                      )
                                  )
                                ), 
                                fluidRow(
                                  box(title = "Новый клиент", width = 6, status = "primary", collapsible = TRUE, collapsed = TRUE, solidHeader = FALSE,
                                      textInput("name", "имя клиента:"),
                                      textInput("address", "адрес клиента:"),
                                      textInput("email", "email клиента:"),
                                      radioButtons(inputId = "update_client", "обновить клиента?", 
                                                   c("да", "нет"), inline = TRUE, selected = "нет"),
                                      br(),
                                      actionButton("create_client", 'добавить клиента', icon = icon('address-book'),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                  ),
                                  box(title='Мерки', width = 6, status = 'primary', collapsible=TRUE, collapsed = TRUE, solidHeader = FALSE,
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
                                             textInput("info", "заметки:"),
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
                                box(title = 'Новый заказ', width = 5, status = 'warning', collapsible = TRUE, solidHeader = FALSE,
                                    uiOutput("customer_selector2"),
                                    uiOutput("item_selector"),
                                    uiOutput("material_selector"),
                                    uiOutput("color_selector"),
                                    selectInput('quantity', label='количество', choices = 1:20),
                                    textInput('price', label='цена'),
                                    textInput('notes', label='заметки'),
                                    br(),
                                    actionButton("create_order", ' добавить заказ', icon = icon('cart-arrow-down'),
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                ),
                                box(title = 'обновить заказ',  width = 5, status = 'warning', collapsible = TRUE, solidHeader = FALSE,
                                    uiOutput("order"),
                                    selectInput('status', label='выбрать статус', 
                                                choices = c('готов', 'ждет оплаты', 'в производстве', 'отправлен'), 
                                                selected = 'ждет оплаты'),
                                    uiOutput("postal"),
                                    radioButtons(inputId = "delete", "удалить заказ?", 
                                                 c("да", "нет"), inline = TRUE, selected = "нет"),
                                    br(),
                                    actionButton("update_order", ' обновить заказ', icon = icon('cart-arrow-down'),
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                )
                        ),
                        
                        tabItem(tabName = 'reminder', 
                                box(title = 'Создать напоминание', width = 5, status = 'primary', solidHeader = FALSE,
                                    dateInput("started_at", label = "начать показ", value = Sys.Date()),
                                    dateInput("finished_at", label = "закончить показ", value = Sys.Date()+2),
                                    textInput('author', label='автор'),
                                    textInput('message', label='сообщение'),
                                    radioButtons(inputId = "delete_reminder", "удалить напоминание?", 
                                                 c("да", "нет"), inline = TRUE, selected = "нет"),
                                    actionButton("create_reminder", 'создать напоминание', icon = icon('"paper-plane"'),
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                ))
                      )
                    )
)


server <- function(input, output, session) {
  
  # GLOBAL VARS
  group_ptr <- list('клиент'='user_name', 'вещь'='item', 'дата'='created_at')
  client_list <- updateSelector('clients', 'name') 
  order_list <- updateSelector('orders', 'none', all=TRUE)
  rvalues <- reactiveValues(revenue=0, n_orders=0, n_clients=0)
  
  
  # UI RENDERING
  output$customer_selector1 <- renderUI({
    selectInput('client_name', label='имя клиента', choices = client_list)
  })
  
  output$customer_selector2 <- renderUI({
    selectInput('user_name', label='имя клиента', choices = client_list)
  })
  
  output$customer_selector3 <- renderUI({
    selectInput('searchable_name', label='имя клиента', choices = client_list, width='350px')
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
  
  output$postal <- renderUI({
    if (input$status == "отправлен") 
      textInput("postal_code", "почтовый код")
  })
  
  output$client_table <- DT::renderDataTable(DT::datatable({
    leftb <- as.character(input$period[1])
    rightb <- as.character(input$period[2])
    data <- global_pool %>% tbl('clients') %>% filter(created_at >= leftb & created_at <= rightb) %>%
      select(everything()) %>% arrange(desc(created_at)) %>% collect()
    rvalues$n_clients <- nrow(data)
    data
  }, rownames = FALSE, filter = "top", options = list(scroller = TRUE, scrollX = TRUE)))
  
  output$orders_table <- DT::renderDataTable(DT::datatable({
    leftb <- as.character(input$period[1])
    rightb <- as.character(input$period[2])
    data <- global_pool %>% tbl('orders') %>% filter(created_at >= leftb & created_at <= rightb) %>% 
      select(everything(), -one_of(c('order_hash', 'postal_code'))) %>% arrange(desc(created_at)) %>% collect()
    rvalues$n_orders <- nrow(data)
    revenue <- data %>% filter(status != 'ждет оплаты') %>% summarise(rev = sum(price*quantity)) %>% collect()
    rvalues$revenue <- revenue$rev
    data
  }, rownames = FALSE, filter = "top", options = list(scroller = TRUE, scrollX = TRUE)))
  
  output$sent_table <- DT::renderDataTable(DT::datatable({
    leftb <- as.character(input$period[1])
    pc <- global_pool %>% 
      tbl('orders') %>% 
      filter(postal_code != '') %>%
      select(user_name, order_hash, postal_code) %>%
      distinct()
    
    st <- global_pool %>% 
      tbl('eventlog') %>% 
      filter(event=='отправлен' & created_at >= leftb) %>% 
      select(order_hash, created_at) %>% 
      group_by(order_hash) %>% 
      summarize(created_at = max(created_at)) 
    
    df <- inner_join(pc, st, by='order_hash') %>%
      select(user_name, created_at, postal_code) %>%
      arrange(desc(created_at)) %>% collect()
    
    df
  }, rownames = FALSE, filter = "top", options = list(scroller = TRUE, scrollX = TRUE)))
  
  output$measurements_table <- DT::renderDataTable(DT::datatable({
    data <- global_pool %>% tbl('measurements') %>% select(everything()) %>% collect()
    data
  }, rownames = FALSE, filter = "top", options = list(scroller = TRUE, scrollX = TRUE)))
  
  
  output$revenue_plot <- renderPlotly({
    
    leftb <- as.character(input$period[1])
    rightb <- as.character(input$period[2])
    grp <- as.name(group_ptr[[input$grouper]])
    data <- global_pool %>% tbl('orders') %>% filter(created_at >= leftb & created_at <= rightb & status != 'ждет оплаты') %>% 
      group_by(grp) %>% summarise(revenue = sum(price*quantity)) %>% collect()
    
    if (grp == 'created_at') {
      
      data$created_at <- as.Date(as.POSIXct(data$created_at, format="%Y-%m-%d", tz = "GMT"))
      graph = plot_ly(data, x = data$created_at, y = data$revenue, type = "bar") %>%
        layout(yaxis = list(title = 'Выручка по дням', autosize = FALSE))
      graph$elementId = NULL
      
    } else {
      
      graph <- plot_ly(x = data[[grp]], y = data$revenue, type = "bar")
      
    }
    
    graph
  })
  
  output$orderBox <- renderInfoBox({
    infoBox(
      "количество заказов", rvalues$n_orders, icon = icon("list"),
      color = "blue"
    )
  })
  
  output$revenueBox <- renderInfoBox({
    infoBox(
      "выручка, $", rvalues$revenue, icon = icon("credit-card"),
      color = "purple"
    )
  })
  
  output$clientBox <- renderInfoBox({
    infoBox(
      "клиентов", rvalues$n_clients, icon = icon("address-book"),
      color = "blue"
    )
  })
  
  output$reminder <- renderMenu({
    
    today <- Sys.Date()
    messageData <- global_pool %>% tbl("reminder") %>% 
      filter(finished_at >= today & started_at <= today) %>% 
      arrange(desc(finished_at)) %>% select(author, message) %>% collect()
    print(messageData)
    if (nrow(messageData) > 0) {
      msgs <- apply(messageData, 1, function(row) {
        messageItem(from = row[["author"]], message = row[["message"]])
      })
      dropdownMenu(type = "messages", .list = msgs, badgeStatus = "warning")
    }
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
      to_hash <- paste0(input$user_name, Sys.Date())
      listed_input <- reactiveValuesToList(input)
      listed_input <- listed_input[names(listed_input) != 'order_id']
      listed_input$order_hash <- digest(to_hash, algo="md5", serialize=F)
      listed_input$event <- 'ждет оплаты'
      insertRow(global_pool, 'orders', listed_input)
      insertRow(global_pool, 'eventlog', listed_input)
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
    if (input$order_id != '') {
      if (input$delete == 'нет'){
        order_hash <- global_pool %>% tbl('orders') %>% 
          filter(order_id == input$order_id) %>% select('order_hash') %>% collect()
        listed_input <- reactiveValuesToList(input)
        listed_input$order_hash <- order_hash$order_hash
        listed_input$event <- listed_input$status
        
        if (input$status == 'отправлен') {
          if (input$postal_code != '') {
            updateRow(listed_input, global_pool, 'orders', id_field='order_hash', to_update=c('status', 'postal_code'))
            insertRow(global_pool, 'eventlog', listed_input)
            updateTextInput(session, "status", value = 'ждет оплаты')
            updateTextInput(session, "order_id", value = '')
            showModal(modalDialog(
              title = "статус заказа обновлен",
              easyClose = TRUE, footer = NULL
            ))
            
          } else {
            
            showModal(modalDialog(
              title = "необходим почтовый код",
              easyClose = TRUE, footer = NULL
            ))
            
          }
        } else {
          updateRow(listed_input, global_pool, 'orders', id_field='order_hash', to_update=c('status'))
          insertRow(global_pool, 'eventlog', listed_input)
          updateTextInput(session, "status", value = 'ждет оплаты')
          updateTextInput(session, "order_id", value = '')
          showModal(modalDialog(
            title = "статус заказа обновлен",
            easyClose = TRUE, footer = NULL
          ))
        }
        
      } else {
        updateRow(input, global_pool, 'orders', id_field='order_id', delete=TRUE)
        showModal(modalDialog(
          title = "заказ удален",
          easyClose = TRUE, footer = NULL
        ))
      }
    } else {
      showModal(modalDialog(
        title = "необходим идентификатор",
        easyClose = TRUE, footer = NULL
      ))
    }
  })
  
  observeEvent(input$create_reminder, {
    if (input$author != '' & input$message != '') {
      if (input$delete_reminder == 'нет') {
        input_ <- list()
        input_$author <- input$author
        input_$message <- input$message
        input_$started_at <- as.character(input$started_at)
        input_$finished_at <- as.character(input$finished_at)
        insertRow(global_pool, 'reminder', input_)
        showModal(modalDialog(
          title = "напоминание создано",
          easyClose = TRUE, footer = NULL
        ))
      } else {
        updateRow(input, global_pool, 'reminder', id_field='message', delete=TRUE)
        showModal(modalDialog(
          title = "напоминание удалено",
          easyClose = TRUE, footer = NULL
        ))
      }
      updateTextInput(session, "author", value = '')
      updateTextInput(session, "message", value = '')
    } else {
      showModal(modalDialog(
        title = "не все поля заполены",
        easyClose = TRUE, footer = NULL
      ))
    }
  })
  
  observeEvent(input$search, {
    if (input$searchable_name != ''){
      output$client_orders <- DT::renderDataTable(DT::datatable({
        data <- global_pool %>% tbl('orders') %>% filter(user_name == input$searchable_name) %>% 
          select(everything(), -one_of(c('order_hash'))) %>% arrange(desc(created_at)) %>% collect()
        data
      }, rownames = FALSE, options = list(scroller = TRUE, scrollX = TRUE)))
      output$address <- DT::renderDataTable(DT::datatable({
        data <- global_pool %>% tbl('clients') %>% filter(name == input$searchable_name) %>% 
          select(everything(), -one_of(c('id'))) %>% collect()
        data
      }, rownames = FALSE, options = list(scroller = TRUE, scrollX = TRUE)))
    }
  })
  
}

shinyApp(ui, server)