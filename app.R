
library("shiny")
library("shinyWidgets")
library("DT")

#install libraries for database update
library("RMariaDB")
library("DBI")
library("shinyalert")

#1. SHINY UI FUNCTION ----

shinyUI <- fluidPage(
  
  setBackgroundColor(
    color = "ghostwhite"
  ),
  tabsetPanel(id = "inTabset",
    tabPanel(title = "Log In", value = "panel1",
       #1a select pub & login ----
       tags$h2("Login"),
       uiOutput(outputId = "SelectedPub"),

       textInput(inputId = "PsWd", label = "Password"),
       
       actionButton(inputId = "login", label = "login"),
       div(style="margin-bottom:10px"),

       textOutput(outputId = "passCheck")
    )
    ,

    tabPanel(title = "View Orders", value = "panel2",
      sidebarLayout(
        sidebarPanel(

          #1b display summary of orders available to drill into ----
          tags$h2("Open Orders List"),
          helpText("To view order detail, click on an Order Number in the table below"),
          dataTableOutput("DTtable"),
          div(style="margin-bottom:10px"),
          
          #button to refresh order list

          actionButton(inputId = 'refreshOrders', label = 'Refresh Orders'),
          div(style="margin-bottom:10px"),
          actionButton(inputId = 'viewByTable', label = "Switch View to 'by Table'")

        ),
        
        mainPanel(
          titlePanel("Selected Order Detail"),

          #1c Display Drilldown Data - i.e. details of selected order ----
          textOutput("Detail of Order Selected"),
          dataTableOutput("drilldown"),
          div(style="margin-bottom:10px"),

          #1d insert button to close the order and  --------

          div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton(inputId = 'delivered', label = 'Mark Order Delivered')),
          div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton(inputId = 'CollectComp', label = 'Close Order (at Payment)')),
          
          div(style="display: clear"),

          div(style="margin-bottom:10px"),


          helpText("The list of orders to the left will refresh each time an existing order is closed.  To refresh the list of orders without closing an order, click 'Refresh Orders' button.")

        )
      )
    )
    ,

    tabPanel(title = "View by Table", value = "panel3",
             sidebarLayout(
               sidebarPanel(

                 uiOutput(outputId = "SelectedTable"),
                 div(style="margin-bottom:10px"),

                 actionButton(inputId = 'tableDetail', label = 'View Orders for Selected Table')
               ),
               mainPanel(

                 titlePanel("Selected Order Detail"),

                 #1c Display Drilldown Data - i.e. details of selected order ----
                 textOutput("Detail of Orders for Selected Table"),
                 dataTableOutput("DTviewByTable"),
                 div(style="margin-bottom:10px"),
 
                  #1d insert button to close the order ----
 
                  div(style="display: inline-block;vertical-align:top; width: 200px;",actionButton(inputId = 'CloseTableTab', label = 'Close Table Tab (at Payment)')),
                  div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton(inputId = 'refresh', label = 'Refresh View')),
 
 
                  div(style="display: clear"),
 
                  #1e insert button to refresh order list ----
                  helpText("The list of orders will refresh each time an existing order is closed.  To refresh the list of orders without closing an order, click 'Refresh View'")
 
               )
             )
          )
  )
)

shinyServer <- function(input, output, session) {

  #0. Set required inputs for connect
  options(mysql = list(
    "host" = "database-2.c7cch80rsap5.eu-west-2.rds.amazonaws.com",
    "port" = 3306,
    "user" = Sys.getenv("MY_UID"),
    "password" = Sys.getenv("MY_PWD")
  ))
  
  #1. Log in and set the pub name ----
  
  hideTab(inputId = "inTabset", target = "panel2")
  hideTab(inputId = "inTabset", target = "panel3")
  values <- reactiveValues()

  #set working drive to be working drive for app
  #setwd("/home/rstudio/ShinyApps/Order")
  
  pubList <- as.list(read.csv(file = "test_centre_list.csv", header = T))
  
  output$SelectedPub <- renderUI({selectInput(inputId = 'TestCentre',
                                                     label = 'Pub',
                                                     choices = pubList)})
  
  
  observeEvent(input$login, {

    if(input$login > 3) {Sys.sleep(10*input$login - 30)}
    output$passCheck <- renderText({
      validate(need(input$PsWd == Sys.getenv(paste0(input$TestCentre, "PsWd")), message = "Error: incorrect password"))
      ""
    })
    
    validate(need(input$PsWd == Sys.getenv(paste0(input$TestCentre, "PsWd")), message = "Error: incorrect password"))
    
    showTab(inputId = "inTabset", target = "panel2")
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
    
    hideTab(inputId = "inTabset", target = "panel1")
    
    values$num_orders <- 0
    values$tbl_pub <- paste0(input$TestCentre, "Orders")
    values$viewByTable <- FALSE
  
    #2. Define load data reactive function
    
    #reactive function to establish connection to database
    conn <- reactive({
      db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = db
      )
    })
    
    #reactive function to load function from database
    loadDat <- reactive({
      cn <- conn()
      query1 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus != 'Closed'", tbl = SQL(values$tbl_pub))
      dat <- dbGetQuery(cn, query1)
      dbDisconnect(cn)
      out <- dat
    })
    
    
    values$open_orders_from_sql <- loadDat()
    
    #3. Load data from sql
    
    #3a. Load on startup or if order list is empty, every 15 seconds
    
    # Anything that calls autoInvalidate will automatically invalidate
    # every 10 seconds.
    autoInvalidate <- reactiveTimer(10000)
    
    observeEvent(
      # Invalidate and re-execute this reactive expression every time the
      # timer fires.
      autoInvalidate(), {
        if(is.null(dim(values$open_orders_from_sql))) {
          values$open_orders_from_sql <- loadDat()
        }
        values$num_orders <- dim(values$open_orders_from_sql)[[1]]
      })
    
    validate(need(dim(values$open_orders_from_sql)[[1]] > 0, message = "No Orders in App - Try Refreshing the Data"))
    
    #4. Create and Update the data table to select from
    
    values$toDisplay <- values$open_orders_from_sql

    #values$summary_orders <- group_by(values$toDisplay, OrderNumber) %>%
    #  summarise(Count = n())

    if(is.null(dim(values$toDisplay)[[1]]) | dim(values$toDisplay)[[1]] == 0) 
    {values$summary_orders <- data.frame(OrderNumber = 0, Count = 0)} else {
      values$summary_orders <- as.data.frame(table(values$toDisplay$OrderNumber))
      names(values$summary_orders) <- c("OrderNumber", "Count")
      values$summary_orders$OrderNumber <- as.numeric(as.character(values$summary_orders$OrderNumber))  
    }
    
    output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
      values$summary_orders
    }))
    #use proxy as a reactive value to trigger reload of data table on update of reactive elements  
    proxy = dataTableProxy('DTtable')
    observe({
      reloadData(proxy, values$summary_orders)
    })
      
    #5. Create the detailed order table
    
    # subset the records to the row that was clicked
    drilldata <- reactive({
      shiny::validate(
        need(length(input$DTtable_rows_selected) > 0, "")
      )
      
      # subset the summary table and extract the column to subset on
      # if you have more than one column, consider a merge instead
      # NOTE: the selected row indices will be character type so they
      #   must be converted to numeric or integer before subsetting
      
      values$selected <- values$summary_orders[as.integer(input$DTtable_rows_selected), ]$OrderNumber
      values$toDisplay[values$toDisplay$OrderNumber %in% values$selected, ]
      
    })
    
    output$drilldown <- DT::renderDataTable(drilldata(), rownames = FALSE, options = list(paging = FALSE, searching = FALSE, bInfo = FALSE))
    
    
    #8. Mark rows from database for order delivered to customer and write back completed records
    
    observeEvent(input$delivered, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Viewing by Table' Mode so cannot mark individual orders."))
      
      db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = db
      )
      
      query2 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Delivered' WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
      close_order_from_sql <- dbGetQuery(cn, query2)
      query3 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus != 'Closed'", tbl = SQL(values$tbl_pub))
      values$open_orders_from_sql <- dbGetQuery(cn, query3)
      
      dbDisconnect(cn)
      
      values$toDisplay <- values$open_orders_from_sql
      #values$summary_orders <- group_by(values$toDisplay, OrderNumber) %>%
      #  summarise(Count = n())
      
      if(is.null(dim(values$toDisplay)[[1]]) | dim(values$toDisplay)[[1]] == 0) 
      {values$summary_orders <- data.frame(OrderNumber = 0, Count = 0)} else {
        values$summary_orders <- as.data.frame(table(values$toDisplay$OrderNumber))
        names(values$summary_orders) <- c("OrderNumber", "Count")
        values$summary_orders$OrderNumber <- as.numeric(as.character(values$summary_orders$OrderNumber))  
      }
      
      output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
        values$summary_orders
      }))
    })
    
    #7. Delete rows from database for order completed and write back completed records
    
    observeEvent(input$CollectComp, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = db
      )
      
      query2 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Closed' WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
      close_order_from_sql <- dbGetQuery(cn, query2)
      query3 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus != 'Closed'", tbl = SQL(values$tbl_pub))
      values$open_orders_from_sql <- dbGetQuery(cn, query3)
      
      dbDisconnect(cn)
      
      values$toDisplay <- values$open_orders_from_sql
      #values$summary_orders <- group_by(values$toDisplay, OrderNumber) %>%
      #  summarise(Count = n())
      
      if(is.null(dim(values$toDisplay)[[1]]) | dim(values$toDisplay)[[1]] == 0) 
      {values$summary_orders <- data.frame(OrderNumber = 0, Count = 0)} else {
        values$summary_orders <- as.data.frame(table(values$toDisplay$OrderNumber))
        names(values$summary_orders) <- c("OrderNumber", "Count")
        values$summary_orders$OrderNumber <- as.numeric(as.character(values$summary_orders$OrderNumber))  
      }
      
      output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
        values$summary_orders
      }))
      
    })
    
    
    #7. Refresh Orders from database
    
    observeEvent(input$refreshOrders, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = db
      )
      
      query3 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus != 'Closed'", tbl = SQL(values$tbl_pub))
      values$open_orders_from_sql <- dbGetQuery(cn, query3)
      
      dbDisconnect(cn)
      
      values$toDisplay <- values$open_orders_from_sql
      #values$summary_orders <- group_by(values$toDisplay, OrderNumber) %>%
      #  summarise(Count = n())
      
      if(is.null(dim(values$toDisplay)[[1]]) | dim(values$toDisplay)[[1]] == 0) 
      {values$summary_orders <- data.frame(OrderNumber = 0, Count = 0)} else {
        values$summary_orders <- as.data.frame(table(values$toDisplay$OrderNumber))
        names(values$summary_orders) <- c("OrderNumber", "Count")
        values$summary_orders$OrderNumber <- as.numeric(as.character(values$summary_orders$OrderNumber))  
      }
      
      output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
        values$summary_orders
      }))
      
    })
    
    
    
    #7. Switch to table view
      
    tableList <- unique(values$open_orders_from_sql$TableNumber)

    


    observeEvent(input$viewByTable, {

      showTab(inputId = "inTabset", target = "panel3")
      updateTabsetPanel(session, "inTabset",
                        selected = "panel3")

      hideTab(inputId = "inTabset", target = "panel2")
      
      output$SelectedTable <- renderUI({selectInput(inputId = 'SelectedTab',
                                                    label = 'Select Table Number and click button to refresh view',
                                                    choices = as.list(tableList))})
    })

    observeEvent(input$tableDetail, {
      
      db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = db
      )
      
      query3 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus != 'Closed'", tbl = SQL(values$tbl_pub))
      values$open_orders_from_sql <- dbGetQuery(cn, query3)
      
      dbDisconnect(cn)
      
      if(input$SelectedTab %in% unique(as.numeric(values$open_orders_from_sql$TableNumber))) {
        
        #4a. Set options for reporting by order number or table number
        
        values$viewByTable <- TRUE
        values$toDisplay <- values$open_orders_from_sql[values$open_orders_from_sql$TableNumber == as.numeric(input$SelectedTab) & values$open_orders_from_sql$Item != "Total",]
        rowNums <- dim(values$toDisplay)[[1]]
        
        values$toDisplay[rowNums + 1, ] <- values$toDisplay[rowNums, ]
        values$toDisplay$Item[rowNums + 1] <- "Total"
        values$toDisplay$Number[rowNums + 1] <- sum(values$toDisplay$Number[1:rowNums])
        values$toDisplay$Price[rowNums + 1] <- sum(values$toDisplay$Price[1:rowNums]*values$toDisplay$Number[1:rowNums])
        
        output$DTviewByTable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
          values$toDisplay
        }))
      } else {
        output$DTviewByTable <- NULL
      }
      
    })
    
    #7. Delete rows from database for order completed and write back completed records
    
    observeEvent(input$CloseTableTab, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = db
      )
      
      query4 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Closed' WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
      close_table_from_sql <- dbGetQuery(cn, query4)
      query5 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus != 'Closed'", tbl = SQL(values$tbl_pub))
      values$open_orders_from_sql <- dbGetQuery(cn, query5)
      
      dbDisconnect(cn)
      
      showTab(inputId = "inTabset", target = "panel2")
      updateTabsetPanel(session, "inTabset",
                        selected = "panel2")
      
      hideTab(inputId = "inTabset", target = "panel3")
      
      values$toDisplay <- values$open_orders_from_sql
      #values$summary_orders <- group_by(values$toDisplay, OrderNumber) %>%
      #  summarise(Count = n())
      
      if(is.null(dim(values$toDisplay)[[1]]) | dim(values$toDisplay)[[1]] == 0) 
      {values$summary_orders <- data.frame(OrderNumber = 0, Count = 0)} else {
        values$summary_orders <- as.data.frame(table(values$toDisplay$OrderNumber))
        names(values$summary_orders) <- c("OrderNumber", "Count")
        values$summary_orders$OrderNumber <- as.numeric(as.character(values$summary_orders$OrderNumber))  
      }
      
      output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
        values$summary_orders
      }))
      
    })
    
    
    #7. Refresh Orders from database
    
    observeEvent(input$refresh, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = db
      )
      
      query6 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus != 'Closed'", tbl = SQL(values$tbl_pub))
      values$open_orders_from_sql <- dbGetQuery(cn, query6)
      
      dbDisconnect(cn)
      
      showTab(inputId = "inTabset", target = "panel2")
      updateTabsetPanel(session, "inTabset",
                        selected = "panel2")
      
      hideTab(inputId = "inTabset", target = "panel3")
      
      values$toDisplay <- values$open_orders_from_sql
      #values$summary_orders <- group_by(values$toDisplay, OrderNumber) %>%
      #  summarise(Count = n())
      
      if(is.null(dim(values$toDisplay)[[1]]) | dim(values$toDisplay)[[1]] == 0) 
      {values$summary_orders <- data.frame(OrderNumber = 0, Count = 0)} else {
        values$summary_orders <- as.data.frame(table(values$toDisplay$OrderNumber))
        names(values$summary_orders) <- c("OrderNumber", "Count")
        values$summary_orders$OrderNumber <- as.numeric(as.character(values$summary_orders$OrderNumber))  
      }
      
      output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
        values$summary_orders
      }))
      
    })






    
    
    
    
    
    
    
    # observeEvent(input$DTtable_rows_selected, {
    #   
    #   shinyalert(
    #     text = "Mark Selected Item as Delivered?",
    #     showCancelButton = TRUE,
    #     callbackR = function(x) { if(x != FALSE) {
    #       values$df = values$df[-c(input$OrderTable_rows_selected, dim(values$df)[[1]]),]
    #       values$df[dim(values$df)[[1]] + 1,] <- c("Total", sum(as.numeric(values$df[,2])), sum(as.numeric(values$df[,2])*as.numeric(values$df[,3])), values$TestCentre, input$TableNumber, values$OrderNum, "", "Open")
    #     }
    #     }, 
    #     inputId = "shinyalert"
    #   )
    # })
    



    
    
  })
} 

shinyApp(ui = shinyUI, server = shinyServer)


