
library("shiny")
library("shinyWidgets")
library("shinymanager")
library("DT")

#install libraries for database update
library("RMariaDB")
library("DBI")
library("shinyalert")
source('~/OrderApp/venueinfo.R')

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

       #textInput(inputId = "PsWd", label = "Password"),
       
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
          
          #useShinyalert(rmd = FALSE),
          useShinyalert(),

          #1d insert button to close the order and  --------

          div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton(inputId = 'delivered', label = 'Mark Order Delivered')),
          div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton(inputId = 'CollectComp', label = 'Close Order (at Payment)')),
          div(style="display: inline-block;vertical-align:top; width: 35px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton(inputId = 'cancel', label = 'Cancel Order')),
          div(style="display: clear"),

          div(style="margin-bottom:10px"),


          helpText("The list of orders to the left will refresh each time an existing order is closed or maked as delivered."),
          helpText("To refresh the list of orders without closing an order, click 'Refresh Orders' button."),
          helpText("If there are no existing orders, the table will refresh automatically every 10 seconds")

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
                 
                 #useShinyalert(rmd = FALSE),
                 useShinyalert(),
 
                  #1d insert button to close the order ----
 
                  div(style="display: inline-block;vertical-align:top; width: 200px;",actionButton(inputId = 'CloseTableTab', label = 'Close Table Tab (at Payment)')),
                  div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",actionButton(inputId = 'refresh', label = 'Refresh View')),
 
 
                  div(style="display:  clear"),
 
                  #1e insert button to refresh order list ----
                  helpText("The list of orders will refresh each time an existing order is closed.  To refresh the list of orders without closing an order, click 'Refresh View'")

               )
             )
          ),
    tabPanel(title = "View Closed Orders", value = "panel4",
             sidebarLayout(
               sidebarPanel(
                 
                 #1b display summary of orders available to drill into ----
                 tags$h2("Closed Orders"),
                 dataTableOutput("summaryClosed")
                 
                 #data file
                 #fileInput(inputId = "file", label = "Upload Pricelist (csv file)"),
                 #helpText("max file size is 60MB")
               ),
               mainPanel(
                 #tabsetPanel(id = "inTabset",
                 #           tabPanel(title = "Order_Detail", value = "panel1",
                 titlePanel("Selected Closed Order Detail"),
                 helpText("This tab allows you to see today's closed orders and takings and to download orders from past dates"),
                 
                 #1c Display Drilldown Data - i.e. details of selected order ----
                 dataTableOutput("drilldownClosed"),
                 
                 # #1d insert button to close the order ----
                 # actionButton(inputId = 'CollectComp', label = 'Close Order'),
                 # div(style="margin-bottom:10px"),
                 
                 #1e insert button to refresh order list ----
                 #helpText("The list of orders to the left will refresh each time an existing order is closed.  To refresh the list of orders without closing an order, click below"),
                 actionButton(inputId = 'refreshOrdersClosed', label = 'Refresh Closed Order List'),
                 
                 tags$h2("Download Closed Orders"),
                 helpText("Select Date Range for Download of all Closed Orders"),
                 helpText("NB: if possible avoid doing downloads while customers are actively using the Customer App"),
                 dateInput(inputId = 'dateFrom', label = 'Date From'),
                 dateInput(inputId = 'dateTo', label = 'Date To'),
                 
                 downloadButton("downloadData", "Download"),
                 
                 actionButton(inputId = 'takingsSum', label = "Update Today's Takings Summary"),
                 div(style="margin-bottom:10px"),
                 tableOutput('takingsTab'),

                 actionButton(inputId = 'goToNHS', label = 'Click to fullfill NHS Data request')
               )
             )
          ),
    
    tabPanel(title = "Confirm Password for NHS Track and Trace Download", value = "panel5",
             #1a select pub & login ----
             helpText("Confirm Password to Access NHS request for info Download"),
             #tags$h2("Login"),
             textInput(inputId = "PsWdNHS", label = "Password"),
             textInput(inputId = "secureToken", label = "Security Token"),
             actionButton(inputId = "loginNHS", label = "Continue"),
             div(style="margin-bottom:10px"),
             textOutput(outputId = "passCheckNHS")
    ),
    
    tabPanel(title = "Download Information to deliver to NHS Track & Trace", value = "panel6",
             sidebarLayout(
               sidebarPanel(
                 
                 #1b display summary of orders available to drill into ----
                 #tags$h2("DownLoad Information For NHS Track & Trace Query"),
                 
               ),
               mainPanel(
                 
                 titlePanel("DownLoad Information For NHS Track & Trace Query"),
                 helpText("This tab allows you to DownLoad Personal Information from the last 21 days"),
                 helpText("This info is to be shared to NHS Track & Track, in line with GDPR policy"),
                 helpText("No other sharing or usage is permitted"),
                 
                 
                 dateInput(inputId = 'dateFromNHS', label = 'Date From'),
                 dateInput(inputId = 'dateToNHS', label = 'Date To'),
                 
                 downloadButton("downloadDataNHS", "Download"),
                 
                 actionButton(inputId = 'closeTabNHS', label = 'Return to Order View')
                 
               )
             )
    ),
    tabPanel(title = "Update Menu", value = "panel7",
             #Provide Display Name, Postcode, Password
             
             #data file
             fileInput(inputId = "uPriceListFile", label = "Upload Pricelist (csv file)", accept = ".csv"),
             
             #Submit button
             actionButton(inputId = "updateMenuButton", label = "Update Menu"),
             
             #show updated menu0
             tableOutput("uPriceList"),
             textOutput(outputId = "filepath")
    )
  )
)

# Wrap your UI with secure_app, enabled admin mode or not
ui <- secure_app(shinyUI, enable_admin = TRUE)


shinyServer <- function(input, output, session) {

  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "/home/shiny/database.sqlite",
      #passphrase = key_get("R-shinymanager-key", "obiwankenobi")
      passphrase = "bananaVacuum291?"
    )
  )
  
  #0. Set required inputs for connect ----
  options(mysql = list(
    "host" = Sys.getenv("SQL_ENDPOINT"),
    "port" = Sys.getenv("SQL_PORT"),
    "user" = Sys.getenv("MY_UID"),
    "password" = Sys.getenv("MY_PWD")
  ))
  
  #1. Log in and set the pub name ----
  
  hideTab(inputId = "inTabset", target = "panel2")
  hideTab(inputId = "inTabset", target = "panel3")
  hideTab(inputId = "inTabset", target = "panel4")
  hideTab(inputId = "inTabset", target = "panel5")
  hideTab(inputId = "inTabset", target = "panel6")
  hideTab(inputId = "inTabset", target = "panel7")
  values <- reactiveValues()

  #set working drive to be working drive for app
  #setwd("/home/rstudio/ShinyApps/Order")
  
  pubList <- as.list(rbind(read.csv(file = "test_centre_list.csv", header = T), venue))
  
  output$SelectedPub <- renderUI({selectInput(inputId = 'TestCentre',
                                                     label = 'Venue',
                                                     choices = pubList)})
  
  
  
  
  observeEvent(input$login, {

    #if(input$login > 3) {Sys.sleep(10*input$login - 30)}
    #output$passCheck <- renderText({
    #  validate(need(input$PsWd == Sys.getenv("VenuePsWd"), message = "Error: incorrect password"))
    #  ""
    #})
    
    #validate(need(input$PsWd == Sys.getenv("VenuePsWd"), message = "Error: incorrect password"))
    
    showTab(inputId = "inTabset", target = "panel2")
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
    showTab(inputId = "inTabset", target = "panel4")
    hideTab(inputId = "inTabset", target = "panel1")
    showTab(inputId = "inTabset", target = "panel7")
    
    values$num_orders <- 0
    values$tbl_pub <- paste0(input$TestCentre, "Orders")
    values$viewByTable <- FALSE
  
    #2. Define connection and load data reactive function and prep database on first use ----
    
    #function to establish connection to database
    db_to_use <- "BAR"
    conn <- function(db = db_to_use) {
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = db
                      )
      cn
    }
    
    #function to load function from database
    loadDat <- function() {
      con <- conn()
      query1 <- sqlInterpolate(con, "SELECT * FROM ?tbl WHERE OrderStatus NOT IN ('Closed', 'cancelled')", tbl = SQL(values$tbl_pub))
      dat <- dbGetQuery(con, query1)
      dbDisconnect(con)
      out <- dat
      out
    }
    
    #function to update the display tables
    fn_update_display <- function() {
      
      values$toDisplay <- values$open_orders_from_sql
      
      if(is.null(dim(values$toDisplay)[[1]]) | dim(values$toDisplay)[[1]] == 0) 
      {summary_orders <- data.frame(OrderNumber = 0, Count = 0)} else {
        summary_orders <- as.data.frame(table(values$toDisplay$OrderNumber))
        names(summary_orders) <- c("OrderNumber", "Count")
        summary_orders$OrderNumber <- as.numeric(as.character(summary_orders$OrderNumber))  
      }
      summary_orders
    }
    
    #test if database exists
    mtry <- try(dbGetQuery(conn(), sqlInterpolate(conn(), "SELECT * FROM ?tbl WHERE OrderStatus NOT IN ('Closed', 'cancelled')", tbl = SQL(values$tbl_pub))))
    if(class(mtry) == "data.frame") {dbDisconnect(conn())}
    
    #if not create it
    if (length(grep("Error", mtry)) > 0) {
      if (length(grep("Failed to connect", mtry)) > 0) {
        cn <- conn(db = "")
        query1 <- sqlInterpolate(cn, "CREATE DATABASE ?db_to_create", db_to_create = SQL(db_to_use))
        cr_db <- dbSendStatement(cn, query1)
        dbClearResult(cr_db)
        dbDisconnect(cn)
      }
      
      a <- input$TestCentre
      Records <- data.frame(
        OrderName = "", OrderEmail = "", OrderTimeIn = "", OrderIntPhone = 0, OrderPhone = 0, OrderNumber = 0, OrderQrRef = "", OrderTimeOut = ""
        , stringsAsFactors = FALSE)
      
      RecordstblName <- paste0(a, "Records")  
      #create archive orders tables by renaming live tables
      #dbGetQuery(cn, paste0("RENAME TABLE `",RecordstblName, "` TO ", paste0("`",RecordstblName, "Archive", gm_date(),"`")))
      #create new live orders table
      cn <- conn()
      DBI::dbWriteTable(cn, name = RecordstblName, value = Records, overwrite = TRUE, row.names = FALSE, field.types = c(
        OrderName = "varchar(50)", OrderEmail = "varchar(50)", OrderTimeIn = "varchar(50)", OrderIntPhone = "double", OrderPhone = "double", OrderNumber = "double", OrderQrRef = "varchar(50)", OrderTimeOut = "varchar(50)"
      ))
      dbDisconnect(cn)
      
      Orders <- data.frame(
        row_names = "", Item = "", Number = 0, Price = 0, Pub = "", TableNumber = 0, OrderNumber = 0, OrderQrRef = "", OrderStatus = "Closed"
        , stringsAsFactors = FALSE)
      OrderstblName <- paste0(a, "Orders")
      #create new live orders table
      cn <- conn()
      DBI::dbWriteTable(cn, name = OrderstblName, value = Orders, overwrite = TRUE, field.types = c(
        row_names = "varchar(50)", Item = "varchar(50)", Number = "double", Price = "double", Pub = "varchar(50)", TableNumber = "double", OrderNumber = "double", OrderQrRef = "varchar(50)", OrderStatus = "varchar(50)"
      ))
      #create closed (& cancelled) orders table
      DBI::dbWriteTable(cn, name = paste0("Closed",OrderstblName), value = Orders, overwrite = TRUE, field.types = c(
        row_names = "varchar(50)", Item = "varchar(50)", Number = "double", Price = "double", Pub = "varchar(50)", TableNumber = "double", OrderNumber = "double", OrderQrRef = "varchar(50)", OrderStatus = "varchar(50)"
      ))
      dbDisconnect(cn)
    }
    
    #Auto delete personal data more than three weeks old - do this on start up or if app is left on, once every 24hrs
    #at start up
    cn <- conn()
    query2 <- sqlInterpolate(cn, "DELETE FROM ?tbl WHERE (DATEDIFF(CAST(NOW() AS DATE), CAST(OrderTimeIn AS DATE)) > 21)", tbl = SQL(paste0(input$TestCentre, "Records")))
    rs <- dbSendStatement(cn, query2)
    dbClearResult(rs)
    dbDisconnect(cn)
    
    #if left on, then every 24 hours
    # Anything that calls autoInvalidateDaily will automatically invalidate every day.
    autoInvalidateDaily <- reactiveTimer(60*60*24*1000)
    observeEvent(
      # Invalidate and re-execute this reactive expression every time the
      # timer fires.
      autoInvalidateDaily(), {
        cn <- conn()
        query2 <- sqlInterpolate(cn, "DELETE FROM ?tbl WHERE (DATEDIFF(CAST(NOW() AS DATE), CAST(OrderTimeIn AS DATE)) > 21)", tbl = SQL(paste0(input$TestCentre, "Records")))
        rs <- dbSendStatement(cn, query2)
        dbClearResult(rs)
        dbDisconnect(cn)
      })
    
    #3. Load data from sql  ----
    
    #3a. Load on startup or if order list is empty, every 10 seconds
    
    #download open orders from database
    values$open_orders_from_sql <- loadDat()
    
    # Anything that calls autoInvalidate will automatically invalidate
    # every 10 seconds.
    autoInvalidate <- reactiveTimer(10000)
    observeEvent(
      # Invalidate and re-execute this reactive expression every time the
      # timer fires.
      autoInvalidate(), {
        values$num_orders <- dim(values$open_orders_from_sql)[[1]]
        if(values$num_orders == 0 | is.null(values$num_orders)) {
          values$open_orders_from_sql <- loadDat()
          #update data table to select from
          values$summary_orders <- fn_update_display()
          output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
            values$summary_orders
          }))
        }
      })
    
    #validate(need(dim(values$open_orders_from_sql)[[1]] > 0, message = "No Orders in App - Try Refreshing the Data"))
    
    #4. Create and Update the data table to select from  ----
    
    values$summary_orders <- fn_update_display()
    output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
      values$summary_orders
    }))
    
    #use proxy as a reactive value to trigger reload of data table on update of reactive elements  
    proxy = dataTableProxy('DTtable')
    observe({
      reloadData(proxy, values$summary_orders)
    })
      
    #5. Create the detailed order table  ----
    
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
    
    
    #6. Mark rows from database for order delivered to customer and write back completed records ----
    
    observeEvent(input$delivered, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Viewing by Table' Mode so cannot mark individual orders."))
      
      cn <- conn()
      query2 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Delivered' WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
      close_order_from_sql <- dbGetQuery(cn, query2)
      dbDisconnect(cn)
      values$open_orders_from_sql <- loadDat()
      
      values$summary_orders <- fn_update_display()
      output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
        values$summary_orders
      }))
    })
    
    #7. Delete rows from database for order completed and write back completed records ----
    
    observeEvent(input$CollectComp, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      shinyalert(
        text = "Paid by Card or Cash?",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "Card",
        cancelButtonText = "Cash",
        callbackR = function(x) { if(x != FALSE) {
          print(values$selected)
          cn <- conn()
          query2 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Card' WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
          close_order_from_sql <- dbSendStatement(cn, query2)
          dbClearResult(close_order_from_sql)
          query1 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
          values$close_order_from_sql <- dbGetQuery(cn, query1)
          query2 <- sqlInterpolate(cn, "DELETE FROM ?tbl WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
          del_close_order <- dbSendStatement(cn, query2)
          dbClearResult(del_close_order)
          dbWriteTable(cn, name = paste0("Closed", values$tbl_pub), value = values$close_order_from_sql, append = TRUE)
          dbDisconnect(cn)
          values$open_orders_from_sql <- loadDat()
          
          values$summary_orders <- fn_update_display()
          output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
            values$summary_orders
          }))
        } else {
          print(values$selected)
          cn <- conn()
          query2 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Cash' WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
          close_order_from_sql <- dbSendStatement(cn, query2)
          dbClearResult(close_order_from_sql)
          query1 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
          values$close_order_from_sql <- dbGetQuery(cn, query1)
          query2 <- sqlInterpolate(cn, "DELETE FROM ?tbl WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
          del_close_order <- dbSendStatement(cn, query2)
          dbClearResult(del_close_order)
          dbWriteTable(cn, name = paste0("Closed", values$tbl_pub), value = values$close_order_from_sql, append = TRUE)
          dbDisconnect(cn)
          values$open_orders_from_sql <- loadDat()
          
          values$summary_orders <- fn_update_display()
          output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
            values$summary_orders
          }))
        }
        }, 
        inputId = "shinyalertCollect"
      )
      

      
    })
    
    #8. Implement Cancel button action ----
    
    observeEvent(input$cancel, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      cn <- conn()
      query1 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Cancelled' WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
      cancel_order_from_sql <- dbSendStatement(cn, query1)
      dbClearResult(cancel_order_from_sql)
      query2 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
      values$close_order_from_sql <- dbGetQuery(cn, query2)
      query3 <- sqlInterpolate(cn, "DELETE FROM ?tbl WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selected))
      del_close_order <- dbSendStatement(cn, query3)
      dbClearResult(del_close_order)
      dbWriteTable(cn, name = paste0("Closed", values$tbl_pub), value = values$close_order_from_sql, append = TRUE)
      dbDisconnect(cn)
      values$open_orders_from_sql <- loadDat()
      
      values$summary_orders <- fn_update_display()
      output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
        values$summary_orders
      }))
      
    })
    
    #9. Refresh Orders from database ----
    
    observeEvent(input$refreshOrders, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      values$open_orders_from_sql <- loadDat()

      values$summary_orders <- fn_update_display()
      output$DTtable <- DT::renderDataTable(rownames = FALSE, selection = 'single', options = list(paging = FALSE, searching = FALSE, bInfo = FALSE), isolate({
        values$summary_orders
      }))
      
    })
    
    #10. Switch to table view ----
      
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
      
      values$open_orders_from_sql <- loadDat()
      
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
    
    #11. Delete rows from database for order completed and write back completed records - table view ----
    
    observeEvent(input$CloseTableTab, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      shinyalert(
        text = "Paid by Card or Cash?",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "Card",
        cancelButtonText = "Cash",
        callbackR = function(x) { if(x != FALSE) {
          cn <- conn()
          query4 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Cash' WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
          close_order_from_sql <- dbSendStatement(cn, query4)
          dbClearResult(close_order_from_sql)
          #query4 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Closed' WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
          #update_cancellations <- dbSendStatement(cn, query4)
          #dbClearResult(update_cancellations)
          query2 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
          values$tbl_close <- dbGetQuery(cn, query2)
          query3 <- sqlInterpolate(cn, "DELETE FROM ?tbl WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
          del_close_order <- dbSendStatement(cn, query3)
          dbClearResult(del_close_order)
          dbWriteTable(cn, name = paste0("Closed", values$tbl_pub), value = values$tbl_close, append = TRUE)
          
          dbDisconnect(cn)
          values$open_orders_from_sql <- loadDat()
          
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
        } else {
          cn <- conn()
          query5 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Card' WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
          close_order_from_sql <- dbSendStatement(cn, query5)
          dbClearResult(close_order_from_sql)
          #query4 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Closed' WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
          #update_cancellations <- dbSendStatement(cn, query4)
          #dbClearResult(update_cancellations)
          query2 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
          values$tbl_close <- dbGetQuery(cn, query2)
          query3 <- sqlInterpolate(cn, "DELETE FROM ?tbl WHERE TableNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(input$SelectedTab))
          del_close_order <- dbSendStatement(cn, query3)
          dbClearResult(del_close_order)
          dbWriteTable(cn, name = paste0("Closed", values$tbl_pub), value = values$tbl_close, append = TRUE)
          
          dbDisconnect(cn)
          values$open_orders_from_sql <- loadDat()
          
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
        }
        }, 
        inputId = "shinyalertCloseTable"
      )
      
      
      
    })
    
    
    #12. Refresh Orders from database - table view ----
    
    observeEvent(input$refresh, {
      
      #shiny::validate(need(input$BillType == "Order", "Error: App is in 'Billing by Table' Mode so cannot close individual orders."))
      
      values$open_orders_from_sql <- loadDat()
      
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
    
    
    #3. INITIAL SET UP UP - PULL IN DATA FROM SQL FOR OPEN ORDERS AND DISPLAY SUMMARY OF ORDERS TO DRILL DOWN ----
    if(input$refreshOrdersClosed == 0){
      #2. Bring in table of open orders from sql database
      
      options(mysql = list(
        "host" = Sys.getenv("SQL_ENDPOINT"),
        "port" = Sys.getenv("SQL_PORT"),
        "user" = Sys.getenv("MY_UID"),
        "password" = Sys.getenv("MY_PWD")
      ))
      
      values$tbl_pubClosed <- paste0("Closed", input$TestCentre, "Orders")
      
      values$db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = values$db
      )
      
      values$query <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE DATE(SUBSTRING(OrderQrRef,  1, 10)) = CURDATE()", tbl = SQL(values$tbl_pubClosed))
      values$closed_orders_from_sql <- dbGetQuery(cn, values$query)
      dbDisconnect(cn)
      values$toDisplayClosed <- values$closed_orders_from_sql
      
      if((is.null(dim(values$toDisplayClosed)[[1]]) | dim(values$toDisplayClosed)[[1]] == 0)) 
      {values$summary_ordersClosed <- data.frame(OrderNumber = 0, Count = 0)} else {
        values$summary_ordersClosed <- as.data.frame(table(values$toDisplayClosed$OrderNumber))
        names(values$summary_ordersClosed) <- c("OrderNumber", "Count")
        values$summary_ordersClosed$OrderNumber <- as.numeric(as.character(values$summary_ordersClosed$OrderNumber))  
      }
      
      # display the data that is available to be drilled down
      output$summaryClosed <- DT::renderDataTable(values$summary_ordersClosed[,-3], selection = 'single', rownames = FALSE)  
      
      values$num_ordersClosed <- dim(values$closed_orders_from_sql)[[1]]
    }
    
    
    #5. CREATE DRILL DOWN TABLE ----
    
    # subset the records to the row that was clicked
    drilldataClosed <- reactive({
      shiny::validate(
        need(length(input$summaryClosed_rows_selected) > 0, "")
      )    
      
      # subset the summary table and extract the column to subset on
      # if you have more than one column, consider a merge instead
      # NOTE: the selected row indices will be character type so they
      #   must be converted to numeric or integer before subsetting
      values$selectedOrderClosed <- values$summary_ordersClosed[as.integer(input$summaryClosed_rows_selected), ]$OrderNumber
      
      #values$closed_orders_from_sql[values$closed_orders_from_sql$OrderNumber %in% input$summary_rows_selected, ]
      values$closed_orders_from_sql[values$closed_orders_from_sql$OrderNumber %in% values$selectedOrderClosed, ]
      
    })
    
    # display the subsetted data
    output$drilldownClosed <- DT::renderDataTable(drilldataClosed(), rownames = FALSE)
    
    
    #7. build 'refresh orders list' button
    
    observeEvent(input$refreshOrdersClosed, {
      
      #removeUI('#takingsTab')
      
      values$db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = values$db
      )
      
      values$query4 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE DATE(SUBSTRING(OrderQrRef,  1, 10)) = CURDATE()", tbl = SQL(values$tbl_pubClosed))
      values$closed_orders_from_sql <- dbGetQuery(cn, values$query4)
      dbDisconnect(cn)
      values$toDisplayClosed <- values$closed_orders_from_sql
      
      # summarise table
      
      #values$summary_orders <- group_by(values$toDisplay, OrderNumber) %>%
      #  summarise(Count = n())
      
      if((is.null(dim(values$toDisplayClosed)[[1]]) | dim(values$toDisplayClosed)[[1]] == 0))
      {values$summary_ordersClosed <- data.frame(OrderNumber = 0, Count = 0)} else {
        values$summary_ordersClosed <- as.data.frame(table(values$toDisplayClosed$OrderNumber))
        names(values$summary_ordersClosed) <- c("OrderNumber", "Count")
        values$summary_ordersClosed$OrderNumber <- as.numeric(as.character(values$summary_ordersClosed$OrderNumber))  
      }
      
      # display the data that is available to be drilled down
      output$summaryClosed <- DT::renderDataTable(values$summary_ordersClosed, selection = 'single', rownames = FALSE)
      
      values$num_ordersClosed <- dim(values$closed_orders_from_sql)[[1]]

    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("ClosedOrderDownload", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        #start_char <- nchar(venue) + 1
        #end_char <- nchar(venue) + 10
        values$db <- "BAR"
        cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                        username = options()$mysql$user,
                        password = options()$mysql$password,
                        host     = options()$mysql$host,
                        port     = options()$mysql$port,
                        dbname = values$db
        )
        startDate <- as.character(input$dateFrom)
        endDate <- as.character(input$dateTo)
        values$query4 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE DATE(SUBSTRING(OrderQrRef, 1, 10)) BETWEEN ?dtFrom AND ?dtTo"
                                        , tbl = SQL(values$tbl_pubClosed), dtFrom = SQL(paste0("'",startDate,"'")), dtTo = SQL(paste0("'",endDate,"'")))
        values$closed_orders_from_sqlDown <- dbGetQuery(cn, values$query4)
        dbDisconnect(cn)
        values$toDisplayClosedDown <- values$closed_orders_from_sqlDown
        write.csv(
          values$toDisplayClosedDown
          , row.names = FALSE
          , file)
      }
    )
    
    observeEvent(input$takingsSum, {
      
      #insertUI()
      
      values$db <- "BAR"
      cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                      username = options()$mysql$user,
                      password = options()$mysql$password,
                      host     = options()$mysql$host,
                      port     = options()$mysql$port,
                      dbname = values$db
      )
      
      values$query4 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE DATE(SUBSTRING(OrderQrRef,  1, 10)) = CURDATE()", tbl = SQL(values$tbl_pubClosed))
      values$closed_orders_from_sql <- dbGetQuery(cn, values$query4)
      dbDisconnect(cn)
      values$toDisplayClosed <- values$closed_orders_from_sql
      
      if((is.null(dim(values$toDisplayClosed)[[1]]) | dim(values$toDisplayClosed)[[1]] == 0)) {} else {
        values$takings <- aggregate(Price ~ OrderStatus, data = values$toDisplayClosed[values$toDisplayClosed$Item == 'Total', ], FUN=sum)
        values$takings <- rbind(values$takings, c("Total", sum(values$takings$Price)))
        names(values$takings)[[2]] <- "Takings"
        output$takingsTab <- renderTable(values$takings)
      }
    })
    
    observeEvent(input$goToNHS, {
      
      showTab(inputId = "inTabset", target = "panel5")
      hideTab(inputId = "inTabset", target = "panel1")
      hideTab(inputId = "inTabset", target = "panel2")
      hideTab(inputId = "inTabset", target = "panel3")
      hideTab(inputId = "inTabset", target = "panel4")
      hideTab(inputId = "inTabset", target = "panel6")
      
      observeEvent(input$loginNHS, {
        
        if(input$loginNHS > 3) {Sys.sleep(10*input$loginNHS - 30)}
        output$passCheckNHS <- renderText({
          validate(need(input$PsWdNHS == Sys.getenv("VenuePsWd"), message = "Error: incorrect password or security token"))
          validate(need(input$secureToken == Sys.getenv("securityToken"), message = "Error: incorrect password or security token"))
          ""
        })
        
        validate(need(input$PsWdNHS == Sys.getenv("VenuePsWd"), message = "Error: incorrect password or security token"))
        validate(need(input$secureToken == Sys.getenv("securityToken"), message = "Error: incorrect password or security token"))
        showTab(inputId = "inTabset", target = "panel6")
        hideTab(inputId = "inTabset", target = "panel5")
        
        values$RecordstblName <- paste0(input$TestCentre, "Records")
        
        output$downloadDataNHS <- downloadHandler(
          filename = function() {
            paste("RecordsForNHSDownload", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            values$db <- "BAR"
            cn <- dbConnect(drv      = RMariaDB::MariaDB(),
                            username = options()$mysql$user,
                            password = options()$mysql$password,
                            host     = options()$mysql$host,
                            port     = options()$mysql$port,
                            dbname = values$db
            )
            startDate <- as.character(input$dateFromNHS)
            endDate <- as.character(input$dateToNHS)
            values$query5 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE DATE(SUBSTRING(OrderQrRef,  1, 10)) BETWEEN ?dtFrom AND ?dtTo"
                                            , tbl = SQL(values$RecordstblName), dtFrom = SQL(paste0("'",startDate,"'")), dtTo = SQL(paste0("'",endDate,"'")))
            values$RecordsForNHS_sqlDown <- dbGetQuery(cn, values$query5)
            dbDisconnect(cn)
            values$toWriteNHS <- values$RecordsForNHS_sqlDown
            write.csv(
              values$toWriteNHS
              , row.names = FALSE
              , file)
          }
        )
        
        observeEvent(input$closeTabNHS, {
                    session$reload()
        })        
      })
    })
  })
  
  output$filepath <- renderText({input$uPriceListFile$datapath})
  observeEvent(input$updateMenuButton, {
    system2(command="cp", args = c(input$uPriceListFile$datapath, '/home/shiny/OrderApp/price_list.csv', stdout = TRUE)
  })
  
  #output$uPriceList <- renderTable({
  #  file <- input$uPriceListFile
  #  uPriceList <- read.csv(file$datapath, header = T)
  #  write.csv(x=uPriceList, file='/home/shiny/OrderApp/price_list2.csv')
  #  uPriceList
  #})
  #output$priceList <- renderTable({
  #    file <- input$priceListfile
  #    priceList <- read.csv(file$datapath, header = T)
  #    write.csv(x=priceList, file=paste("/home/shiny/OrderApp/price_list-", gsub("_", "-", tolower(venueName)), ".csv", sep=""))
  #})
    
} 

shinyApp(ui = ui, server = shinyServer)


