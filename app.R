library(shiny)
library(bs4Dash)
library(DT) 
library(tidyr)
library(quantmod)
library(tidyquant)
library(dplyr)

shinyApp(
  ui = dashboardPage(
    title = "ARKgento",
    header = dashboardHeader(title = "ARKgento"),
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "sidebarmenu",
        sidebarHeader("Header 1"),
        menuItem(
          "Cedears",
          tabName = "cedears"
          #icon = icon("sliders")
        ),
        menuItem(
          "ARK ETF's",
          tabName = "ark"
          #icon = icon("id-card")
        ),
        menuItem(
            "Calcular Inversion",
            tabName = "calcular",
            icon = icon("calculator")
        ),
        menuItem(
          "Rendimiento pasado",
          tabName = "rendimiento",
          icon = icon("money-bill")
        )
      )
    ),
    controlbar = dashboardControlbar(),
    footer = dashboardFooter(),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "cedears",
            tags$p("La información de cedears fue extraída de la web del Banco Comafi (https://www.comafi.com.ar/2254-CEADEAR-SHARES.note.aspx)."),
            tags$p("La cotización y volumen operado se extrae en el momento de InvertirOnline."),
            tags$p(tags$b("Peso compuesto es la suma del peso del stock en todos los ETF de ARK. Es decir que si un stock tiene un peso de 3 en un ETF y 5 en otro, este valor es 8.")),
          fluidRow(
            box(title = "Listado de cedears",
                width = 12,
                dataTableOutput("cedtable"))
          )
        ),
        tabItem(
          tabName = "ark",
          fluidRow(
            column(width = 12,
                   checkboxInput("filtrarconcedear", "Mostrar solo los que tienen cedear", value = TRUE, width = NULL),
                   tags$p("Si se selecciona el checkbox, se muestra información extraída del banco Comafi y la cantidad de shares que tiene ARK, junto con el peso en el  ETF. Date indica la fecha de actualización de la información de ARK."),
                   tags$p("Si el checkbox no esta seleccionado, se muestra la info de ARK.")
                   )  
          ),
          
          box(
            title = "ARKG",
            width = 12,
            maximizable = T,
            closable = F,
            dataTableOutput("ARKGtable")
            ),
          
          box(
            title = "ARKQ",
            width = 12,
            maximizable = T,
            closable = F,
            dataTableOutput("ARKQtable")
          ),
          
          box(
            title = "ARKK",
            width = 12,
            maximizable = T,
            closable = F,
            dataTableOutput("ARKKtable")
          ),
          
          box(
            title = "ARKF",
            width = 12,
            maximizable = T,
            closable = F,
            dataTableOutput("ARKFtable")
          ),
          
          box(
            title = "ARKW",
            width = 12,
            maximizable = T,
            closable = F,
            dataTableOutput("ARKWtable")
          )
          ),
            tabItem(
                tabName = "calcular",
                  tags$p("Seleccione el monto en pesos aproximado a invertir."),
                  tags$p("PesoCompuestoRecalculado: ajusta a un 100% los valores de la columna PesoCompuesto"),
                  tags$p("CuantoPuedoGastar: Monto en peso que puedo destinar a 1 stock X, teniendo en cuenta el monto a invertir y el PesoCompuestoRecalculado."),
                  tags$p("Si se selecciona el checkbox, si no me alcanza para al menos 1 cedear de un stock, asigno el dinero necesario para cubrirlo (puedo pasarme del monto a invertir)"),
                  tags$p("CuantosPuedoComprar: cantidad de cedears que puedo adquirir"),
                  tags$p("FraccionAccionReal: que fraccion de la accion real significa la cantidad de cedears que puedo comprar (teniendo en cuenta los Ratios)"),
                tags$p(tags$b("Ignorar las siguientes: se puede elegir stocks que no queremos, por ejemplo por bajo volumen operado, porque son caros o porque no nos interesan.")),
                fluidRow(
                  bs4ValueBoxOutput("NumberAccionesOut"),
                  bs4ValueBoxOutput("GastoRealOut"),
                ),
                
                    box(title = "Condiciones",
                        width = 12,
                        fluidRow(
                            column(width = 4,
                                   numericInput("inversionSize", label = "Monto aproximado a invertir",
                                                value = 200000)
                                   ),
                            column(width = 4,
                                   selectizeInput("ignorarTickers", "Ignorar los siguientes", c(""), selected = NULL, multiple = TRUE,
                                                  options = NULL)
                                   ),
                            column(width = 4,
                                   checkboxInput("forzarEntrada", "Si no me alcanza el dinero ajusto para comprar al menos 1 cedear", value = TRUE, width = NULL))
                        )
                      ),
                    
                    box(title = "Tabla",
                        width = 12,
                        fluidRow(
                            column(
                                width = 12,
                                dataTableOutput("TablaSeleccionados")
                            )
                        )
                    )
                
            ),
        tabItem(
          tabName = "rendimiento",
          tags$p("Seleccionar la fecha. Se compara el rendimiento contra 3 índices."),
          tags$p("Al apretar el boton calcular rendimiento puede demorar hasta 1 minuto para hacer la consulta de la información"),
          fluidRow(
            box(title = "Calcular rendimiento",
                width = 4,
                dateInput("fechaRendimiento", label = "Seleccione fecha inicial",
                          value = "2020-01-01"),
                actionButton("calcularRendimiento", "Calcular Rendimiento")
                ),
            # bs4Card(
            #   title = "Rendimiento ARKgento",
            #   width = 8,
            #   collapsible = F,
            #   closable = F,
            #   maximizable = F,
            #   uiOutput("ARKgentocomp")
            # )
            bs4ValueBoxOutput("ARKgentocomp"),
          ),
          fluidRow(
            bs4Card(
              title = "Nasdaq Composite",
              width = 4,
              collapsible = F,
              closable = F,
              maximizable = F,
              uiOutput("IXICcomp")
            ),
            bs4Card(
              title = "S&P 500 Index USD",
              width = 4,
              collapsible = F,
              closable = F,
              maximizable = F,
              uiOutput("INXcomp")
            ),
            bs4Card(
              title = "DOW JONES Index USD",
              width = 4,
              collapsible = F,
              closable = F,
              maximizable = F,
              uiOutput("DJIcomp")
            )
            ),
          fluidRow(
            column(width = 12,
                   box(title = "Resultados detallados",
                       width = 12,
                       fluidRow(
                         column(
                           width = 12,
                           dataTableOutput("TablaRendimientos")
                         )
                       )
                   )
                   )
          )
          )
        )
        
      )
    
  ),
  server = function(input, output) {
    
    vals <- reactiveValues()
    vals$df <- data.frame()
    vals$prices <- data.frame()
    vals$PerformanceARKgento <- 0
    
    output$cedtable <- DT::renderDataTable(
      mergeado_peso,
      options = list(scrollX = TRUE)
    )
    
    output$ARKWtable <- DT::renderDataTable({
      if(input$filtrarconcedear) {
        DT::datatable(cedears_ARKW,
                      options = list(scrollX = TRUE))
      } else {
        DT::datatable(ARKW,
                      options = list(scrollX = TRUE))
      }
    })
    
    output$ARKGtable <- DT::renderDataTable({
        if(input$filtrarconcedear) {
            DT::datatable(cedears_ARKG,
                          options = list(scrollX = TRUE))
        } else {
            DT::datatable(ARKG,
                          options = list(scrollX = TRUE))
        }
    })
    
    output$ARKQtable <- DT::renderDataTable({
        if(input$filtrarconcedear) {
            DT::datatable(cedears_ARKQ,
                          options = list(scrollX = TRUE))
        } else {
            DT::datatable(ARKQ,
                          options = list(scrollX = TRUE))
        }
    })
    
    output$ARKFtable <- DT::renderDataTable({
        if(input$filtrarconcedear) {
            DT::datatable(cedears_ARKF,
                          options = list(scrollX = TRUE))
        } else {
            DT::datatable(ARKF,
                          options = list(scrollX = TRUE))
        }
    })
    
    output$ARKKtable <- DT::renderDataTable({
        if(input$filtrarconcedear) {
            DT::datatable(cedears_ARKK,
                          options = list(scrollX = TRUE))
        } else {
            DT::datatable(ARKK,
                          options = list(scrollX = TRUE))
        }
    })
    
    output$TablaSeleccionados <- DT::renderDataTable({
      
        #browser()
          
        df <- mergeado_peso
        df$PesoCompuestoRecalculado <- round(df$PesoCompuesto*100/sum(df$PesoCompuesto),2)
        disponible <- input$inversionSize
        df$CuantoPuedoGastar <- round(df$PesoCompuestoRecalculado*disponible/100, 1)
        

        if(input$forzarEntrada == TRUE) {
            # Hago una reformulacion. Si lo que puedo gastar es < que lo que vale, le pongo ete valor
            df$CuantoPuedoGastar <- ifelse(df$CuantoPuedoGastar < df$ÚltimoOperado, 
                                           df$ÚltimoOperado, df$CuantoPuedoGastar)
            
        }
        
        df$CuantosPuedoComprar <- df$CuantoPuedoGastar/df$ÚltimoOperado
        
        los_que_compro <- df[df$CuantosPuedoComprar >= 1, ]
        los_que_compro$CuantosPuedoComprar <- floor(los_que_compro$CuantosPuedoComprar)
        
        los_que_compro$Ratio.CEDEARs.valor.subyacente <- as.character(los_que_compro$Ratio.CEDEARs.valor.subyacente)
        
        los_que_compro_final <- separate(data = los_que_compro, col = Ratio.CEDEARs.valor.subyacente, into = c("RatioUsar", "Eliminar"), sep = "\\:")
        #los_que_compro_final <- los_que_compro_final[-c("Eliminar"),]
        los_que_compro_final$FraccionAccionReal <- round(los_que_compro_final$CuantosPuedoComprar/as.numeric(as.character(los_que_compro_final$RatioUsar)), 3)
        vals$df <- los_que_compro_final

        updateSelectizeInput(inputId = "ignorarTickers", choices = vals$df$Símbolo.BYMA)
        
        DT::datatable(los_que_compro_final,
                      options = list(scrollX = TRUE))
        
    })
    
    output$NumberAccionesOut <- renderbs4ValueBox({
      df <- vals$df[!vals$df$Símbolo.BYMA %in% input$ignorarTickers, ]
      bs4ValueBox(
        #value = tags$h1(nrow(vals$df)),
        value = tags$h1(nrow(df)),
        subtitle = tags$h4("Acciones a comprar"),
        color = "primary",
        icon = icon("shopping-cart")
      )     
    })
    
    output$GastoRealOut <- renderbs4ValueBox({
      df <- vals$df[!vals$df$Símbolo.BYMA %in% input$ignorarTickers, ]
      bs4ValueBox(
        #value = tags$h1(paste0("$",as.character(sum(vals$df$CuantosPuedoComprar*vals$df$ÚltimoOperado)))),
        value = tags$h1(paste0("$",as.character(sum(df$CuantosPuedoComprar*df$ÚltimoOperado)))),
        subtitle = tags$h4("Monto a invertir"),
        color = "warning",
        icon = icon("dollar")
      )     
    })
    
    output$ARKgentocomp <- renderbs4ValueBox({
      
      restext <- ifelse(vals$PerformanceARKgento >= 0, "success", "danger")
      bs4ValueBox(
        value = tags$h1(paste0(as.character(vals$PerformanceARKgento), "%")),
        subtitle = tags$h4("Rendimiento ARKgento Index"),
        color = restext,
        icon = icon("percent")
      )     
    })
    
    
    observeEvent(input$calcularRendimiento, {
      
      # validate(
      #   need(nrow(vals$df) != 0, "Dirijise al tab Calcular Inversion para comenzar")
      # )
      if(nrow(vals$df) == 0) {
        showNotification("Dirijise al tab Calcular Inversion para comenzar")
      } else {
      withProgress(message = 'Obteniendo información de stocks', value = 0, {
        
        n <- 2
        
        incProgress(1/n, detail = "Procesando datos")
        
        df.filtrado <- vals$df[!vals$df$Símbolo.BYMA %in% input$ignorarTickers, ]
        
        #tickers = vals$df$Ticker.en.Mercado.de.Origen  
        tickers = df.filtrado$Ticker.en.Mercado.de.Origen  
        tickers <- as.character(tickers)

      quantmod::getSymbols(tickers,
                           from = input$fechaRendimiento,
                           to = "2021-01-21")
      prices <- map(tickers,function(x) Ad(get(x)))
      prices <- reduce(prices,merge)
      colnames(prices) <- tickers
      
      df.usar <- rbind(prices[1,], prices[nrow(prices),])
      df.usar <- as.data.frame(t(df.usar))
      df.usar$Ticker <- rownames(df.usar)
      colnames(df.usar) <- c("PrecioInicial", "PrecioFinal", "Ticker")
      rownames(df.usar) <- NULL
      df.aux <- df.filtrado
      df.aux <- df.aux[c("Ticker.en.Mercado.de.Origen", "FraccionAccionReal")]
      df.usar <- df.usar[c("Ticker", "PrecioInicial", "PrecioFinal")]
      df.usar$PorcentajeVariacion <- round(as.numeric(df.usar$PrecioFinal)/as.numeric(df.usar$PrecioInicial)*100-100,2)
      df.usar <- merge(df.usar, df.aux, by.x = "Ticker", by.y = "Ticker.en.Mercado.de.Origen")
      df.usar$PrecioInicial <- round(df.usar$PrecioInicial,2)
      df.usar$PrecioFinal <- round(df.usar$PrecioFinal,2)
      df.usar$PrecioInicialPonderado <- round(df.usar$PrecioInicial*df.usar$FraccionAccionReal,2)
      df.usar$PrecioFinalPonderado <- round(df.usar$PrecioFinal*df.usar$FraccionAccionReal,2)
      
      vals$PerformanceARKgento <- round(sum(df.usar$PrecioFinalPonderado)/sum(df.usar$PrecioInicialPonderado)*100-100,2)
      
      vals$prices <- df.usar
      
      })
      }
    })
    
    observeEvent(input$fechaRendimiento, 
                 {
                   
                   getSymbols("^DJI", from=input$fechaRendimiento, to=format(Sys.Date(),"%Y-%m-%d"))
                   vals$cambioDJI <- round(as.numeric(Cl(DJI[nrow(DJI),"DJI.Close"]))/as.numeric(Cl(DJI[1,"DJI.Close"]))*100-100,2)
                   vals$lastDJI <- round(as.numeric(Cl(DJI[nrow(DJI),"DJI.Close"])))
                   
                   getSymbols("^IXIC", from=input$fechaRendimiento, to=format(Sys.Date(),"%Y-%m-%d"))
                   vals$cambioIXIC <- round(as.numeric(Cl(IXIC[nrow(IXIC),"IXIC.Close"]))/as.numeric(Cl(IXIC[1,"IXIC.Close"]))*100-100,2)
                   vals$lastIXIC <- round(as.numeric(Cl(IXIC[nrow(IXIC),"IXIC.Close"])))
                   
                   getSymbols("^GSPC", from=input$fechaRendimiento, to=format(Sys.Date(),"%Y-%m-%d"))
                   vals$cambioINX <- round(as.numeric(Cl(GSPC[nrow(GSPC),"GSPC.Close"]))/as.numeric(Cl(GSPC[1,"GSPC.Close"]))*100-100,2)
                   vals$lastINX <- round(as.numeric(Cl(GSPC[nrow(GSPC),"GSPC.Close"])))
                   
                 })
    
    output$DJIcomp <- renderUI({
      
      texto_icono <- ifelse(vals$cambioDJI >= 0, "caret-up", "caret-down")
      
      descriptionBlock(
        number = paste0(vals$cambioDJI, "%"), 
        numberColor = ifelse(vals$cambioDJI >= 0, "success", "danger"), 
        numberIcon = icon(texto_icono),
        header = vals$lastDJI, 
        text = "DOW JONES Index USD", 
        rightBorder = F,
        marginBottom = FALSE
      )
      
    })
    
    output$IXICcomp <- renderUI({
      
      texto_icono <- ifelse(vals$cambioIXIC >= 0, "caret-up", "caret-down")
      descriptionBlock(
        number = paste0(vals$cambioIXIC, "%"), 
        numberColor = ifelse(vals$cambioIXIC >= 0, "success", "danger"), 
        numberIcon = icon(texto_icono), 
        header = vals$lastIXIC, 
        text = "Nasdaq Composite", 
        rightBorder = F,
        marginBottom = FALSE
      )
      
    })
    
    
    output$INXcomp <- renderUI({
      
      texto_icono <- ifelse(vals$cambioINX >= 0, "caret-up", "caret-down")
      
      descriptionBlock(
        number = paste0(vals$cambioINX, "%"), 
        numberColor = ifelse(vals$cambioINX >= 0, "success", "danger"), 
        numberIcon = icon(texto_icono),
        header = vals$lastINX, 
        text = "S&P 500 Index USD", 
        rightBorder = F,
        marginBottom = FALSE
      )
      
    })
    
    output$TablaRendimientos <- renderDataTable({
      df <- vals$prices
      DT::datatable(df,
                    options = list(scrollX = TRUE))
    })
    
  
    
  }
)