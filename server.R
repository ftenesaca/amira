#--- AMIRA ver.5 rev.1: server.R -----------------------------------

# library(xtable)
# library(zoo)
library(shiny)
library(rsconnect)
library(dygraphs)
library(ggplot2)
library(forecast)
library(xts)
library(lubridate)
library(reshape2)
library(datasets)

#--- Función de Autocorrelación ---------------

qacf <- function(x, conf.level = 0.95, max.lag = NULL,min.lag = 0)
                {
                ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
                bacf <- acf(x, plot = FALSE, lag.max = max.lag)
                bacfdf <- with(bacf, data.frame(lag, acf))
                
                if  (min.lag > 0)
                {
                  bacfdf <- bacfdf[-seq(1, min.lag), ]
                }
                
                significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
                bacfdf <- cbind(bacfdf, significant)
                
                
                q <- qplot(lag,acf , data = bacfdf, geom = "bar", stat = "identity", position = "identity", ylab = "Autocorrelation",fill = factor(significant))
                
                q <- q + geom_hline(yintercept = -ciline,color = "blue", size = 0.2)
                
                q <- q + geom_hline(yintercept = ciline,color = "blue", size = 0.2)
                ?qplot
                
                q <- q + geom_hline(yintercept = 0, color = "red",size = 0.3)
                
                q <- q + scale_fill_hue(name = paste("Nivel de confianza de",(conf.level*100),"%"),
                                        breaks = 0:1,labels = c("No Lags", "Lags")) + theme(panel.background = element_rect(size = 3,
                                                                                                                            colour = "black",
                                                                                                                            fill = "white"),
                                                                                            axis.ticks = element_line(size = 2),
                                                                                            axis.title.x = element_text(size = rel(1.2),
                                                                                                                        face = "bold"),
                                                                                            axis.title.y = element_text(size = rel(1.2),
                                                                                                                        face = "bold"),
                                                                                            plot.title = element_text(size = 20,
                                                                                                                      face = "bold",
                                                                                                                      vjust = 1.5),
                                                                                            legend.position = "bottom",
                                                                                            legend.title = element_text(size=rel(1.2),
                                                                                                                        face="bold"),
                                                                                            legend.text = element_text(colour="blue",
                                                                                                                       size = 13)
                                                                                            )
                return(q)
                }


sqacf <- function(x, conf.level = 0.95, max.lag = NULL,min.lag = 0)
                  {
                    ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
                    bacf <- acf(x, plot = FALSE, lag.max = max.lag, na.action = na.pass)
                    bacfdf <- with(bacf, data.frame(lag, acf))
                    
                    if  (min.lag > 0)
                    {
                      bacfdf <- bacfdf[-seq(1, min.lag), ]
                    }
                    
                    significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
                    bacfdf <- cbind(bacfdf, significant)
                    
                    q <- qplot(lag, acf, data = bacfdf, geom = "bar", stat = "identity", position = "identity",ylab = "Autocorrelation",fill = factor(significant))
                    
                    q <- q + geom_hline(yintercept = -ciline,color = "blue", size = 0.2)
                    
                    q <- q + geom_hline(yintercept = ciline,color = "blue", size = 0.2)
                    
                    q <- q + geom_hline(yintercept = 0, color = "red",size = 0.3)
                    
                    q <- q + scale_fill_hue(name = paste("Nivel de confianza de",(conf.level*100),"%"),
                                            breaks = 0:1,labels = c("No Lags", "Lags")) + theme(panel.background = element_rect(size = 3,
                                                                                                                                colour = "black",
                                                                                                                                fill = "white"),
                                                                                                axis.ticks = element_line(size = 2),
                                                                                                axis.title.x = element_text(size = rel(1.2),
                                                                                                                            face = "bold"),
                                                                                                axis.title.y = element_text(size = rel(1.2),
                                                                                                                            face = "bold"),
                                                                                                plot.title = element_text(size = 20,
                                                                                                                          face = "bold",
                                                                                                                          vjust = 1.5),
                                                                                                legend.position = "bottom",
                                                                                                legend.title = element_text(size=rel(1.2),
                                                                                                                            face="bold"),
                                                                                                legend.text = element_text(colour="blue",
                                                                                                                           size = 13)
                                            )
                    return(q)
                  }


#--- Función de Autocorrelación Parcial ---------------

qpacf <- function (x, conf.level = 0.95, max.lag = NULL,min.lag = 0) 
                  {
                  ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
                  bacf <- pacf(x, plot = FALSE, lag.max = max.lag)
                  bacfdf <- with(bacf, data.frame(lag, acf))
                  
                  if (min.lag > 0)
                  {
                    bacfdf <- bacfdf[-seq(1, min.lag), ]
                  }
                  
                  significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
                  bacfdf <- cbind(bacfdf, significant)
                  
                  q <- qplot(lag, acf, data = bacfdf, geom = "bar",stat = "identity", position = "identity",ylab = "Autocorrelation",fill = factor(significant))
                  
                  q <- q + geom_hline(yintercept = -ciline,color = "blue", size = 0.2)
                  
                  q <- q + geom_hline(yintercept = ciline,color = "blue", size = 0.2)
                  
                  q <- q + geom_hline(yintercept = 0, color = "red",size = 0.3)
                  
                  q <- q + scale_fill_hue(name = paste("Nivel de confianza de",(conf.level*100),"%"),
                                          breaks = 0:1, labels = c("No Lags", "Lags")) + theme(panel.background = element_rect(size = 3,
                                                                                                                               colour = "black",
                                                                                                                               fill = "white"),
                                                                                               axis.ticks = element_line(size = 2),
                                                                                               axis.title.x = element_text(size = rel(1.2),
                                                                                                                           face = "bold"),
                                                                                               axis.title.y = element_text(size = rel(1.2),
                                                                                                                           face = "bold"),
                                                                                               plot.title = element_text(size = 20,
                                                                                                                         face = "bold",
                                                                                                                         vjust = 1.5),
                                                                                               legend.position = "bottom",
                                                                                               legend.title = element_text(size=rel(1.2),
                                                                                                                           face="bold"),
                                                                                               legend.text = element_text(colour="blue",
                                                                                                                          size = 13)
                                                                                              )
                  return(q)
                }

sqpacf <- function (x, conf.level = 0.95, max.lag = NULL,min.lag = 0) 
                  {
                    ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
                    bacf <- pacf(x, plot = FALSE, lag.max = max.lag, na.action = na.pass)
                    bacfdf <- with(bacf, data.frame(lag, acf))
                    
                    if (min.lag > 0)
                    {
                      bacfdf <- bacfdf[-seq(1, min.lag), ]
                    }
                    
                    significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
                    bacfdf <- cbind(bacfdf, significant)
                    
                    q <- qplot(lag, acf, data = bacfdf, geom = "bar",stat = "identity", position = "identity",ylab = "Autocorrelation",fill = factor(significant))
                    
                    q <- q + geom_hline(yintercept = -ciline,color = "blue", size = 0.2)
                    
                    q <- q + geom_hline(yintercept = ciline,color = "blue", size = 0.2)
                    
                    q <- q + geom_hline(yintercept = 0, color = "red",size = 0.3)
                    
                    q <- q + scale_fill_hue(name = paste("Nivel de confianza de",(conf.level*100),"%"),
                                            breaks = 0:1, labels = c("No Lags", "Lags")) + theme(panel.background = element_rect(size = 3,
                                                                                                                                 colour = "black",
                                                                                                                                 fill = "white"),
                                                                                                 axis.ticks = element_line(size = 2),
                                                                                                 axis.title.x = element_text(size = rel(1.2),
                                                                                                                             face = "bold"),
                                                                                                 axis.title.y = element_text(size = rel(1.2),
                                                                                                                             face = "bold"),
                                                                                                 plot.title = element_text(size = 20,
                                                                                                                           face = "bold",
                                                                                                                           vjust = 1.5),
                                                                                                 legend.position = "bottom",
                                                                                                 legend.title = element_text(size=rel(1.2),
                                                                                                                             face="bold"),
                                                                                                 legend.text = element_text(colour="blue",
                                                                                                                            size = 13)
                                            )
                    return(q)
                  }




#--- Función Principal a nivel del Servidor ---

shinyServer(function(input, output)
                    {
                    
                    datos <- reactive(
                                        {
                      
                                          # El contenido de la variable input$file1 es NULO inicialmente.
                                          # Después que el usuario selecciona y cargua un archivo de texto,
                                          # se transforma en un DATAFRAME con las columnas 'name',
                                          # 'size', 'type', and 'datapath'.
                                          # La columna 'datapath' contiene la ruta del archivo temporal con los datos
                                          # que fueron cargados.
                                          
                                          inFile <- input$file1
    
                                          if (is.null(inFile))
                                            return('Cargue el archivo de datos...')  #NULL
                                          
                                          # read.delim, Crea un DATAFRAME a partir de un archivo con formato tabla
                                          # file= Variable que almacena la ruta del archivo relativa al directorio de trabajo.
                                          # header= Indica si la estrucutra de la tabla en el archivo tiene encabezado (TRUE).
                                          # sep= Establece el caracter separador de campos.
                                          # quote= Establece el caracter de comillas.
                                          # dec= Establece el caracter para decimales.
                                          # ...
                                
                                          d <- read.delim(file   = inFile$datapath,
                                                          header = input$header,
                                                          sep    = input$sep,
                                                          quote  = input$quote,
                                                          dec    = input$dec)
                                          
                                          return(d)
                                         }
                                       )
  
                    
                    serie1 <- reactive(
                                        {
                                        
                                        inFile <- input$file1
                                          
                                        if (is.null(inFile))
                                            return('Cargue el archivo de datos...')  #NULL
                                          
                                        d <- datos()
                                        
                                        options(scipen=999)
                                        
                                        d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                        
                                        if (input$seriefr == 4)
                                              {
                                                d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                              }
                                        
                                        if (input$seriefr == 12)
                                              {
                                                d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                              }
                                        
                                        #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                                                          
                                        d.df <- data.frame(x=index(d.ts),y=melt(d.ts)$value)
                                     
                                        return(d.df)
                                        }
                                      )
  
                    
                    serie2 <- reactive(
                                        {
                                          
                                          inFile <- input$file1
                                          
                                          if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                          
                                          d <- datos()
                                          
                                          options(scipen=999)
                                          
                                          d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                          
                                          if (input$seriefr == 4)
                                                {
                                                d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                }
                                          
                                          if (input$seriefr == 12)
                                                {
                                                d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                }
                                          #d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                          
                                          
                                          return(d.ts) 
                                        }
                                      )
                    
                    
                    serie3 <- reactive(
                                        {
                                          
                                          inFile <- input$file1
                                          
                                          if (is.null(inFile))
                                            return('Cargue el archivo de datos...')  #NULL
                                          
                                          d <- log(datos())
                                          
                                          options(scipen=999)
                                          
                                          d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                          
                                          if (input$seriefr == 4)
                                                {
                                                  d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                }
                                          
                                          if (input$seriefr == 12)
                                                {
                                                  d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                }
                                          
                                          #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                          
                                          return(d.ts) 
                                        }
                                      )
                    
                    modelo01 <- reactive(
                                          {
                                          
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielar)
                                                  {
                                                  d <- log(datos())
                                                  }
                                            
                                            options(scipen=999)
                                            
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts[,1]
                                            
                                            metodo <- input$m01.metodo
                                                          
                                            m01 <- ar(x = ts.orignal, method = metodo)
                                                                                                          
                                            return(m01)
                                          }
                                        )
                    
                    modelo11 <- reactive(
                                          {
                                          
                                          inFile <- input$file1
                                          
                                          if (is.null(inFile))
                                            return('Cargue el archivo de datos...')  #NULL
                                          
                                          d <- datos()
                                          
                                          if (input$serielar)
                                                {
                                                d <- log(datos())
                                                }
                                          
                                          options(scipen=999)
                                          
                                          d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                          
                                          if (input$seriefr == 4)
                                                {
                                                  d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                }
                                          
                                          if (input$seriefr == 12)
                                                {
                                                  d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                }
                                          
                                          #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                          
                                          ts.orignal <- d.ts[,1]
                                          
                                          ordermax <- input$ar
                                          metodo <- input$m11.metodo
                                          
                                          m11 <- ar(x = ts.orignal,aic=FALSE, order.max=ordermax, method = metodo)
                                          
                                          return(m11)
                                          }
                                        )
                  
                    modelo02 <- reactive(
                                          {
                                            
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielma)
                                                  {
                                                  d <- log(datos())
                                                  }
                                            
                                            options(scipen=999)
                                            
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts[,1]
                                            
                                            metodo <- input$m02.metodo
                                            
                                            m02 <- auto.arima(x = ts.orignal,
                                                              d = 0,
                                                              D = 0,
                                                              max.p = 0,
                                                              #max.q = ,
                                                              max.P = 0,
                                                              max.Q = 0,
                                                              #max.order =,
                                                              max.d = 0,
                                                              max.D = 0, 
                                                              stationary = TRUE,
                                                              seasonal = FALSE,
                                                              ic = metodo
                                                              )
                                            
                                            return(m02)
                                          }
                                        )
                    
                    modelo12 <- reactive(
                                          {
                                            
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielma)
                                                  {
                                                  d <- log(datos())
                                                  }
                                            
                                            options(scipen=999)
                                           
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts[,1]
                                            
                                            ordermax <- input$ma
                                            metodo <- input$m12.metodo
                                            
                                            m12 <- arima(x = ts.orignal, order = c(0,0,ordermax), method = metodo)
                                            
                                            return(m12)
                                          }
                                        )
                    
                    
                    modelo03 <- reactive(
                                          {
                                            
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielarma)
                                                  {
                                                  d <- log(datos())
                                                  }
                                                  
                                            options(scipen=999)
                                            
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts[,1]
                                            
                                            metodo <- input$m03.metodo
                                            
                                            m03 <- auto.arima(x = ts.orignal,
                                                              d = 0,
                                                              D = 0,
                                                              #max.p = ,
                                                              #max.q = ,
                                                              max.P = 0,
                                                              max.Q = 0,
                                                              #max.order =,
                                                              max.d = 0,
                                                              max.D = 0, 
                                                              stationary = TRUE,
                                                              seasonal = FALSE,
                                                              ic = metodo
                                                              )
                                            
                                            return(m03)
                                          }
                                        )
                    
                    
                    modelo13 <- reactive(
                                          {
                                            
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielarma)
                                                  {
                                                  d <- log(datos())
                                                  }
                                            
                                            options(scipen=999)
                                            
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts[,1]
                                            
                                            ordermaxar <- input$ar1
                                            ordermaxma <- input$ma1
                                            
                                            metodo <- input$m13.metodo
                                            
                                            m13 <- arima(x = ts.orignal, order = c(ordermaxar,0,ordermaxma), method = metodo)
                                            
                                            return(m13)
                                          }
                                        )
                    
                    
                    modelo04 <- reactive(
                                          {
                                            
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielarima)
                                                  {
                                                  d <- log(datos())
                                                  }
                                            
                                            options(scipen=999)
                                            
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts[,1]
                                            
                                            metodo <- input$m04.metodo
                                                                                                                                    
                                            m04 <- auto.arima(x = ts.orignal,
                                                              #d = 0,
                                                              D = 0,
                                                              #max.p = ,
                                                              #max.q = ,
                                                              max.P = 0,
                                                              max.Q = 0,
                                                              #max.order =,
                                                              #max.d = 0,
                                                              max.D = 0, 
                                                              stationary = FALSE,
                                                              seasonal = TRUE,
                                                              ic = metodo
                                            )
                                            
                                            return(m04)
                                          }
                                        )
                    
                    
                    modelo14 <- reactive(
                                          {
                                            
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielarima)
                                                  {
                                                  d <- log(datos())
                                                  }
                                            
                                            options(scipen=999)
                                            
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(input$seriea,as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts[,1]
                                            
                                            ordermaxar    <- input$ar2
                                            ordermaxdiff  <- input$diff2
                                            ordermaxma    <- input$ma2
                                            
                                            metodo <- input$m14.metodo
                                            
                                            m14 <- arima(x = ts.orignal, order = c(ordermaxar,ordermaxdiff,ordermaxma), method = metodo)
                                            
                                            return(m14)
                                          }
                                        )
                    
                    
                    modelo05 <- reactive(
                                          {
                                            
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielE)
                                                  {
                                                  d <- log(datos())
                                                  }
                                            
                                            options(scipen=999)
                                            
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts
                                            
                                            ordermaxar <- input$arE
                                            ordermaxdiff  <- input$diffE
                                            ordermaxma <- input$maE
                                            
                                            metodo <- input$m05.metodo
                                            
                                            m05 <- arima(x = ts.orignal, order = c(ordermaxar,ordermaxdiff,ordermaxma), method = metodo)
                                            
                                            return(m05)
                                          }
                                        )
                    
                    
                    pmodelo05 <- reactive(
                                            {
                                            
                                            inFile <- input$file1
                                            
                                            if (is.null(inFile))
                                              return('Cargue el archivo de datos...')  #NULL
                                            
                                            d <- datos()
                                            
                                            if (input$serielE)
                                                  {
                                                  d <- log(datos())
                                                  }
                                            
                                            options(scipen=999)
                                            
                                            d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                            
                                            if (input$seriefr == 4)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            if (input$seriefr == 12)
                                                  {
                                                    d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                  }
                                            
                                            #d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                            
                                            ts.orignal <- d.ts
                                            
                                            ordermaxar <- input$arE
                                            ordermaxdiff  <- input$diffE
                                            ordermaxma <- input$maE
                                            
                                            metodo <- input$m05.metodo
                                            periodo <- input$periodo
                                            
                                            m05 <- arima(x = ts.orignal, order = c(ordermaxar,input$diffE,ordermaxma), method = metodo)
                                            
                                            p05 <- predict(m05,periodo)
                                            
                                            return(p05)
                                            }
                                        )
                    
                    fmodelo05 <- reactive(
                                            {
                                              
                                              inFile <- input$file1
                                              
                                              if (is.null(inFile))
                                                return('Cargue el archivo de datos...')  #NULL
                                              
                                              d <- datos()
                                              
                                              if (input$serielE)
                                                    {
                                                    d <- log(datos())
                                                    }
                                              
                                              options(scipen=999)
                                              
                                              d.ts <- ts(data = d,start=c(as.numeric(input$seriea),1),frequency=as.numeric(input$seriefr))
                                              
                                              if (input$seriefr == 4)
                                                    {
                                                      d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$serieq)),frequency=as.numeric(input$seriefr))
                                                    }
                                              
                                              if (input$seriefr == 12)
                                                    {
                                                      d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                                    }
                                              
                                              #d.ts <- ts(data = d,start=c(as.numeric(input$seriea),as.numeric(input$seriem)),frequency=as.numeric(input$seriefr))
                                              
                                              ts.orignal <- d.ts
                                              
                                              ordermaxar <- input$arE
                                              ordermaxdiff  <- input$diffE
                                              ordermaxma <- input$maE
                                              
                                              metodo <- input$m05.metodo
                                              periodo <- input$periodo
                                              
                                              m05 <- arima(x = ts.orignal, order = c(ordermaxar,input$diffE,ordermaxma), method = metodo)
                                              
                                              f05 <- forecast(m05,periodo)
                                              
                                              return(f05)
                                            }
                                          )
                    
                    #---------- Inicio Panel Datos ----------
                    
                    # renderTable, Crea una tabla reactiva (output slot)
                    
                    output$datos1 <- renderDataTable(
                                        
                                                      {
                                                      inFile <- input$file1
                                                        
                                                      if (is.null(inFile))
                                                        return('Cargue el archivo de datos...')  #NULL
                                                      
                                                      datos()
                                                      },
                                                      
                                                      options=list(pageLength = 15)
                                                      
                                                    )
  
                    #---------- Fin Panel Datos -------------
  
  
                    #---------- Inicio Panel Serie ----------
                    
                    output$serie  <- renderDygraph(
                                                    {
    
                                                    inFile <- input$file1
                                                      
                                                    if (is.null(inFile))
                                                        return('Cargue el archivo de datos...')  #NULL 
                                                    
                                                    p <-dygraph(serie2(), main = "Serie Temporal") %>% 
                                                          dyAxis("x", label = "Periodo", drawGrid = FALSE) %>%
                                                          dyAxis("y", label = "Valores") %>%
                                                          dySeries(dimnames(serie2())[[2]], label = "Valores", color="#3182bd")  %>%
                                                          dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                                                          dyRangeSelector(height = 40, strokeColor = "lightblue",keepMouseZoom = TRUE) %>%
                                                          dyHighlight(highlightCircleSize = 5) 
                                                
                                                    print(p)
                                                    }
                                                  )
          

                    output$acf1 <- renderPlot(
                                              {
                                                inFile <- input$file1
                                               
                                                if (is.null(inFile))
                                                  return('Cargue el archivo de datos...')  #NULL 
                                                
                                                p <- qacf(serie2())
                                                
                                                print(p)
                                              }
                                            )
                    
                    output$pacf1 <- renderPlot(
                                                {
                                                
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL
                                                  
                                                  p <- qpacf(serie2())
                                                
                                                print(p)
                                                }
                                              )
                    
                    
                    output$tserie  <- renderDygraph(
                                                    {
                                                      
                                                      inFile <- input$file1
                                                      
                                                      if (is.null(inFile))
                                                        return('Cargue el archivo de datos...')  #NULL 
                                                      
                                                      p <- dygraph(serie2(), main = "Serie Temporal") %>% 
                                                        dyAxis("x", label = "Periodo", drawGrid = FALSE) %>%
                                                        dyAxis("y", label = "Valores") %>%
                                                        dySeries(dimnames(serie2())[[2]], label = "Valores", color="#3182bd") %>%
                                                        dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                                                        dyRangeSelector(height = 40, strokeColor = "lightblue",keepMouseZoom = TRUE) %>%
                                                        dyHighlight(highlightCircleSize = 5) 
                                                      
                                                      if(input$seriel)
                                                          {
                                                          if(input$seried == 0)
                                                              {
                                                              p <- dygraph(serie3(), main = "Serie Temporal [Log]") %>% 
                                                                    dyAxis("x", label = "Periodo", drawGrid = FALSE) %>%
                                                                    dyAxis("y", label = "Valores") %>%
                                                                    dySeries(dimnames(serie3())[[2]], label = "Valores", color="#DDA600") %>%
                                                                    dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                                                                    dyRangeSelector(height = 40, strokeColor = "#DDA600",keepMouseZoom = TRUE) %>%
                                                                    dyHighlight(highlightCircleSize = 5)
                                                              }
                                                          
                                                          if(input$seried == 1)
                                                              {
                                                              serield1 <- diff(serie3(), d = 1)
                                                              
                                                              p <- dygraph(serield1, main = "Serie Temporal [Log & d=1]") %>% 
                                                                    dyAxis("x", label = "Periodo", drawGrid = FALSE) %>%
                                                                    dyAxis("y", label = "Valores") %>%
                                                                    dySeries(dimnames(serield1)[[2]], label = "Valores", color="#019E5A") %>%
                                                                    dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                                                                    dyRangeSelector(height = 40, strokeColor = "#019E5A",keepMouseZoom = TRUE) %>%
                                                                    dyHighlight(highlightCircleSize = 5)
                                                              } 
                                                          
                                                          if(input$seried == 2)
                                                              {
                                                              serield2 <- diff(serie3(), d = 2)
                                                              
                                                              p <- dygraph(serield2, main = "Serie Temporal [Log & d=2]") %>% 
                                                                    dyAxis("x", label = "Periodo", drawGrid = FALSE) %>%
                                                                    dyAxis("y", label = "Valores") %>%
                                                                    dySeries(dimnames(serield2)[[2]], label = "Valores", color="#019E5A") %>%
                                                                    dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                                                                    dyRangeSelector(height = 40, strokeColor = "#019E5A",keepMouseZoom = TRUE) %>%
                                                                    dyHighlight(highlightCircleSize = 5)
                                                              }
                                                          }
                                                      else
                                                          {
                                                            if(input$seried == 1)
                                                              {
                                                              serield1 <- diff(serie2(), d = 1)
                                                              
                                                              p <- dygraph(serield1, main = "Serie Temporal [d=1]") %>% 
                                                                    dyAxis("x", label = "Periodo", drawGrid = FALSE) %>%
                                                                    dyAxis("y", label = "Valores") %>%
                                                                    dySeries(dimnames(serield1)[[2]], label = "Valores", color="#767676") %>%
                                                                    dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                                                                    dyRangeSelector(height = 40, strokeColor = "#767676",keepMouseZoom = TRUE) %>%
                                                                    dyHighlight(highlightCircleSize = 5)
                                                              }
                                                     
                                                            if(input$seried == 2)
                                                              {
                                                                serield2 <- diff(serie2(), d = 2)
                                                                
                                                                p <- dygraph(serield2, main = "Serie Temporal [d=2]") %>% 
                                                                      dyAxis("x", label = "Periodo", drawGrid = FALSE) %>%
                                                                      dyAxis("y", label = "Valores") %>%
                                                                      dySeries(dimnames(serield2)[[2]], label = "Valores", color="#767676") %>%
                                                                      dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                                                                      dyRangeSelector(height = 40, strokeColor = "#767676",keepMouseZoom = TRUE) %>%
                                                                      dyHighlight(highlightCircleSize = 5)
                                                              }   
                                                          }
                                                   
                                                      print(p)
                                                    }
                                                  )
                    
                    output$tacf1 <- renderPlot(
                                                {
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  p <- qacf(serie2())
                                                  
                                                  if(input$seriel)
                                                    {
                                                    if(input$seried == 0)
                                                      {
                                                      p <- qacf(serie3())
                                                      }
                                                    
                                                    if(input$seried == 1)
                                                      {
                                                      serield1 <- diff(serie3(), d = 1)
                                                      
                                                      p <- qacf(serield1)
                                                      } 
                                                    
                                                    if(input$seried == 2)
                                                      {
                                                      serield2 <- diff(serie3(), d = 2)
                                                      
                                                      p <- qacf(serield2)
                                                      }
                                                    }
                                                  else
                                                    {
                                                    if(input$seried == 1)
                                                      {
                                                      serield1 <- diff(serie2(), d = 1)
                                                      
                                                      p <- qacf(serield1)
                                                      }
                                                    
                                                    if(input$seried == 2)
                                                      {
                                                      serield2 <- diff(serie2(), d = 2)
                                                      
                                                      p <- qacf(serield2)
                                                      }   
                                                    }
                                       
                                                  print(p)
                                                }
                                              )
                    
                    output$tpacf1 <- renderPlot(
                                                {
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  p <- qpacf(serie2())
                                                  
                                                  if(input$seriel)
                                                    {
                                                    if(input$seried == 0)
                                                      {
                                                      p <- qpacf(serie3())
                                                      }
                                                    
                                                    if(input$seried == 1)
                                                      {
                                                      serield1 <- diff(serie3(), d = 1)
                                                      
                                                      p <- qpacf(serield1)
                                                      } 
                                                    
                                                    if(input$seried == 2)
                                                      {
                                                      serield2 <- diff(serie3(), d = 2)
                                                      
                                                      p <- qpacf(serield2)
                                                      }
                                                    }
                                                  else
                                                    {
                                                    if(input$seried == 1)
                                                      {
                                                      serield1 <- diff(serie2(), d = 1)
                                                      
                                                      p <- qpacf(serield1)
                                                      }
                                                    
                                                    if(input$seried == 2)
                                                      {
                                                      serield2 <- diff(serie2(), d = 2)
                                                      
                                                      p <- qpacf(serield2)
                                                      }   
                                                    }
                                                  
                                                  print(p)
                                                }
                                              )
                    
                    #---------- Fin Panel Serie -----------------------
                 
                    #---------- Inicio Panel Modelo --------------------

                    output$m01 <- renderPrint({modelo01()})
                    
                    output$m11 <- renderPrint({modelo11()})
                    
                    output$sacf1 <- renderPlot(
                                                {
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  p <- sqacf(modelo11()$resid)
                                                  
                                                  print(p)
                                                }
                                              )

                    output$spacf1 <- renderPlot(
                                                {
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  p <- sqpacf(modelo11()$resid)
                                                  
                                                  print(p)
                                                }
                                              )
                    
                    output$m02 <- renderPrint({modelo02()})
                    
                    output$m12 <- renderPrint({modelo12()})
                    
                    output$sacf2 <- renderPlot(
                                                {
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  p <- sqacf(modelo12()$residuals)
                                                  
                                                  print(p)
                                                }
                                              )
                    
                    output$spacf2 <- renderPlot(
                                                  {
                                                    inFile <- input$file1
                                                    
                                                    if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL 
                                                    
                                                    p <- sqpacf(modelo12()$residuals)
                                                    
                                                    print(p)
                                                  }
                                               )
                    
                    output$m03 <- renderPrint({modelo03()})
                    
                    output$m13 <- renderPrint({modelo13()})
                    
                    output$sacf3 <- renderPlot(
                                                {
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  p <- sqacf(modelo13()$residuals)
                                                  
                                                  print(p)
                                                }
                                              )
                    
                    output$spacf3 <- renderPlot(
                                                  {
                                                    inFile <- input$file1
                                                    
                                                    if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL 
                                                    
                                                    p <- sqpacf(modelo13()$residuals)
                                                    
                                                    print(p)
                                                  }
                                               )
                    
                    
                    output$m04 <- renderPrint({modelo04()})
                    
                    output$m14 <- renderPrint({modelo14()})
                    
                    output$sacf4 <- renderPlot(
                                                {
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  p <- sqacf(modelo14()$residuals)
                                                  
                                                  print(p)
                                                }
                                              )
                    
                    
                    output$spacf4 <- renderPlot(
                                                {
                                                  inFile <- input$file1
                                                  
                                                  if (is.null(inFile))
                                                    return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  p <- sqpacf(modelo14()$residuals)
                                                  
                                                  print(p)
                                                }
                                               )
                    
                    #---------- Fin Panel Modelo ---------------------------                    
                    
                    #---------- Inicio Panel Estimación --------------------
                    
                    output$modeloE <- renderPrint({modelo05()})
                    
                    #---------- Fin Panel Estimación------------------------ 
                    
                    #---------- Inicio Panel Análisis-- --------------------
                    
                    output$diagx2 <- renderPrint(
                                                  {
                                                    
                                                  inFile <- input$file1
                                                    
                                                  if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                  m <- Box.test(modelo05()$residuals, type = input$test)
                                                  as.numeric(as.matrix(m$statistic))
                                                  }
                                                )
                    
                    output$diagpv <- renderPrint(
                                                  {
                                                    
                                                  inFile <- input$file1
                                                    
                                                  if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL 
                                                    
                                                  m <- Box.test(modelo05()$residuals, type = input$test)
                                                  as.numeric(as.matrix(m$p.value))
                                                  }
                                                )
                    
                    output$diagdc <- renderPrint(
                                                  {
                                                    
                                                  inFile <- input$file1
                                                    
                                                  if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL   
                                                    
                                                  m <- Box.test(modelo05()$residuals, type = input$test)
                                                  j <- as.numeric(as.matrix(m$p.value))
                                                  
                                                  if(j >= 0.05)
                                                    {
                                                    print("El valor-p de la prueba es mayor que 0.05 por tanto NO se rechaza la hipóteisis nula H0")
                                                    }
                                                  
                                                  if(j < 0.05)
                                                    {
                                                    print("El valor-p de la prueba es menor que 0.05 por tanto SI se rechaza la hipóteisis nula H0")
                                                    }
                                                  }
                                                )
                    
                    output$diagcn <- renderPrint(
                                                  {
                                                    
                                                  inFile <- input$file1
                                                    
                                                  if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL 
                                                    
                                                  m <- Box.test(modelo05()$residuals, type = input$test)
                                                  j <- as.numeric(as.matrix(m$p.value))
                                                  
                                                  if(j >= 0.05)
                                                    {
                                                    print("Por consiguiente, NO hay suficiente evidencia estadística para rechazar la hipótesis nula H0; es decir, los residuos del modelo presentan aleatoriedad.")
                                                    }
                                                  
                                                  if(j < 0.05)
                                                    {
                                                    print("Por consiguiente, SI hay suficiente evidencia estadística para rechazar la hipótesis nula H0; es decir, los residuos del modelo no estan distribuidos de forma independiente.")
                                                    }
                                                  }
                                                )
                                                
                    new.data <- reactive({data.frame(x = serie1()[,1],y = modelo05()$residuals)})
           
                    output$resplot <- renderPlot(
                                                  {
                                                    
                                                  inFile <- input$file1
                                                    
                                                  if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL   
                                                  
                                                  p <- ggplot(data = new.data(),
                                                              aes(x = x, y = y)) + geom_line(colour = "#FF5555") +
                                                                                   scale_y_continuous(name = "Residuos") +
                                                                                   scale_x_continuous(name = "Periodo") +
                                                                                   ggtitle("Residuos de la Serie Estimada")+
                                                    
                                                                                   theme(panel.background = element_rect(size = 3,
                                                                                                                        colour = "black",
                                                                                                                        fill = "#FFFFFF"),
                                                                                         axis.ticks = element_line(size = 2),
                                                                                         axis.title.x = element_text(size = rel(1.2),
                                                                                                                     face = "bold"),
                                                                                         axis.title.y = element_text(size = rel(1.2),
                                                                                                                     face = "bold"),
                                                                                         plot.title = element_text(size = 20,
                                                                                                                   face = "bold",
                                                                                                                   vjust = 1.5),
                                                                                         legend.position = "bottom",
                                                                                         legend.title = element_text(size=rel(1.2),
                                                                                                                     face="bold")
                                                                                         )
                                                  
                                                  print(p)
                                                  }
                                                )
                    
                    
                    output$histres <- renderPlot(
                                                  {
                                                    
                                                  inFile <- input$file1
                                                    
                                                  if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL    
                                                  
                                                  p <- ggplot(data = new.data(),
                                                              aes(y)) + geom_histogram(binwidth = input$bin) +
                                                                        scale_y_continuous(name = "Cantidad") +
                                                                        scale_x_continuous(name = "Residuos") +
                                                                        ggtitle("Histograma de Residuos de la Serie Estimada")+
                                                                        
                                                                        theme(panel.background = element_rect(size = 3,
                                                                                                              colour = "black",
                                                                                                              fill = "white"),
                                                                              axis.ticks = element_line(size = 2),
                                                                              
                                                                              axis.title.x = element_text(size = rel(1.2),
                                                                                                          face = "bold"),
                                                                              axis.title.y = element_text(size = rel(1.2),
                                                                                                          face = "bold"),
                                                                              
                                                                              plot.title = element_text(size = 20,
                                                                                                        face = "bold",
                                                                                                        vjust = 1.5)
                                                                              )
                                                  
                                                  if(input$yaxis == 2)
                                                        {
                                                        p <- ggplot(data = new.data(),
                                                                    aes(y)) + geom_histogram(aes(y = ..density..),binwidth = input$bin) +
                                                                    scale_y_continuous(name = "Densidad") +
                                                                    scale_x_continuous(name = "Residuos") +
                                                                    ggtitle("Histograma de Residuos de la Serie Estimada")+
                                                                    
                                                                    theme(panel.background = element_rect(size = 3,
                                                                                                          colour = "black",
                                                                                                          fill = "white"),
                                                                          axis.ticks = element_line(size = 2),
                                                                          
                                                                          axis.title.x = element_text(size = rel(1.2),
                                                                                                      face = "bold"),
                                                                          axis.title.y = element_text(size = rel(1.2),
                                                                                                      face = "bold"),
                                                                          
                                                                          plot.title = element_text(size = 20,
                                                                                                    face = "bold",
                                                                                                    vjust = 1.5)
                                                                          )
                                                        }
                                                  
                                                  if(input$dens)
                                                        {
                                                        p <- p + geom_density(colour = "#FF7F2A", fill = "#00FF00", alpha = 0.5)
                                                        }
                                                  
                                                  if(input$indivobs)
                                                        {
                                                        p <- p + geom_rug(colour = "RED")
                                                        }
                                                  
                                                  print(p)
                                                  
                                                  }
                                                )
                    
                    #---------- Fin Panel Análisis ------------------------------- 
                    
                    
                    #---------- Incicio Panel Predicción -------------------------
                    
                    
                    output$dygpred <- renderDygraph(
                                                      {
                                                                                                       
                                                        ind <- as.zoo(serie2()[,0])
                                                        val <- as.zoo(serie2()[,1])
                                                        #lwr <- as.zoo(p05$lower)
                                                        fit <- as.zoo(fmodelo05()$mean)
                                                        #upp <- as.zoo(p05$upper)
                                                        
                                                        
                                                        if (input$serielE)
                                                              {
                                                              ind <- as.zoo(serie3()[,0])
                                                              val <- as.zoo(serie3()[,1])
                                                              #lwr <- as.zoo(p05$lower)
                                                              fit <- as.zoo(fmodelo05()$mean)
                                                              #upp <- as.zoo(p05$upper)
                                                              }
                                                        
                                                        
                                                        all<- merge(ind, val, fit)
                                                        all.xts <- xts(all, date_decimal(index(all)))
                                                        
                                                        p <- dygraph(all.xts, "Serie Temporal [Actual & Predicción]") %>%
                                                                    dySeries("val", label = "Actual", color = "#3182bd") %>%
                                                                    dySeries("fit", label = "Predicción", color = "#e41a1c") %>%
                                                                    dyAxis("x", label = "Periodo", drawGrid = FALSE) %>%
                                                                    dyAxis("y", label = "Valores") %>%
                                                                    dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
                                                                    dyRangeSelector(height = 40,
                                                                                    strokeColor = "lightblue",
                                                                                #    dateWindow = c("7975","7991"),
                                                                                    keepMouseZoom = TRUE) %>%
                                                                    dyHighlight(highlightCircleSize = 5) 

                                                        print(p)
                                                      }
                                                    )
                    
                    
                    output$prediccion <- renderPrint({pmodelo05()$pred})
                    
                    output$plotpred <- renderPlot(
                                                  {
                                                    inFile <- input$file1
                                                    
                                                    if (is.null(inFile))
                                                      return('Cargue el archivo de datos...')  #NULL 
                                                  
                                                    pred <- pmodelo05()$pred
                                                    limsup <- pmodelo05()$pred+1.96*pmodelo05()$se
                                                    liminf <- pmodelo05()$pred-1.96*pmodelo05()$se
                                                    
                                                    p <- ts.plot( 
                                                                 pred,
                                                                 limsup,
                                                                 liminf,
                                                                 col=c("#e41a1c","#1ABC9C","#1ABC9C"),
                                                                 gpars=list(xlab = "Periodo", 
                                                                            ylab = "Predicción", 
                                                                            lty  = c(1,2,2),
                                                                            main = "Serie Temporal [Predicción]"
                                                                            )
                                                                 )
                                                                 
                                                  
                                                    print(p)
                                                  }
                                                 )
                    
                    #---------- Fin Panel Predicción ----------------------------- 
                    
                               
                    } # function(input, output)
            
            ) # shinyServer()