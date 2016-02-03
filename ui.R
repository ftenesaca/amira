#--- Demo 5: ui.R -----------------------------------

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


#--- Demo 5 -----------------------------------

shinyUI(fluidPage(tags$style(HTML("
									                h1.proyecto {color: #2D1152;
                      										     text-align: center;
                                               font-size: xx-large;
                                               font-family: Verdana, Geneva, sans-serif;
                                               font-weight: bold;
                                               text-shadow: 2px 2px #F1F1F1;}
                        
                                  h2.tesis {color: #FFFFFF;
                      									    text-align: center;
                                            font-size: x-large;
                                            font-family: Verdana, Geneva, sans-serif;
                                            font-weight: bold;
                                            text-shadow: 2px 2px #626262;
                                            padding-bottom: 15px;}
                								
                                  table.table1 {background-image: url('texturebg.png');
                                                width: 100%; 
                                                border-radius: 5px;}

                									")
                							),

				          titlePanel(windowTitle = "AMIRA | FCNM-ESPOL | FTNS@K",
                             title = tags$table(class = "table1",tags$tbody(tags$tr(tags$td(tags$a(href="http://www.espol.edu.ec", tags$img(src = "espol.png", alt="Escuela Superior Politécnica del Litoral"))),
																		                                                tags$td(tags$h1(class="proyecto",'Matería Integradora'),
                                                                                            tags$h2(class="tesis",'Diseño e Implementación de un Asistente para el Análisis de Series Temporales Estacionarias',
                                                                                            tags$br(),
                                                                                            'a través de Paquetes de Código Abierto para el Desarrollo de Aplicaciones Web Reactivas')),
                                                                   
                                                                                    tags$td(tags$a(href="http://www.fcnm.espol.edu.ec", tags$img(src = "fcnm.png",alt="Facultad de Ciencias Naturales y Matemáticas"))) 
                                																                   )
														                                              )
											                          )
              							),                                               
                        


          sidebarLayout(sidebarPanel(conditionalPanel(condition = "input.tab == 1",
                                                      
                                                      wellPanel(HTML('<p>
                                                                      <center>
                                                                      <img src="amira.png" width = "50%"/>
                                                                      <br/>
                                                                      <br/>
                                                                      <b><a href="https://espol.shinyapps.io/Demo5/" target = "_self">Asistente para el Análisis de Series de Tiempo</a></b>
                                                                      
                                                                      <br/><br/>Fernando Tenesaca<br/>
                                                                      <a href="https://www.facebook.com/ftenesaca" target = "_blank">Facebook</a>
                                                                      <b>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;</b>
                                                                      <a href="https://twitter.com/ftenesaca" target = "_blank">@ftenesaca</a>
                                                                      </center>
                                                                      </p>'
                                                                    )
                                                              ),
                                                      
                                                      # wellPanel, Crea un panel con borde y fondo gris
                                                      wellPanel(strong("ARCHIVO DE DATOS"),
                                                                
                                                                # fileInput, Crea un control para cargar uno o más archivos
                                                                  # inputId= Variable de ingreso.
                                                                  # label= Etiqueta.
                                                                  # multiple= Varios archivos (FALSE)  
                                                                  # accept= MIME Types (NULL)
                                                                fileInput(inputId  = "file1",
                                                                          label    = "",
                                                                          multiple = FALSE,
                                                                          accept   = c("text/csv",
                                                                                       "text/comma-separated-values,text/plain")
                                                                          ),
                                                                                                                        
                                                                # checkboxInput, Crea un control para especificar valores lógicos
                                                                  # inputId= Variable de ingreso.
                                                                  # label= Etiqueta.
                                                                  # value= Valor incial (TRUE/FALSE)
                                                                checkboxInput(inputId = "header",
                                                                              label   = "Encabezado",
                                                                              value   = FALSE),
                                                                
                                                                # Añade una línea horizontal
                                                                  # Glosario de Tags HTML en Sniny
                                                                  # http://shiny.rstudio.com/articles/tag-glossary.html
                                                                tags$hr(),
                                                                
                                                                # radioButtons, Crea un control para generar una lista de selección
                                                                  # inputId= Variable de ingreso.
                                                                  # label= Etiqueta.
                                                                  # choices= Lista de valores
                                                                  # selected= Valor inicial de la lista
                                                                  # inline= Presentación vertical/horizontal (FALSE)
                                                                radioButtons(inputId  = "sep",
                                                                             label    = "Separador",
                                                                             choices  = c("Coma"         = ",",
                                                                                          "Punto y coma" = ";",
                                                                                          "Tabulador"    = "\t"),
                                                                             selected = "\t",
                                                                             inline   = FALSE),
                                                                
                                                                tags$hr(),
                                                                
                                                                radioButtons(inputId  = "quote",
                                                                             label    = "Comilla",
                                                                             choices  = c("Ninguna"        = "",
                                                                                          "Comilla doble"  = '"',
                                                                                          "Comilla simple" = "'"),
                                                                             selected = "",
                                                                             inline   = FALSE),
                                                                
                                                                tags$hr(),
                                                                
                                                                radioButtons(inputId  = "dec",
                                                                             label    = "Símbolo Decimal",
                                                                             choices  = c("Coma" = ",",
                                                                                          "Punto"= "."),
                                                                             selected = ".",
                                                                             inline   = FALSE)
                
                                                                )
                                                      
                                                      ),
                                                                                           
                                     conditionalPanel(condition = "input.tab == 2",
                                                      
                                                      wellPanel(HTML('<p>
                                                                      <center>
                                                                      <img src="amira.png" width = "50%"/>
                                                                      <br/>
                                                                      <br/>
                                                                      <b><a href="https://espol.shinyapps.io/Demo5/" target = "_self">Asistente para el Análisis de Series de Tiempo</a></b>
                                                                      
                                                                      <br/><br/>Fernando Tenesaca<br/>
                                                                      <a href="https://www.facebook.com/ftenesaca" target = "_blank">Facebook</a>
                                                                      <b>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;</b>
                                                                      <a href="https://twitter.com/ftenesaca" target = "_blank">@ftenesaca</a>
                                                                      </center>
                                                                      </p>'
                                                                    )
                                                                ),
                                                      
                                                      # wellPanel, Crea un panel con borde y fondo gris
                                                      wellPanel(strong("SERIE TEMPORAL"), 
                                                                
                                                                br(),
                                                                br(),
                                                                
                                                                selectInput(inputId  = "seriefr",
                                                                            label    = "Frecuencia",
                                                                            choices  = c("Mensual"    = "12",
                                                                                         "Trimestral" = "4",
                                                                                         "Anual"      = "1"),
                                                                            selected = "1"),
                                                                
                                                                numericInput(inputId = "seriea",
                                                                             label   = "Año",
                                                                             min     = 0,
                                                                             max     = 2050,
                                                                             value   = 0),
                                                                
                                                                conditionalPanel(condition = "input.seriefr == 12",
                                                                            
                                                                                 selectInput(inputId = "seriem",
                                                                                             label   = "Mes",
                                                                                             choices = c("Enero"     = "1",
                                                                                                         "Febrero"   = "2",
                                                                                                         "Marzo"     = "3",
                                                                                                         "Abril"     = "4",
                                                                                                         "Mayo"      = "5",
                                                                                                         "Junio"     = "6",
                                                                                                         "Julio"     = "7",
                                                                                                         "Agosto"    = "8",
                                                                                                         "Septiembre"= "9",
                                                                                                         "Octubre"   = "10",
                                                                                                         "Noviembre" = "11",
                                                                                                         "Diciembre" = "12"),
                                                                                             selected = "1")
                                                                                 
                                                                                 ),
                                                                
                                                                conditionalPanel(condition = "input.seriefr == 4",
                                                                                 
                                                                                 selectInput(inputId = "serieq",
                                                                                             label   = "Trimestre",
                                                                                             choices = c("Primero" = "1",
                                                                                                         "Segundo" = "2",
                                                                                                         "Tercero" = "3",
                                                                                                         "Cuarto"  = "4"),
                                                                                             selected = "1")
                                                                                 
                                                                                )
                                                              ),
                                                      
                                                      wellPanel(checkboxInput(inputId ="seriet",
                                                                              label   = strong("TRANSFORMAR SERIE TEMPORAL"),
                                                                              value   = FALSE),
                                                                br(),
                                                                
                                                                conditionalPanel(condition = "input.seriet == true",
                                                                                 
                                                                                 wellPanel(checkboxInput(inputId ="seriel",
                                                                                                         label   = strong("Transformación Logarítmica"),
                                                                                                         value   = FALSE),
                                                                                           
                                                                                           selectInput(inputId = "seried",
                                                                                                       label = "Diferenciación Ordinaria  (d)",
                                                                                                       choices = list("Sin diferenciar"= 0,
                                                                                                                      "Primer Orden"   = 1,
                                                                                                                      "Segundo Orden"  = 2),
                                                                                                       selected = 0)
                                                                                          )
                                                                                )
                                                                )
                                                      
                                                      ),
                                     
                                     conditionalPanel(condition = "input.tab == 3",
                                                      
                                                      wellPanel(HTML('<p>
                                                                      <center>
                                                                      <img src="amira.png" width = "50%"/>
                                                                      <br/>
                                                                      <br/>
                                                                      <b><a href="https://espol.shinyapps.io/Demo5/" target = "_self">Asistente para el Análisis de Series de Tiempo</a></b>
                                                                      
                                                                      <br/><br/>Fernando Tenesaca<br/>
                                                                      <a href="https://www.facebook.com/ftenesaca" target = "_blank">Facebook</a>
                                                                      <b>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;</b>
                                                                      <a href="https://twitter.com/ftenesaca" target = "_blank">@ftenesaca</a>
                                                                      </center>
                                                                      </p>'
                                                                    )
                                                                ),
                                                      
                                                      wellPanel(strong("ORDEN DEL MODELO"),
                                                                
                                                                br(),
                                                                br(),
                                                                
                                                                wellPanel(
                                                                            sliderInput(inputId = "ar",
                                                                                        label = "Autoregresivo",
                                                                                        min = 0,
                                                                                        max = 25,
                                                                                        value = 1,width="100%"),
                                                                            
                                                                            checkboxInput(inputId ="serielar",
                                                                                          label   = strong("Transformación Logarítmica"),
                                                                                          value   = FALSE)
                                                                         ),
                                                                
                                                                wellPanel(
                                                                            sliderInput(inputId = "ma",
                                                                                        label = "Media Móvil",
                                                                                        min = 0,
                                                                                        max = 25,
                                                                                        value = 1,width="100%"),
                                                                            
                                                                            
                                                                            checkboxInput(inputId ="serielma",
                                                                                          label   = strong("Transformación Logarítmica"),
                                                                                          value   = FALSE)
                                                                            
                                                                          ),
                                                                
                                                                wellPanel(
                                                                              strong("Autoregresivo de Media Móvil"),
                                                                              br(),
                                                                              br(),
                                                                              sliderInput(inputId = "ar1",
                                                                                          label = "Autoregresivo",
                                                                                          min = 0,
                                                                                          max = 25,
                                                                                          value = 1,width="100%"),
                                                                              
                                                                              sliderInput(inputId = "ma1",
                                                                                          label = "Media Móvil",
                                                                                          min = 0,
                                                                                          max = 25,
                                                                                          value = 1,width="100%"),
                                                                              
                                                                              checkboxInput(inputId ="serielarma",
                                                                                            label   = strong("Transformación Logarítmica"),
                                                                                            value   = FALSE)
                                                                              
                                                                          ),
                                                                
                                                                wellPanel(
                                                                              strong("Autoregresivo Integrado de Media Móvil"),
                                                                              br(),
                                                                              br(),
                                                                              sliderInput(inputId = "ar2",
                                                                                          label = "Autoregresivo",
                                                                                          min = 0,
                                                                                          max = 25,
                                                                                          value = 1,width="100%"),
                                                                              
                                                                              sliderInput(inputId = "diff2",
                                                                                          label = "Diferenciación (d)",
                                                                                          min = 0,
                                                                                          max = 2,
                                                                                          value = 0,width="100%"),
                                                                              
                                                                              sliderInput(inputId = "ma2",
                                                                                          label = "Media Móvil",
                                                                                          min = 0,
                                                                                          max = 25,
                                                                                          value = 1,width="100%"),
                                                                              
                                                                              checkboxInput(inputId ="serielarima",
                                                                                            label   = strong("Transformación Logarítmica"),
                                                                                            value   = FALSE)
                                                                              
                                                                            )
                                                               
                                                                )
                                                      
                                                      ),
                                     
                                     conditionalPanel(condition = "input.tab == 4 || input.tab == 5 || input.tab == 6",
                                                      
                                                      wellPanel(HTML('<p>
                                                                     <center>
                                                                     <img src="amira.png" width = "50%"/>
                                                                     <br/>
                                                                     <br/>
                                                                     <b><a href="https://espol.shinyapps.io/Demo5/" target = "_self">Asistente para el Análisis de Series de Tiempo</a></b>
                                                                     
                                                                     <br/><br/>Fernando Tenesaca<br/>
                                                                     <a href="https://www.facebook.com/ftenesaca" target = "_blank">Facebook</a>
                                                                     <b>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;</b>
                                                                     <a href="https://twitter.com/ftenesaca" target = "_blank">@ftenesaca</a>
                                                                     </center>
                                                                     </p>'
                                                                    )
                                                                ),
                                                      
                                                      wellPanel(strong("ORDEN DEL MODELO"),
                                                                
                                                                br(),
                                                                br(),
                                                                
                                                                wellPanel(
                                                                          sliderInput(inputId = "arE",
                                                                                      label = "Autoregresivo",
                                                                                      min = 0,
                                                                                      max = 5,
                                                                                      value = 1,width="100%"),
                                                                          
                                                                          sliderInput(inputId = "diffE",
                                                                                      label = "Diferenciación (d)",
                                                                                      min = 0,
                                                                                      max = 2,
                                                                                      value = 1,width="100%"),
                                                                  
                                                                          sliderInput(inputId = "maE",
                                                                                      label = "Media Móvil",
                                                                                      min = 0,
                                                                                      max = 5,
                                                                                      value = 1,width="100%"),
                                                                          
                                                                          checkboxInput(inputId ="serielE",
                                                                                        label   = strong("Transformación Logarítmica"),
                                                                                        value   = FALSE)
                                                                         )
                                                              )
                                                   
                                                      ),
                                     
                                     
                                     conditionalPanel(condition = "input.tab == 5",
                                                      
                                                      wellPanel(radioButtons(inputId = "yaxis",
                                                                             #label = strong("HISTOGRAMA"),
                                                                             label = "HISTOGRAMA",
                                                                             choices = c("Cantidad" = 1,
                                                                                         "Densidad" = 2),
                                                                             selected = 1),
                                                                
                                                                wellPanel(
                                                                sliderInput(inputId = "bin",
                                                                            label = "Intervalo",
                                                                            min = 0.1,
                                                                            max = 5,
                                                                            value = 0.1,width="100%"),
                                                                
                                                                checkboxInput(inputId = "indivobs",
                                                                              labe = "Observaciones individuales",
                                                                              value = FALSE),
                                                                
                                                                conditionalPanel(condition="input.yaxis == 2",
                                                                                 
                                                                                 checkboxInput(inputId = "dens",
                                                                                               label = "Densidad estimada",
                                                                                               value = FALSE)
                                                                                )
                                                                        )
                                                                )
                                                     ),
                                     
                                     
                                     
                                     conditionalPanel(condition = "input.tab == 6",
                                                      
                                                      wellPanel(strong("PREDICCION"),
                                                                
                                                                br(),
                                                                br(),
                                                                
                                                                wellPanel(
                                                                          sliderInput(inputId = "periodo",
                                                                                      label = "Periodo",
                                                                                      min = 1,
                                                                                      max = 25,
                                                                                      value = 12,width="100%")
                                                                          )
                                                                )
                                                      ),
                                     
                                     width = "3" # Puede añadir [,width= n] para establecer el ancho del sidebarPanel
                                     
                                                   
                                     ),# Fin del sidebarPAnel
                        
                        mainPanel(tabsetPanel(id = "tab",
                                              
                                              tabPanel(title = "Datos",
                                                       
                                                       helpText("En esta sección se carga el archivo en formato texto/CSV con los datos que se utilizan para el análisis.",
                                                                br(),
                                                                br()
                                                                ),
                                                       
                                                       dataTableOutput("datos1"),
                                                       
                                                       value = 1
                                                       ),
                                              
                                              
                                              tabPanel(title = "Serie",
                                                       
                                                       HTML('<p>En esta sección se establece los parámetros de la serie temporal orignal y se se visualizan los gráficos de:</p>

                                                             <ul>
                                                                <li>La serie de tiempo (Plot).</li>
                                                              	<li>La función de autocorrelación (ACF).</li>
                                                              	<li>La función de autocorrelación parcial (PACF).</li>
                                                             </ul>
                                                            
                                                            <p>&nbsp;</p>
                                                            '),
                                                       
                                                       tabsetPanel(id = "subtab", 
                                                                   tabPanel(title = "Plot",
                                                                            dygraphOutput("serie"),
                                                                            br(),
                                                                            value = 1),
                                                                   tabPanel(title = "ACF",
                                                                            plotOutput(outputId = "acf1"),
                                                                            br(),
                                                                            value = 2),
                                                                   tabPanel(title = "PACF",
                                                                            plotOutput(outputId = "pacf1"),
                                                                            br(),
                                                                            value = 3)
                                                                   
                                                                  ),
                                                     
                                                       conditionalPanel(condition = "input.seriet == true",
                                                                        
                                                                        HTML('<p>En esta sección se aplica las transformaciones seleccionadas a la serie temporal orignal y se se visualizan los gráficos de:</p>

                                                                             <ul>
                                                                                <li>La serie de tiempo (Plot).</li>
                                                                                <li>La función de autocorrelación (ACF).</li>
                                                                                <li>La función de autocorrelación parcial (PACF).</li>
                                                                             </ul>
                                                                             
                                                                             <p>&nbsp;</p>
                                                                             '),
                                                                        
                                                                        tabsetPanel(id = "subtab", 
                                                                                    tabPanel(title = "Plot",
                                                                                             dygraphOutput("tserie"),
                                                                                             br(),
                                                                                             value = 1),
                                                                                    tabPanel(title = "ACF",
                                                                                             plotOutput(outputId = "tacf1"),
                                                                                             br(),
                                                                                             value = 2),
                                                                                    tabPanel(title = "PACF",
                                                                                             plotOutput(outputId = "tpacf1"),
                                                                                             br(),
                                                                                             value = 3)
                                                                                    
                                                                                    )
                                                                        
                                                                        
                                                                        ),
                                                     
                                                       value = 2
                                                       ),
                                              
                                                                                            
                                              tabPanel(title = "Modelos",
                                                       
                                                       helpText("En esta sección se identifica el modelo de la serie temporal utililzando Correlogramas,
                                                                 la", strong("Función de Autocorrelación"), "(ACF) y la",
                                                                 strong("Función de Autocorrelación Parcial"), "(PACF).",
                                                                 br(),
                                                                 br()
                                                                 ),
                                                       
                                                       tabsetPanel(id = "subtab",
                                                                   
                                                                   tabPanel(title = "Modelo AR",
                                                                            
                                                                            verbatimTextOutput(outputId = "m01"),
                                                                            
                                                                            selectInput(inputId = "m01.metodo",
                                                                                        label = "Método para ajustar el modelo",
                                                                                        choices = c("OLS" = "ols",
                                                                                                    "MLE"="mle"),
                                                                                        selected = "ols"),
                                                                            
                                                                            tabsetPanel(id = "subtab", 
                                                                                        tabPanel(title = "Selección",
                                                                                                 
                                                                                                 verbatimTextOutput(outputId = "m11"),
                                                                                                 
                                                                                                 selectInput(inputId = "m11.metodo",
                                                                                                             label = "Método para ajustar el modelo",
                                                                                                             choices = c("OLS" = "ols",
                                                                                                                         "MLE"="mle"),
                                                                                                             selected = "ols"),
                                                                                                 br(),
                                                                                                 br(),
                                                                                                 br(),
                                                                                                 
                                                                                                 
                                                                                                 value = 1),
                                                                                        tabPanel(title = "ACF [Residuos]",
                                                                                                 plotOutput(outputId = "sacf1"),
                                                                                                 value = 2),
                                                                                        tabPanel(title = "PACF [Residuos]",
                                                                                                 plotOutput(outputId = "spacf1"),
                                                                                                 value = 3)
                                                                                        
                                                                                        ),
                                                                               
                                                                            value = 1),
                                                                   
                                                                   tabPanel(title = "Modelo MA",
                                                                                                                                                        
                                                                            verbatimTextOutput(outputId = "m02"),
                                                                            
                                                                            selectInput(inputId = "m02.metodo",
                                                                                      label = "Método para ajustar el modelo",
                                                                                       choices = c("AICc" = "aicc",
                                                                                                   "AIC"="aic",
                                                                                                   "BIC"="bic"),
                                                                                       selected = "aicc"),
                                                                                                                                                        
                                                                            tabsetPanel(id = "subtab", 
                                                                                        tabPanel(title = "Selección",
                                                                                                 
                                                                                                 verbatimTextOutput(outputId = "m12"),
                                                                                                 
                                                                                                 selectInput(inputId = "m12.metodo",
                                                                                                             label = "Método para ajustar el modelo",
                                                                                                             choices = c("CSS" = "CSS",
                                                                                                                         "MLE"="ML"),
                                                                                                             selected = "CSS"),
                                                                                                 br(),
                                                                                                 br(),
                                                                                                 br(),
                                                                                                 
                                                                                                 value = 1),
                                                                                        tabPanel(title = "ACF [Residuos]",
                                                                                                 plotOutput(outputId = "sacf2"),
                                                                                                 value = 2),
                                                                                        tabPanel(title = "PACF [Residuos]",
                                                                                                 plotOutput(outputId = "spacf2"),
                                                                                                 value = 3)
                                                                                        
                                                                                        ),
                                                                            
                                                                            
                                                                            value = 2),
                                                                   
                                                                   tabPanel(title = "Modelo ARMA",
                                                                            
                                                                            verbatimTextOutput(outputId = "m03"),
                                                                            
                                                                            selectInput(inputId = "m03.metodo",
                                                                                        label = "Método para ajustar el modelo",
                                                                                        choices = c("AICc" = "aicc",
                                                                                                    "AIC"="aic",
                                                                                                    "BIC"="bic"),
                                                                                        selected = "aicc"),
                                                                            
                                                                            tabsetPanel(id = "subtab", 
                                                                                        tabPanel(title = "Selección",
                                                                                                 
                                                                                                 verbatimTextOutput(outputId = "m13"),
                                                                                                 
                                                                                                 selectInput(inputId = "m13.metodo",
                                                                                                             label = "Método para ajustar el modelo",
                                                                                                             choices = c("CSS" = "CSS",
                                                                                                                         "MLE"="ML"),
                                                                                                             selected = "CSS"),
                                                                                                 br(),
                                                                                                 br(),
                                                                                                 br(),
                                                                                                 
                                                                                                 value = 1),
                                                                                        tabPanel(title = "ACF [Residuos]",
                                                                                                 plotOutput(outputId = "sacf3"),
                                                                                                 value = 2),
                                                                                        tabPanel(title = "PACF [Residuos]",
                                                                                                 plotOutput(outputId = "spacf3"),
                                                                                                 value = 3)
                                                                                        
                                                                                        ),
                                                                            
                                                                            
                                                                            value = 3),
                                                                   
                                                                   tabPanel(title = "Modelo ARIMA",
                                                                            
                                                                            verbatimTextOutput(outputId = "m04"),
                                                                            
                                                                            selectInput(inputId = "m04.metodo",
                                                                                        label = "Método para ajustar el modelo",
                                                                                        choices = c("AICc" = "aicc",
                                                                                                    "AIC"="aic",
                                                                                                    "BIC"="bic"),
                                                                                        selected = "aicc"),
                                                                            
                                                                            tabsetPanel(id = "subtab", 
                                                                                        tabPanel(title = "Selección",
                                                                                                 
                                                                                                 verbatimTextOutput(outputId = "m14"),
                                                                                                 
                                                                                                 selectInput(inputId = "m14.metodo",
                                                                                                             label = "Método para ajustar el modelo",
                                                                                                             choices = c("CSS" = "CSS",
                                                                                                                         "MLE"="ML"),
                                                                                                             selected = "CSS"),
                                                                                                 br(),
                                                                                                 br(),
                                                                                                 br(),
                                                                                                 
                                                                                                 value = 1),
                                                                                        tabPanel(title = "ACF [Residuos]",
                                                                                                 plotOutput(outputId = "sacf4"),
                                                                                                 value = 2),
                                                                                        tabPanel(title = "PACF [Residuos]",
                                                                                                 plotOutput(outputId = "spacf4"),
                                                                                                 value = 3)
                                                                                        ),
                                                                            value = 4)
                                                                  ),
                                                     
                                                       value = 3
                                                       ),
                                              
                                              
                                              tabPanel(title = "Estimación",
                                                       
                                                       helpText("En esta sección se estima orden del modelo que mejor describe la serie en función del modelo identificado",
                                                                br(),
                                                                br()
                                                                ),
                                                       
                                                       verbatimTextOutput(outputId = "modeloE"),
                                                       
                                                       selectInput(inputId = "m05.metodo",
                                                                   label = "Método para ajustar el modelo",
                                                                   choices = c("CSS" = "CSS",
                                                                               "MLE"="ML"),
                                                                   selected = "ML"),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       
                                                       value = 4
                                                       ),
                                              
                                              tabPanel(title = "Análisis",
                                                       
                                                       helpText("En esta sección se analizan los residuos de la serie estimada de acuerdo al modelo seleccionado (Prueba Portmanteau).",
                                                                br(),
                                                                br()
                                                                ),
                                                       
                                                       withTags(div(class = "fluid-row",
                                                                    div(selectInput(inputId = "test",
                                                                                    label = strong("Prueba de Aletoriedad de los Residuos"),
                                                                                    choices = c("Box-Pierce"= "Box",
                                                                                                "Ljung-Box"= "Ljung"),
                                                                                    selected = "Ljung"),
                                                                        br(),
                                                                        br(),
                                                                        
                                                                        helpText(strong("Hipóteisis:"),
                                                                                 br(),
                                                                                 
                                                                                 strong("H0:"),
                                                                                 "Los datos SI se distribuyen de forma independiente.",
                                                                                 br(),
                                                                                 
                                                                                 strong("H1:"),
                                                                                 "Los datos NO se distribuyen de forma independiente.",
                                                                                 br(),
                                                                                 br(),
                                                                                 br(),
                                                                                 
                                                                                 strong("Cálculo:"),
                                                                                 br(),
                                                                                 "El estadístico de prueba es:",
                                                                                 textOutput(outputId = "diagx2"),
                                                                                 "El valor-p de la prueba es:",
                                                                                 textOutput(outputId = "diagpv"),
                                                                                 br(),
                                                                                 br(),
                                                                                 
                                                                                 strong("Decisión:"),
                                                                                 textOutput(outputId = "diagdc"),
                                                                                 br(),
                                                                                 br(),
                                                                                 
                                                                                 strong("Conclusión:"),
                                                                                 textOutput(outputId = "diagcn")
                                                                                 ),
                                                                        class = "span5"
                                                                        ),
                                                                    
                                                                    div(class = "span7",
                                                                        
                                                                        plotOutput(outputId = "resplot", height = "290px"),
                                                                        br(),
                                                                        br(),
                                                                        
                                                                        plotOutput(outputId = "histres", height = "290px")
                                                                       )
                                                                   )
                                                              ),
                                                              
                                                       br(),
                                                       br(),
                                                       
                                                       value = 5
                                                       ),
                                              
                                              
                                              tabPanel(title = "Predicción",
                                                       
                                                       helpText("En esta sección se predice los valores asociados a la serie estimada de acuerdo al modelo seleccionado.",
                                                                br(),
                                                                br()                                                                
                                                                ),
                                                       
                                                       tabsetPanel(id = "subtab", 
                                                                   tabPanel(title = "Predicción",
                                                                            plotOutput(outputId = "plotpred"),
                                                                            br(),
                                                                            value = 1),
                                                                   tabPanel(title = "Compuesta",
                                                                            dygraphOutput("dygpred"),
                                                                            br(),
                                                                            value = 2)
                                                                   ),
                                                       
                                                       verbatimTextOutput(outputId = "prediccion"),
                                                       

                                                       value = 6
                                                       ) #Aquí va coma si se añade tabs
                                              
                                              
                                              #tabPanel(title = "Código",
                                                       
                                                       
                                                    
                                              #         value = 7
                                              #        )
                                              ) # tabsetpanel()
                                  
                                  ) # mainPanel()
                          )#sidebarLayout()
                        ) # fluidPage()
        
        ) # shinyUI()