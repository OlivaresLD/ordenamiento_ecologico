require("DBI"); require("RPostgreSQL"); require( "RPostgres"); require("odbc"); require("RODBC")
require( "leaflet"); require("sf"); require("dplyr"); require("htmltools")
require("htmltools"); require("shiny"); require("shinydashboard")

conexion<- dbConnect(PostgreSQL(), dbname="BASE-DE-DATOS",
                     host="XXX.XXX.XX.XX",
                     port= 5432,
                     user="Xxxxxxx",
                     password= "xxxxxx")

con <- odbcConnect("xxxxxxxxx", uid="XXXXXX", pwd = "xxXXXXX") # Mediante conexión DNS establecida previamente para SQL Server


licencias <- st_read(conexion, "pl_licencias") %>% st_transform(4326)
for(i in c(2,5:10,14)){Encoding(licencias[[i]]) <- 'UTF-8'}
licencias$adeudo <- as.numeric(licencias$adeudo)
licencias$contribuyente_sexo <- factor(licencias$contribuyente_sexo)
licencias$l_an <- substr(licencias$years_debt,1,4) %>% as.numeric()

anti <- sqlQuery(con, "SELECT * FROM dbo.Licencias")
deuda <- sqlQuery(con, "SELECT * FROM dbo.Adeudos")

anti$licencia <- sprintf("%010d", anti$NumeroLicencia)
deuda$licencia <- deuda$NumeroLicencia %>% sprintf("%010d",.)

dbDisconnect(conexion) # quitar cuando se tenga la actualización cada cierto tiempo
close(con)

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Aplicación de consulta de Padrón y Licencias", disable = TRUE),
  dashboardSidebar(
    p(br(), tags$b("Para usar esta aplicación da click en los puntos del mapa para conocer los datos de adeudos y licencias. Puedes filtrar la información seleccionando un monto de adeudo de interés, así como el año desde el último pago.")), br(),
    p("Verás del lado derecho de la pantalla cómo se actualizan los puntos en el mapa con las licencias que cuenten con registro de coordenadas. El tamaño del círculo indica el monto del adeudo y entre más claro su color, más viejo el registro de adeudo."),
    br(), p(),
    sliderInput("adeudo",
                "1) Seleccione montos de adeudo de interés",
                ticks= TRUE,
                min = 0,
                max = max(licencias$adeudo) %>% round()+1,
                value = c(0,max(licencias$adeudo) %>% round()+1),
                step = 5000
                ), br(),
    tags$style(type = "text/css",
               HTML(".irs--shiny .irs-grid-text { color: white }", ".irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {background: transparent}")),
    selectInput('anno', "2) Seleccione un año desde el último pago", 
                choices = seq(max(licencias$l_an, na.rm = TRUE), min(licencias$l_an, na.rm = TRUE)), 
                multiple = FALSE)
    # selectInput('giro', "3) Escriba su giro de interés",
    #             choices = sort(unique(licencias$giro)), multiple = TRUE),br()
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("monto", width = 4),
      infoBoxOutput("deudor", width = 3),
      valueBoxOutput("lics", width = 2),
      valueBoxOutput("avan", width = 3),
      br(),
      leafletOutput("mymap", height = 600), #, height = '100%'
      p(),
      valueBoxOutput("montogeo", width = 4),
      infoBoxOutput("deudorgeo", width = 3),
      infoBoxOutput("numgeo", width = 2),
      valueBoxOutput("alta", width = 3)
    )
  )
)

server <- function(input, output, session){
  filtro <- reactive({
    licencias %>%
      dplyr::filter(adeudo >= input$adeudo[1] & adeudo <= input$adeudo[2] & grepl(input$anno, years_debt) ) #  & giro == input$giro
  })
  
  output$monto <- renderValueBox(
    valueBox(paste0('$ ', sum(deuda$Importe) %>% format(nsmall = 2, big.mark = ",")), "Total de adeudos", 
             color = "red", icon = icon("usd", lib = "glyphicon")) # )#
  ) 
  output$deudor <- renderInfoBox(
    infoBox("Número de adeudos",
            tags$p(length(unique(deuda$NumeroLicencia[which(deuda$Importe > 1)])), style = "font-size: 200%;"),
             color = 'orange', icon = icon("pawn", lib = "glyphicon"))
  )
  output$lics <- renderValueBox(
    valueBox((length(anti$NumeroLicencia) + 
                length(licencias$licencia[!(licencias$licencia %in% anti$licencia)]) ) %>% format(big.mark = ","), 
             "Número de licencias", color = "blue", icon = icon("briefcase", lib = "glyphicon")) # )#
  )
  av <- length(licencias$licencia) / (length(anti$NumeroLicencia) + length(licencias$licencia[!(licencias$licencia %in% anti$licencia)]))
  output$avan <- renderValueBox( ## Actulizar con acceso a datos de padrón y licencias oficial
    valueBox(value = sprintf("%0.1f%%", av*100) ,
             "Porcentaje de avance georreferenciado", color = 'green', icon = icon("check", lib = "glyphicon"))
  )
  output$montogeo <- renderValueBox(
    valueBox(paste0('$ ', format(sum(licencias$adeudo), nsmall = 2, big.mark = ",")), 
             "Total de adeudos georreferenciados", color = "red", icon = icon("usd", lib = "glyphicon"))
  )
  output$deudorgeo <- renderInfoBox(
    infoBox("Adeudos georreferenciados", 
            tags$p(length(licencias$nombre[which(licencias$adeudo > 0)]), style = "font-size: 200%;"),
            color = 'yellow', icon = icon("bishop", lib = "glyphicon"))
  )
  output$numgeo <- renderValueBox(
    valueBox(dim(licencias)[1] %>% format(big.mark = ",") , "Licencias georreferenciadas", 
             color = "light-blue", icon = icon("map-marker", lib = "glyphicon"))
  )
  output$alta <- renderInfoBox(
    infoBox("Fecha de última licencia", 
            tags$p(licencias$fecha_alta %>% max() %>% format("%d de %B de %Y"), style = "font-size: 150%;"),
            color = 'navy')
  )
  
  output$mymap<- renderLeaflet({
    variable <- paste0('Adeudo: <b style="color:#890a00;"> $ ',format(filtro()$adeudo, nsmall = 1, big.mark = ","),'</b> <br> Nombre del comercio: ',filtro()$nombre, ' <br> Número de licencia: <b>', filtro()$licencia,'</b> <br> Fecha de alta: <span style="color:blue;">', filtro()$fecha_alta %>% format("%d de %B del %Y"), '</span>', '<br><br> Giro: ', filtro()$giro, ' <br> Localidad: ', filtro()$poblacion, ' <br> Año desde el último pago: ', filtro()$l_an) %>% lapply(htmltools::HTML)
    pale <- colorBin('Reds', filtro()$l_an, bins = 5)
    
    leaflet(filtro()) %>%
    # leaflet(licencias) %>% # Activar en caso de que la línea de arriba no funcione
      addTiles(options = providerTileOptions(opacity = 0.9)) %>%
      addProviderTiles('Esri.WorldImagery',
                       options = providerTileOptions(opacity = 0.45)) %>%
      setView(lng = -105.262, lat = 20.820, zoom = 11) %>%
      addCircleMarkers(
        color = ~pale(l_an),
        label = ~licencia,
        popup = ~variable,
        group = "Padrón y licencias",
        fillOpacity = 0.5,
        radius = ~adeudo*(12/max(adeudo, na.rm = TRUE))+2
      )
  })
}

shinyApp(ui = ui, server = server)
