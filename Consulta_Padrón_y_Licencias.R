# agregar filtro por giro y dejar todos en caso de estar en blanco

require("DBI"); require("RPostgreSQL"); require( "RPostgres")
require( "leaflet"); require("sf"); require("dplyr")
require("htmltools"); require("shiny"); require("shinydashboard")

conexion<- dbConnect(PostgreSQL(), dbname="BASE-DE-DATOS",
                     host="XXX.XXX.XX.XX",
                     port= 5432,
                     user="Xxxxxxx",
                     password= "xxxxxx")
# as.data.frame(sort(dbListTables(conexion) ))

licencias <- st_read(conexion, "pl_licencias") %>% st_transform(4326)
for(i in c(2,5:10,14)){Encoding(licencias[[i]]) <- 'UTF-8'}
licencias$adeudo <- as.numeric(licencias$adeudo)
licencias$contribuyente_sexo <- factor(licencias$contribuyente_sexo)
licencias$l_an <- substr(licencias$years_debt,1,4) %>% as.numeric()

# annios <- strsplit(licencias$years_debt, ",") %>% unlist() %>% as.numeric()

licencias_df <- st_set_geometry(licencias, NULL)
licencias_df[c('x','y')]  <- licencias %>% st_transform(4326) %>% st_coordinates()

dbDisconnect(conexion) # quitar cuando se tenga la actualización cada cierto tiempo


ui <- fluidPage(
  titlePanel("Aplicación de consulta de Padrón y Licencias"),
  sidebarLayout(
    sidebarPanel(
      # p(), br(), br(), br(), br(),
      p(br(), tags$b("Para usar esta aplicación selecciona el monto de adeudo de interés, el año desde el último pago y el tipo de giro (si no se selecciona se visualizan todos los registros.")), br(),
      p("Verás del lado derecho de la pantalla cómo se actualizan los puntos en el mapa con las licencias que cuenten con registro de coordenadas, en la tabla inferior verás un resumen de todos los registros"),
      br(), p(),
      sliderInput("adeudo",
                  "1) Seleccione montos de adeudo de interés",
                  ticks= TRUE,
                  min = 0,
                  max = max(licencias$adeudo) %>% round()+1,
                  value = c(0,max(licencias$adeudo) %>% round()+1),
                  # value = sort(sample(max(licencias$adeudo), 2)),
                  step = 500), br(),
      selectInput('anno', "2) Seleccione un año desde el último pago", 
                  choices = seq(max(licencias$l_an, na.rm = TRUE), min(licencias$l_an, na.rm = TRUE)), 
                  multiple = FALSE),
      # selectInput('giro', "3) Escriba su giro de interés",
      #             choices = sort(unique(licencias$giro)), multiple = TRUE),br()
          ),
    
    mainPanel(
      # tableOutput("tabla"),
      valueBox(paste0('$ ', format(sum(licencias$adeudo),
                                   nsmall = 2, big.mark = ",")), "Total de adeudos", color = "red"),
      valueBox(length(unique(licencias$nombre[which(licencias$adeudo > 0)])) ,
                                               "Número de deudores", color = 'orange'),
      valueBox(licencias$fecha_alta %>% max() %>% format("%d de %B de %Y") , 
                                               "Fecha de última licencia", color = 'blue'),
      br(),
      leafletOutput("mymap", height = 700), #, height = '100%'
      p(),
      valueBox(dim(licencias)[1] , "Registros georreferenciados", color = "yellow"),
      valueBox(length(licencias$nombre[which(licencias$adeudo > 0)]) ,
               "Número de deudas"),
      valueBox(licencias$fecha_alta %>% min() %>% format("%d de %B de %Y") , 
               "Fecha de licencia más antigua", color = 'blue')
    )
  )
)

server <- function(input, output, session){
    # licencias <- reactivePoll(1.8e6, test, lic) # actualización cada 5hrs 1.8e6
    # preparar función test (comparar dim)
    # preparar función lic para obtener todos los datos
 
  filtro <- reactive({
    licencias %>%
      dplyr::filter(adeudo >= input$adeudo[1] & adeudo <= input$adeudo[2] & grepl(input$anno, years_debt) ) #  & giro == input$giro
  })
  
  # IF(isTruthy(input$giro)){
  #   filtro <- reactive({
  #     licencias %>%
  #       dplyr::filter(between(adeudo, input$adeudo[1], input$adeudo[2]) & grepl(input$anno, years_debt) & giro == input$giro)
  #   })
  # } ELSE {
  #   filtro <- reactive({
  #     licencias %>%
  #       dplyr::filter(between(adeudo, input$adeudo[1], input$adeudo[2]) & grepl(input$anno, years_debt))
  #   })
  # }
  
  
  filtro_df <- reactive(st_set_geometry(filtro(), NULL))

  output$tabla <- renderTable(colnames = TRUE, {head(filtro_df())})
  
  output$mymap<- renderLeaflet({
    variable <- paste0('Adeudo: <b style="color:#890a00;"> $ ',format(filtro()$adeudo, nsmall = 1, big.mark = ","),'</b> <br> Nombre del comercio: ',filtro()$nombre, ' <br> Número de licencia: <b>', filtro()$licencia,'</b> <br> Fecha de alta: <span style="color:blue;">', filtro()$fecha_alta %>% format("%d de %B de %Y"), '</span>', '<br><br> Giro: ', filtro()$giro, ' <br> Localidad: ', filtro()$poblacion, ' <br> Año desde el último pago: ', filtro()$l_an) %>% lapply(htmltools::HTML)
    pale <- colorBin('Reds', filtro()$l_an, bins = 5)
    
    leaflet(filtro()) %>%
    # leaflet(licencias) %>% # Activar en caso de que la línea de arriba no funcione
      addTiles(options = providerTileOptions(opacity = 0.9)) %>%
      addProviderTiles('Esri.WorldImagery',
                       options = providerTileOptions(opacity = 0.45)) %>%
      # addProviderTiles('Esri.WorldImagery', group = "ESRI Sat") %>%
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
