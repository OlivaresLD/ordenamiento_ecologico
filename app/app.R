##############################################
##############################################
##### Script developed by  ###################
##### Luis Daniel Olivares Martínez  #########
##### View more on: ##########################
##### https://github.com/olivaresld  #########
##############################################
##############################################
# Consulta tu riesgo

require( "leaflet"); require("sf"); require("dplyr")
require("htmltools"); require("shiny"); require("shinydashboard")
library("tidyverse"); library("scales"); library('highcharter')

# setwd("C:/_SIGEBB/shiny consulta Riesgo/app")
colris <- st_read("www/colonias_riesgos.shp") %>% st_transform(4326)
fall <- st_read("www/fallasyfrac_sgm.shp") %>% st_transform(4326)

tab <- read.csv("www/riesgo_colonia.csv", encoding = "UTF-8")

tab$H1 <- tab$TT + tab$H1
tab$H2 <- tab$H2 + tab$H1
tab$H3 <- tab$H3 + tab$H2
tab$H4 <- tab$H4 + tab$H3
tab$H5 <- tab$H4 + tab$H5

riesgo <- data.frame(risk_l = c("Huracán categoría 3","Huracán categoría 4","Tormenta Tropical","Huracán categoría 5","Huracán categoría 1","Huracán categoría 2", "Inundación Pluvial", "Licuefacción", "Aumento del nivel del mar", "Tsunami", "Fallas y fracturas", "Inundación Fluvial"), risk = names(tab)[20:31], orden = c(4,5,1,6,2,3,7,11,10,9,12,8))
riesgo <- riesgo[order(riesgo$orden),]

selection_risk <- list(`Inundación` = as.list(riesgo$risk_l[1:9]),
                       `Cambio climático (SSP2 4.5)` = as.list(riesgo$risk_l[10]),
                       `Agrietamientos y colapsos` = as.list(riesgo$risk_l[11:12]))

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Aplicación de consulta de Riesgos por manzana", disable = TRUE),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      fluidRow(
                        column(12,
                               valueBoxOutput("r_inu", width = 3),
                               valueBoxOutput("r_tsu", width = 3),
                               valueBoxOutput("r_col", width = 3),
                               valueBoxOutput("r_cc", width = 3)),
                        column(12, style = "height:600pm;",
                               box(title = 'Explorador de riesgos por manzana. Haga zoom y luego click en su lugar de interés.', width = 8, 
                                   status = "danger",leafletOutput("mymap", height = 614)),
                               fluidRow(
                                 box(title = 'Explorador por fenómeno natural', width = 4, 
                                     status = "danger",selectInput('risk', ">> Seleccione de la lista el tipo de fenómeno natural de interés",
                                                          choices = c('Estadística general del municipio',selection_risk), multiple = FALSE),
                                     br(), tableOutput("tabla"), # textOutput("texto"), #
                                     fluidRow( br(),
                                     ) ),
                                 box(title = 'Principales localidades afectadas por el fenómeno natural seleccionado', width = 4,
                                     status = "danger", highchartOutput("gra", height = 200)) #  # tableOutput("gra")
                               )
                        )
                      )
                    )
)

server <- function(input, output, session){

  output$texto <- renderText(if(input$risk != 'Estadística general del municipio'){riesgo$risk[input$risk == riesgo$risk_l]} else TRUE) #input$risk
  
  output$tabla <- renderTable({
    condi <-  if(input$risk != 'Estadística general del municipio'){riesgo$risk[input$risk == riesgo$risk_l]} else TRUE
    res_risk <- data.frame(Parámetro = c("Población total","Hombres","Mujeres","Inmigrantes","Con alguna disapacidad","Viviendas"), Valor = NA)
    res_risk$Valor[1] <- sum(tab$pop_t[which(tab[[condi]] != 0)], na.rm = TRUE) %>% 
      format(nsmall = 2, big.mark = ",") %>% paste0('<b style= "font-size: 1.5rem;">',.,'</b>')
    res_risk$Valor[2] <- sum(tab$pop_h[which(tab[[condi]] != 0)], na.rm = TRUE) %>% 
      format(nsmall = 2, big.mark = ",") %>% paste0('<b style= "font-size: 1.5rem;">',.,'</b>')
    res_risk$Valor[3] <- sum(tab$pop_m[which(tab[[condi]] != 0)], na.rm = TRUE) %>% 
      format(nsmall = 2, big.mark = ",") %>% paste0('<b style= "font-size: 1.5rem;">',.,'</b>')
    res_risk$Valor[4] <- sum(tab$pob_for[which(tab[[condi]] != 0)], na.rm = TRUE) %>% 
      format(nsmall = 2, big.mark = ",") %>% paste0('<b style= "font-size: 1.5rem;">',.,'</b>')
    res_risk$Valor[5] <- sum(tab$pob_dis[which(tab[[condi]] != 0)], na.rm = TRUE) %>% 
      format(nsmall = 2, big.mark = ",") %>% paste0('<b style= "font-size: 1.5rem;">',.,'</b>')
    res_risk$Valor[6] <- sum(tab$viv_t[which(tab[[condi]] != 0)], na.rm = TRUE) %>% 
      format(nsmall = 2, big.mark = ",") %>% paste0('<b style= "font-size: 1.5rem;">',.,'</b>')
    
    return(res_risk)
  }, width = '100%', colnames = FALSE, options = list(autoWidth = TRUE), sanitize.text.function = function(x){x})
  
  output$r_inu <- renderValueBox({
    tab$inu <- tab$H5 + tab$inun_pluv + tab$inun_fluvi + tab$tsunami
    inu <- sum(tab$viv_t[which(tab[['inu']] != 0)], na.rm = TRUE)
    por_inu <- inu / sum(tab$viv_t) * 100

    text1_inu <- inu %>% format(nsmall = 2, big.mark = ",")
    text2_inu <- paste0(text1_inu, " (", por_inu %>% round(2),"%)")
      
    valueBox(text2_inu, "Viviendas con riesgo de inundación", 
             color = "blue", icon = icon("smog"))
  })
  
  output$r_col <- renderValueBox({
    tab$col <- tab$licuefacc + tab$fallas_fra
    col <- sum(tab$viv_t[which(tab[['col']] != 0)], na.rm = TRUE)
    por_col <- col / sum(tab$viv_t) * 100
    
    text1_col <- col %>% format(nsmall = 2, big.mark = ",")
    text2_col <- paste0(text1_col, " (", por_col %>% round(2),"%)")
    
    valueBox(text2_col, "Viviendas con riesgo por socavón o colapso", 
             color = "orange", icon = icon("house-damage"))
  })
  
  output$r_tsu <- renderValueBox({
    tsu <- sum(tab$viv_t[which(tab[['tsunami']] != 0)], na.rm = TRUE)
    por_tsu <- tsu / sum(tab$viv_t) * 100
    
    text1_tsu <- tsu %>% format(nsmall = 2, big.mark = ",")
    text2_tsu <- paste0(text1_tsu, " (", por_tsu %>% round(2),"%)")
    
    valueBox(text2_tsu, "Viviendas con riesgo de inundación por tsunami", 
             color = "teal", icon = icon("water"))
  })
  
  output$r_cc <- renderValueBox({
    cc <- sum(tab$viv_t[which(tab[['nivel.mar']] != 0)], na.rm = TRUE)
    por_cc <- cc / sum(tab$viv_t) * 100
    
    text1_cc <- cc %>% format(nsmall = 2, big.mark = ",")
    text2_cc <- paste0(text1_cc, " (", por_cc %>% round(2),"%)")
    
    valueBox(text2_cc, "Viviendas inundables por Cambio Climático al 2100", 
             color = "maroon", icon = icon("temperature-high"))
  })
  
  output$mymap<- renderLeaflet({ #render javascript leaflet
    sinel <- function(campo){
      a <- deparse(substitute(campo)) %>% substr(5,15)
      b <- riesgo$risk_l[which(riesgo$risk == a)]
      
      ifelse(campo > 0, paste0("Riesgo por ", b, "<br>"), "")
    }
   
    variable <- paste0('Población de la manzana: <b style="color:#890a00;">',  tab$pop_t, '</b><br>Mujeres en la manzana: <b>', tab$pop_m, '</b><br>Hombres en la manzana: <b>',  tab$pop_h , '</b><br>Personas con discapacidad: <b>',  tab$pob_dis, '</b><br>Viviendas: <b>',  tab$viv_t, '</b><br>Viviendas habitadas: <b style="color:#890a00;">', tab$viv_hab , '</b><br><p style="font-size:110%; font-weight:bold; font-size:120%; text-align:right; color:#890a00;"><span style="font-style:italic; color:#000001;">Riesgos presentes:</span><br>', sinel(tab$TT), sinel(tab$H1), sinel(tab$H2), sinel(tab$H3), sinel(tab$H4), sinel(tab$H5), sinel(tab$inun_pluv), sinel(tab$inun_fluvi), sinel(tab$tsunami), sinel(tab$nivel.mar), sinel(tab$licuefacc), sinel(tab$fallas_fra)) %>% lapply(htmltools::HTML)
    
    tab$suma <- rowSums(tab[c(23,26:31)], na.rm = TRUE)
    pale <- colorBin('YlOrBr', tab$suma, bins = 6)

    leaflet() %>%
      addProviderTiles('Esri.WorldTopoMap',
                       options = providerTileOptions(opacity = 0.33)) %>%
      setView(lng = -105.262, lat = 20.820, zoom = 11) %>%
      addPolygons(data = colris,
        color = ~pale(tab$suma),
        label = "click para mayor detalle",
        popup = ~variable,
        fillOpacity = 0.75
      )
    
  })
  
  output$gra <- renderHighchart({
    condi <-  if(input$risk != 'Estadística general del municipio'){riesgo$risk[input$risk == riesgo$risk_l]} else TRUE
    
    filtro <- which(tab[[condi]] != 0)
    tabl <- tapply(tab$NOM_LOC[filtro], tab$NOM_LOC[filtro], length)%>% 
      sort(decreasing = TRUE) %>% as.data.frame()
    names(tabl) <- "colonias"; tabl <- cbind(loc = rownames(tabl), tabl)
    tabl <- tabl[which(tabl$loc != ''),]; tabl <- tabl[1:7,]
    
    hchart(tabl, "column", hcaes(x = loc, y = colonias), color="#0B4F53", name = 'Colonias afectadas por localidad') %>%
      hc_yAxis(title = '') %>% hc_xAxis(title = '') %>%
      hc_add_theme(hc_theme_google())
  })
  
}

shinyApp(ui = ui, server = server)