library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
library(plotly)
library(gridExtra)
library(lubridate)
library(reshape2)
library(grid)
library(gridExtra)
library(gridtext)
library(DT)
library(png)
library(ggpubr)
library(leaflet)
library(rgdal)
#library(raster)
library(rworldmap)
library(sparkline)
library(htmltools)
library(purrr)
library(trendbreaker)
library(zoo)
library(imputeTS)
library(dplyr)
library(stringr)
library(httr)




options(spinner.color="grey",spinner.color.background="#ffffff",spinner.size=2)

header <- dashboardHeader(title = "CEPAL")

sidebar <- dashboardSidebar(
  sidebarMenu(id="tab",
              selectInput("classific","Country classification HDI",choices=c("All","Very high"="VERY HIGH HUMAN DEVELOPMENT","High"="HIGH HUMAN DEVELOPMENT","Medium"="MEDIUM HUMAN DEVELOPMENT","Low"="LOW HUMAN DEVELOPMENT")),
              radioButtons("sex","Gender",choices=c("Male","Female")),
              radioButtons("group","GDI group",choices=c("All",1,2,3,4,5)),
              selectInput("indicator","Indicators",choices=c("GDI","HDI","SDG3","SDG4.3","SDG4.4","SDG8.5"))
  ),
  hr(),
  HTML("<p>Created by <a href='https://www.linkedin.com/in/silvano-oliveira-327740139/'>Silvano Oliveira</a></p>")
)

page <- dashboardBody(
  fluidRow(
    column(width = 6,
      withSpinner(plotlyOutput("plot1",height=800),type = 2)
    ),
    column(width = 6,
      leafletOutput("map",height = 740)
    )
  )
)


ui <- dashboardPage(header,sidebar,page,skin='black')

server <- function(input, output) {
  
  github_link="https://github.com/barbosasil/cepal/raw/main/2020_Statistical_Annex_Table_4.xlsx"
  temp_file=tempfile(fileext = ".xlsx")
  req=GET(github_link,
             authenticate(Sys.getenv("GITHUB_PAT"), ""),
             write_disk(path = temp_file))
  db=data.frame(readxl::read_excel(temp_file,sheet = "Table 4", 
                             col_types = c("numeric","text", "numeric", "skip", "text","skip", "numeric", "skip", "numeric",
                                           "skip", "numeric", "skip", "numeric","skip", "numeric", "skip", "numeric",
                                           "skip", "numeric", "skip", "numeric","skip", "numeric", "skip", "numeric","skip"), skip = 5))
  
  db$HDI_class=NA
  db[is.na(db$HDI.rank)==T,]$HDI_class=db[is.na(db$HDI.rank)==T,]$Country
  
  for(i in 3:nrow(db)){
    db$HDI_class[i]=ifelse(is.na(db$HDI_class[i]),db$HDI_class[i-1],db$HDI_class[i])
  }
  
  
  db=db[is.na(db$HDI.rank)==F,]
  names(db)=c("rank","country","GDI_value","GDI_group","HDI_female","HDI_male","expect_female","expect_male",
              "expect_school_female","expect_school_male","years_school_female","years_school_male","income_female","income_male","HDI_class")
  
  temp1=db[,c("rank","country","GDI_value","GDI_group","HDI_female","expect_female","expect_school_female","years_school_female","income_female","HDI_class")]
  temp2=db[,c("rank","country","GDI_value","GDI_group","HDI_male","expect_male","expect_school_male","years_school_male","income_male","HDI_class")]
  
  names(temp1)=c("rank","country","GDI_value","GDI_group","HDI","expect","expect_school","years_school","income","HDI_class")
  names(temp2)=c("rank","country","GDI_value","GDI_group","HDI","expect","expect_school","years_school","income","HDI_class")
  
  temp1$gender="Female"
  temp2$gender="Male"
  
  db2=rbind(temp1,temp2)
  rm(temp1,temp2)
  
  iso=read.csv("https://raw.githubusercontent.com/barbosasil/cepal/main/iso2e3_world.csv")
  iso[which(iso$iso3=="BHS"),]$country="Bahamas"
  iso[which(iso$iso3=="CAF"),]$country="Central African Republic"
  iso[which(iso$iso3=="COM"),]$country="Comoros"
  iso[which(iso$iso3=="COG"),]$country="Congo"
  iso[which(iso$iso3=="COD"),]$country="Congo (Democratic Republic of the)"
  iso[which(iso$iso3=="DOM"),]$country="Dominican Republic"
  iso[which(iso$iso3=="SWZ"),]$country="Eswatini (Kingdom of)"
  iso[which(iso$iso3=="GMB"),]$country="Gambia"
  iso[which(iso$iso3=="HKG"),]$country="Hong Kong, China (SAR)"
  iso[which(iso$iso3=="KOR"),]$country="Korea (Republic of)"
  iso[which(iso$iso3=="LAO"),]$country="Lao People's Democratic Republic"
  iso[which(iso$iso3=="MHL"),]$country="Marshall Islands"
  iso[which(iso$iso3=="MDA"),]$country="Moldova (Republic of)"
  iso[which(iso$iso3=="NLD"),]$country="Netherlands"
  iso[which(iso$iso3=="NER"),]$country="Niger"
  iso[which(iso$iso3=="MKD"),]$country="North Macedonia"
  iso[which(iso$iso3=="PHL"),]$country="Philippines"
  iso[which(iso$iso3=="RUS"),]$country="Russian Federation"
  iso[which(iso$iso3=="SDN"),]$country="Sudan"
  iso[which(iso$iso3=="TZA"),]$country="Tanzania (United Republic of)"
  iso[which(iso$iso3=="ARE"),]$country="United Arab Emirates"
  iso[which(iso$iso3=="GBR"),]$country="United Kingdom"
  iso[which(iso$iso3=="USA"),]$country="United States"
  
  
  db=merge(db,iso,by="country",all.x=T)
  db2=merge(db2,iso,by="country",all.x=T)
  db2=data.frame(db2)
  
  output$plot1<-renderPlotly({
   if(input$group=='All'){
     if(input$indicator=="HDI"){
       if(input$classific=="All"){
         plot_ly(db2,x=~gender,y=~HDI,type="box") %>% 
           layout(xaxis = list(title="HDI"),yaxis = list(title = 'Gender')) %>% 
           layout(title="HDI by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific),],x=~gender,y=~HDI,type="box") %>% 
           layout(xaxis = list(title="HDI"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("HDI by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="GDI"){
       if(input$classific=="All"){
         plot_ly(db2,x=~gender,y=~GDI_value,type="box") %>% 
           layout(xaxis = list(title="GDI"),yaxis = list(title = 'Gender')) %>% 
           layout(title="GDI by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific),],x=~gender,y=~GDI_value,type="box") %>% 
           layout(xaxis = list(title="GDI"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("GDI by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="SDG3"){
       if(input$classific=="All"){
         plot_ly(db2,x=~gender,y=~expect,type="box") %>% 
           layout(xaxis = list(title="Life expectancy at birth"),yaxis = list(title = 'Gender')) %>% 
           layout(title="Life expectancy at birth by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific),],x=~gender,y=~expect,type="box") %>% 
           layout(xaxis = list(title="Life expectancy at birth"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("Life expectancy at birth by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="SDG4.3"){
       if(input$classific=="All"){
         plot_ly(db2,x=~gender,y=~expect_school,type="box") %>% 
           layout(xaxis = list(title="Expected year of schooling"),yaxis = list(title = 'Gender')) %>% 
           layout(title="Expected year of schooling by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific),],x=~gender,y=~expect_school,type="box") %>% 
           layout(xaxis = list(title="Expected year of schooling"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("Expected year of schooling by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="SDG4.4"){
       if(input$classific=="All"){
         plot_ly(db2,x=~gender,y=~years_school,type="box") %>% 
           layout(xaxis = list(title="Mean years of schooling"),yaxis = list(title = 'Gender')) %>% 
           layout(title="Mean years of schooling by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific),],x=~gender,y=~years_school,type="box") %>% 
           layout(xaxis = list(title="Mean years of schooling"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("Mean years of schooling by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="SDG8.5"){
       if(input$classific=="All"){
         plot_ly(db2,x=~gender,y=~income,type="box") %>% 
           layout(xaxis = list(title="Estimated gross national income"),yaxis = list(title = 'Gender')) %>% 
           layout(title="Estimated gross national income per capita by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific),],x=~gender,y=~income,type="box") %>% 
           layout(xaxis = list(title="Estimated gross national income"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("Estimated gross national income per capita by gender in countries classified at",tolower(input$classific)))
       } 
     }
   } else if(input$group!="All"){
     if(input$indicator=="HDI"){
       if(input$classific=="All"){
         plot_ly(db2[which(db2$GDI_group==input$group),],x=~gender,y=~HDI,type="box") %>% 
           layout(xaxis = list(title="HDI"),yaxis = list(title = 'Gender')) %>% 
           layout(title="HDI by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific & db2$GDI_group==input$group),],x=~gender,y=~HDI,type="box") %>% 
           layout(xaxis = list(title="HDI"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("HDI by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="GDI"){
       if(input$classific=="All"){
         plot_ly(db2[which(db2$GDI_group==input$group),],x=~gender,y=~GDI_value,type="box") %>% 
           layout(xaxis = list(title="GDI"),yaxis = list(title = 'Gender')) %>% 
           layout(title="GDI by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific & db2$GDI_group==input$group),],x=~gender,y=~GDI_value,type="box") %>% 
           layout(xaxis = list(title="GDI"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("GDI by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="SDG3"){
       if(input$classific=="All"){
         plot_ly(db2[which(db2$GDI_group==input$group),],x=~gender,y=~expect,type="box") %>% 
           layout(xaxis = list(title="Life expectancy at birth"),yaxis = list(title = 'Gender')) %>% 
           layout(title="Life expectancy at birth by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific & db2$GDI_group==input$group),],x=~gender,y=~expect,type="box") %>% 
           layout(xaxis = list(title="Life expectancy at birth"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("Life expectancy at birth by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="SDG4.3"){
       if(input$classific=="All"){
         plot_ly(db2[which(db2$GDI_group==input$group),],x=~gender,y=~expect_school,type="box") %>% 
           layout(xaxis = list(title="Expected year of schooling"),yaxis = list(title = 'Gender')) %>% 
           layout(title="Expected year of schooling by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific & db2$GDI_group==input$group),],x=~gender,y=~expect_school,type="box") %>% 
           layout(xaxis = list(title="Expected year of schooling"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("Expected year of schooling by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="SDG4.4"){
       if(input$classific=="All"){
         plot_ly(db2[which(db2$GDI_group==input$group),],x=~gender,y=~years_school,type="box") %>% 
           layout(xaxis = list(title="Mean years of schooling"),yaxis = list(title = 'Gender')) %>% 
           layout(title="Mean years of schooling by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific & db2$GDI_group==input$group),],x=~gender,y=~years_school,type="box") %>% 
           layout(xaxis = list(title="Mean years of schooling"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("Mean years of schooling by gender in countries classified at",tolower(input$classific)))
       } 
     } else if(input$indicator=="SDG8.5"){
       if(input$classific=="All"){
         plot_ly(db2[which(db2$GDI_group==input$group),],x=~gender,y=~income,type="box") %>% 
           layout(xaxis = list(title="Estimated gross national income"),yaxis = list(title = 'Gender')) %>% 
           layout(title="Estimated gross national income per capita by gender")
       } else if(input$classific!="All"){
         plot_ly(db2[which(db2$HDI_class==input$classific & db2$GDI_group==input$group),],x=~gender,y=~income,type="box") %>% 
           layout(xaxis = list(title="Estimated gross national income"),yaxis = list(title = 'Gender')) %>%
           layout(title=HTML("Estimated gross national income per capita by gender in countries classified at",tolower(input$classific)))
       } 
     }
   }

  })
  
  output$map<-renderLeaflet({
    
    wmap <- getMap()
    wmap@data=left_join(wmap@data,db2[which(db2$gender==input$sex),c("iso3","rank","country","GDI_value","GDI_group","HDI","expect","expect_school","years_school","income","HDI_class")],by=c("ADM0_A3"="iso3"))
    
    pal1<-colorBin(palette="YlOrBr", domain=c(min(wmap@data$GDI_value,na.rm=T), max(wmap@data$GDI_value,na.rm=T)), bins = 6, na.color = "white", pretty=FALSE, alpha = TRUE)
    pal2<-colorBin(palette="YlOrBr", domain=c(min(wmap@data$HDI,na.rm=T), max(wmap@data$HDI,na.rm=T)), bins = 6, na.color = "white", pretty=FALSE, alpha = TRUE)
    pal3<-colorBin(palette="YlOrBr", domain=c(min(wmap@data$expect,na.rm=T), max(wmap@data$expect,na.rm=T)), bins = 6, na.color = "white", pretty=FALSE, alpha = TRUE)
    pal4<-colorBin(palette="YlOrBr", domain=c(min(wmap@data$expect_school,na.rm=T), max(wmap@data$expect_school,na.rm=T)), bins = 6, na.color = "white", pretty=FALSE, alpha = TRUE)
    pal5<-colorBin(palette="YlOrBr", domain=c(min(wmap@data$years_school,na.rm=T), max(wmap@data$years_school,na.rm=T)), bins = 6, na.color = "white", pretty=FALSE, alpha = TRUE)
    pal6<-colorBin(palette="YlOrBr", domain=c(min(wmap@data$income,na.rm=T), max(wmap@data$income,na.rm=T)), bins = 6, na.color = "white", pretty=FALSE, alpha = TRUE)
    
    if(input$indicator=="GDI"){
      leaflet(wmap,options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(weight=1, opacity = 1.0,color = 'grey',fillOpacity = 0.9, smoothFactor = 0.5,
                    fillColor = ~pal1(wmap@data$GDI_value),
                    popup = paste0("<strong>Country: </strong>",wmap@data$ADMIN,
                                   "<br><strong>","GDI:"," </strong>",wmap@data$GDI_value),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>% 
        addLegend(pal = pal1,values  = wmap@data$GDI_value,position = "bottomright",title = "GDI") %>% 
        setView(-30, 50 , zoom = 2)
    } else if(input$indicator=="HDI"){
      leaflet(wmap,options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(weight=1, opacity = 1.0,color = 'grey',fillOpacity = 0.9, smoothFactor = 0.5,
                    fillColor = ~pal2(wmap@data$HDI),
                    popup = paste0("<strong>Country: </strong>",wmap@data$ADMIN,
                                   "<br><strong>","HDI:"," </strong>",wmap@data$HDI),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>% 
        addLegend(pal = pal2,values  = wmap@data$HDI,position = "bottomright",title = "HDI") %>% 
        setView(-30, 50 , zoom = 2)
    } else if(input$indicator=="SDG3"){
      leaflet(wmap,options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(weight=1, opacity = 1.0,color = 'grey',fillOpacity = 0.9, smoothFactor = 0.5,
                    fillColor = ~pal3(wmap@data$expect),
                    popup = paste0("<strong>Country: </strong>",wmap@data$ADMIN,
                                   "<br><strong>","Life expectancy at birth:"," </strong>",wmap@data$expect),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>% 
        addLegend(pal = pal3,values  = wmap@data$expect,position = "bottomright",title = "SDG3") %>% 
        setView(-30, 50 , zoom = 2)
    } else if(input$indicator=="SDG4.3"){
      leaflet(wmap,options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(weight=1, opacity = 1.0,color = 'grey',fillOpacity = 0.9, smoothFactor = 0.5,
                    fillColor = ~pal4(wmap@data$expect_school),
                    popup = paste0("<strong>Country: </strong>",wmap@data$ADMIN,
                                   "<br><strong>","Expected years of schooling:"," </strong>",wmap@data$expect_school),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>% 
        addLegend(pal = pal4,values  = wmap@data$expect_school,position = "bottomright",title = "SDG4.3") %>% 
        setView(-30, 50 , zoom = 2)
    } else if(input$indicator=="SDG4.4"){
      leaflet(wmap,options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(weight=1, opacity = 1.0,color = 'grey',fillOpacity = 0.9, smoothFactor = 0.5,
                    fillColor = ~pal5(wmap@data$years_school),
                    popup = paste0("<strong>Country: </strong>",wmap@data$ADMIN,
                                   "<br><strong>","Mean years of schooling:"," </strong>",wmap@data$years_school),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>% 
        addLegend(pal = pal5,values  = wmap@data$years_school,position = "bottomright",title = "SDG4.4") %>% 
        setView(-30, 50 , zoom = 2)
    } else if(input$indicator=="SDG8.5"){
      leaflet(wmap,options = leafletOptions(zoomControl = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(weight=1, opacity = 1.0,color = 'grey',fillOpacity = 0.9, smoothFactor = 0.5,
                    fillColor = ~pal6(wmap@data$income),
                    popup = paste0("<strong>Country: </strong>",wmap@data$ADMIN,
                                   "<br><strong>","Estimated gross national income:"," </strong>",wmap@data$income),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>% 
        addLegend(pal = pal6,values  = wmap@data$income,position = "bottomright",title = "SDG8.5") %>% 
        setView(-30, 50 , zoom = 2)
    }
    
})
  
}

shinyApp(ui, server)







