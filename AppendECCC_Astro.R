#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#install.packages("weathercan", repos = "https://ropensci.r-universe.dev")
# establish packages required
packages <- c("weathercan", "dplyr", "tidyverse", "ggplot2", "naniar", "feedr","mapview","lunar","lutz", "sf")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Append ECCC weather and Earth astro metrics to point location data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("OWL", "Choose a CSV file with dates and coordinates", 
                      accept=c(
                        "text/csv",
                        "text/comma-seperated-values, text/plain",
                        ".csv")
                      ),
            checkboxInput("Weather", "Weather", TRUE),
            checkboxInput("Solar", "Solar", TRUE),
            checkboxInput("Lunar", "Lunar", TRUE),
            sliderInput("radius",
                        "Distance to find stations:",
                        min = 50,
                        max = 5000,
                        value = 500,step = 25)
            ,
            
            # Button
            downloadButton("downloadData", "Download")),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

OWL <- reactive({
  if(is.null(input$OWL))
    return(NULL)
  
  GD_Aug <-read.csv(input$OWL$datapath)
    #### Date and time management to address concatenated date/time ####
    #Convert Avenza Dates/Times to R times for GD_Aug where both start and end times exist
     
    dtparts = t(as.data.frame(strsplit(GD_Aug$Date.Created, 'T')))
    row.names(dtparts) = NULL #get rid of name column that is baggage from strsplit
    tmparts = t(as.data.frame(strsplit(dtparts[,2],'-')))
    row.names(tmparts) = NULL
    # tmparts[,1]+tmparts[,2]
    thetimes = chron::chron(dates=dtparts[,1],
                            times=tmparts[,1],
                            format=c('y-m-d','h:m:s'))
    thetimes.2 <- as.POSIXct(thetimes)
    attr(thetimes.2,"tzone") <- "America/Denver"   #Need to address the time zone issue
    
    GD_Aug$NewDate <- thetimes.2 + lubridate::hm(tmparts[,2])  #Correct for time zones and DST based on the Avenza snippet - should correct for DST
   
    
    #### Initiate weather sequence if the checkbox for weather is checked    ####
    if(input$Weather){
      
      allstnbyOWL <- numeric(1)  #Create an empty vector to hold stationID
      weathTab <- NA  #Create an empty df that will hold the interim table for each row
      i=1 #reset the index.. likely redundant
      
      #Loop to look up the station ID for each location... integrate weather dl later
      for(i in 1:nrow(GD_Aug)) {
        weathTab  <-  weathercan::stations_search( 
          coords=c(GD_Aug[i,]$Latitude,GD_Aug[i,]$Longitude),
          interval= "hour", 
          dist=as.numeric(input$radius)) %>%
          select(-prov, -climate_id, -WMO_id, -TC_id) %>%
          filter(end >= 2021 & start <= 2019) %>%
          dplyr::slice_min(order_by = distance)
        # .$station_id)
        
        print(paste("For loc:", GD_Aug[i,]$Latitude,"x", GD_Aug[i,]$Longitude, " the nearest station ", "(",weathTab$station_name,")", " is ",weathTab$distance, "km away.") )
        allstnbyOWL[i] <- as.vector(as.numeric(weathTab$station_id))
      }
      
      OWLSTN <- cbind(GD_Aug, allstnbyOWL) #Add a column with the nearest stationID to each location
      
      #To minimize the magnitude and frequency of downloads from the EC server, combine the 
      #results of the weather stations and time periods that are required and then call dl as a bundle
      RequiredStationData <- OWLSTN %>% #Returns a df with all unique stations-day (and min/max datetime) to be used for calling WeatherCan station dl
        dplyr::group_by(allstnbyOWL, lubridate::as_date(NewDate)) %>%
        dplyr::summarise(start = min(NewDate), end= max(NewDate), n=n())
      
      #To reduce the number of redundant calls to Weather Canada, build a table of station data
      #Create a single weather data table that can be used for the entire dataset
      
      dl <- list() #List that will hold each daily weather table for each unique station
      
      for(j in 1:nrow(RequiredStationData)){  #Loop through each unique station-day from the dataset and pull the weather table
        dl_inst <- weathercan::weather_dl(station_ids = OWLSTN[j,]$allstnbyOWL,
                                          interval = "hour",
                                          start=as.Date(OWLSTN[j,]$NewDate),
                                          end=as.Date(OWLSTN[j,]$NewDate))
        dl[[j]] <- as.data.frame(dl_inst)
      }
      station_results <- as.data.frame(do.call("rbind", dl))%>%
        dplyr::mutate(MATCHHR = lubridate::hour(time)) %>%
        dplyr::mutate(DMY_M= lubridate::as_date(x=time))#This combines the chunks of daily data for each station that will be used to match temps
      
      #Create a table with owl observations and the associated weather information
      OWLWEATH <- OWLSTN %>% 
        dplyr::mutate(MatchDate = lubridate::round_date(NewDate,unit="hour"))%>%
        dplyr::mutate(MatchHour = lubridate::hour(MatchDate)) %>%
        dplyr::mutate(DMY_M = lubridate::as_date(NewDate))%>%#format(strptime((MatchDate,"#H:%M:%S"),'%H:%M')) ) %>% #Need to link stationID, date to the nearest time from GD_Aug versus hourly from station_results
        dplyr::left_join(.,station_results, by=c("allstnbyOWL"="station_id", "DMY_M"="DMY_M","MatchHour"="MATCHHR"))
      
    }
    
    #### Initiate weather sequence if the checkbox for weather is checked    ####  
    if(input$Solar){
      
      OWLWEATH <- OWLWEATH %>%
        # dplyr::left_join(., )
        # getMoonTimes(data.frame(data=., date=as.Date(.$MatchDate), lat=.$Latitude, lon=.$Longitude)) %>% 
        # getMoonTimes(data=.) %>%
        cbind(.,
              # suncalc::getMoonTimes(data=data.frame(date=as.Date(OWLWEATH$MatchDate), lat=OWLWEATH$Latitude, lon=OWLWEATH$Longitude))  %>% select(rise, set) %>% rename(m_rise = rise, m_set=set),
              suncalc::getSunlightPosition(data=data.frame(date=as.Date(OWLWEATH$MatchDate), lat=OWLWEATH$Latitude, lon=OWLWEATH$Longitude)) %>% select(altitude, azimuth) %>% rename(sunp_alt=altitude, sunp_azi=azimuth),
              #suncalc::getMoonPosition(data=data.frame(date=as.Date(OWLWEATH$MatchDate), lat=OWLWEATH$Latitude, lon=OWLWEATH$Longitude)) %>% select(altitude, azimuth, distance, parallacticAngle) %>% rename(mp_alt=altitude, mp_azi=azimuth, mp_dist=distance, mp_angle=parallacticAngle),
              suncalc::getSunlightTimes(data=data.frame(date=as.Date(OWLWEATH$MatchDate), lat=OWLWEATH$Latitude, lon=OWLWEATH$Longitude))  %>% select(sunrise, sunriseEnd, solarNoon, goldenHour, sunsetStart, sunset, dusk, nauticalDusk, night, nadir, nightEnd, nauticalDawn, dawn),
              # suncalc::getMoonIllumination(date=as.Date(OWLWEATH$MatchDate))  %>% select(fraction, phase, angle) %>% rename(m_fract = fraction, m_phz=phase, m_ang=angle)
              
        )#add columns from MoonTimes
      
      
    }
    
    #### Initiate weather sequence if the checkbox for weather is checked    ####  
    if(input$Lunar){
      
      OWLWEATH <- OWLWEATH %>%
        # dplyr::left_join(., )
        # getMoonTimes(data.frame(data=., date=as.Date(.$MatchDate), lat=.$Latitude, lon=.$Longitude)) %>% 
        # getMoonTimes(data=.) %>%
        cbind(.,
              suncalc::getMoonTimes(data=data.frame(date=as.Date(OWLWEATH$MatchDate), lat=OWLWEATH$Latitude, lon=OWLWEATH$Longitude))  %>% select(rise, set) %>% rename(m_rise = rise, m_set=set),
              #  suncalc::getSunlightPosition(data=data.frame(date=as.Date(OWLWEATH$MatchDate), lat=OWLWEATH$Latitude, lon=OWLWEATH$Longitude)) %>% select(altitude, azimuth) %>% rename(sunp_alt=altitude, sunp_azi=azimuth),
              suncalc::getMoonPosition(data=data.frame(date=as.Date(OWLWEATH$MatchDate), lat=OWLWEATH$Latitude, lon=OWLWEATH$Longitude)) %>% select(altitude, azimuth, distance, parallacticAngle) %>% rename(mp_alt=altitude, mp_azi=azimuth, mp_dist=distance, mp_angle=parallacticAngle),
              # suncalc::getSunlightTimes(data=data.frame(date=as.Date(OWLWEATH$MatchDate), lat=OWLWEATH$Latitude, lon=OWLWEATH$Longitude))  %>% select(sunrise, sunriseEnd, solarNoon, goldenHour, sunsetStart, sunset, dusk, nauticalDusk, night, nadir, nightEnd, nauticalDawn, dawn),
              suncalc::getMoonIllumination(date=as.Date(OWLWEATH$MatchDate))  %>% select(fraction, phase, angle) %>% rename(m_fract = fraction, m_phz=phase, m_ang=angle)
              
        )
    }
    

    
    
   
})

        

output$table <- renderTable({
        OWL()$OWLWEATH
      })

# tableOutput(OWLWEATH)
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$OWLWEATH, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(OWLWEATH, file, row.names = FALSE)
    }
  )
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
