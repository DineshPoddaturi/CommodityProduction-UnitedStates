### Agriculture production in dollars by state and by commodity in the US
require(tidyverse)
require(readxl)
require(lubridate)
# require(gganimate)
# require(magick)
# require(gifski)
require(plotly)
require(shinythemes)
require(maps)


#### reading data
agProd <- read_excel("Data/AgProductionDollars.xlsx") %>% as.data.frame()

#### Selecting the required columns from the dataframe
agProdSub <- agProd %>% select(Year, State, Commodity, `Data Item`, Value)

#### Here we make some modifications to the column `Data Item`. We remove some redundent characters
agProdSub$`Data Item` <- gsub(" - PRODUCTION, MEASURED IN $", replacement = "", agProdSub$`Data Item`, fixed = TRUE)
agProdSub$`Data Item` <- gsub(", ", replacement = "_", agProdSub$`Data Item`, fixed = TRUE)
agProdSub$`Data Item` <- gsub("_(", replacement = "(", agProdSub$`Data Item`, fixed = TRUE)
# agProdSub$`Data Item` <- gsub(")", replacement = "", agProdSub$`Data Item`, fixed = TRUE)
# agProdSub$`Data Item` <- gsub(" ", replacement = "-", agProdSub$`Data Item`, fixed = TRUE)

#### The following will give use the data type of each column. 
# agProdSub %>% glimpse()

#### I observed that the Value column is of chr type. So we change it to numeric as it represents the production in dollar amount
agProdSub$Value <- as.numeric(agProdSub$Value)

agProdSub$Year <- as.numeric(agProdSub$Year)

# agProdSub$Year <- year(as.Date(as.character(agProdSub$Year), format = "%Y"))

agProdSub <- agProdSub %>% drop_na()

#### I change some of the column names to be more general. 
agProduction <- agProdSub %>% transmute(Year, State, Commodity, CommoditySub = `Data Item`, Production = Value) %>% 
    filter(Year >=1980) %>% filter(Production >= 10000000) %>% select(-CommoditySub)

agProductionYears <- agProduction %>% distinct(Year)

#### The following will give us the distinct commodities that appear in the data
commodity <- sort(unique(agProduction$Commodity))

# years <- unique(agProduction$Year)


#### Here I write the function that gives me the sub commodities 
subCommodity <- function(com){
    
    subCom <- agProduction %>% group_by(Commodity) %>% distinct(CommoditySub)
    
    subCom <- unique(subCom$CommoditySub[subCom$Commodity %in% com])
    
    return(subCom)
}

#### Here I write the function that gives me the states that produce/produced the commodity 
stateCommodity <- function(com){
    
    states <- state.name
    
    stateProd <- agProduction %>%
        group_by(Commodity)  %>% distinct(State)
    
    stateProd <- unique(stateProd$State[stateProd$Commodity %in% com])
    
    stateProd <- sort(stateProd)
    
    return(stateProd)
}


##### Here I get the years by commodity
yearCommodity <- function(dataSelected){
    
    yearProd <- dataSelected %>% distinct(Year)
    
    yearProd <- unique(yearProd$Year)
    
    yearProd <- sort(yearProd, decreasing = TRUE)
    
    return(yearProd)
}


#### Here I write the trends function. This will accept commodity name, and returns the production in dollars
trendsCommodityNation <- function(com){
    
    subCom <- agProduction %>% filter(Commodity %in% com) %>% 
        drop_na() %>% select(Year, Commodity, Production) %>% arrange(Year)
    
    subCom <- subCom %>% group_by(Year) %>% mutate(Production = sum(Production)) %>% as.data.frame() %>% distinct()
    
    subCom <- subCom %>% mutate(Production = Production/1000000)
    
    
    return(subCom)
}


#### The following function accepts the selected commodity, year and returns the data contaning he productoin of the selected items and 
#### latitude and longitude for plotting the US map
statesMapData <- function(com, yr){
    
    commData <- agProduction %>% filter(Commodity %in% com & Year %in% yr) %>% 
        drop_na() %>% select(Year, State, Commodity, Production)
    commData <- commData %>% group_by(State) %>% mutate(Production = sum(Production)) %>% as.data.frame() %>% distinct()
    commData$region <- tolower(commData$State)
    commData <- commData %>% mutate(Production = Production/1000000)
    
    all_states <- map_data("state")
    
    completeDataSet <- left_join(all_states, commData,by="region") %>% 
        mutate(Production = if_else(is.na(Production), 0, Production))
    
    return(completeDataSet)
}


## The following function accepts the selected commodity, year and returns the top ten states in terms of production of that corresponding 
## commodity in that specific year
topTenStates <- function(com, yr){
    
    commData <- agProduction %>% filter(Commodity %in% com & Year %in% yr) %>% 
        drop_na() %>% select(Year, State, Commodity, Production)
    
    topTen <- commData %>% group_by(State) %>% mutate(Production = sum(Production)/1000000) %>% arrange(desc(Production)) %>% 
        as.data.frame() %>% distinct() %>% head(10)
    
    topTen$State <- factor(topTen$State,levels = topTen$State[order(topTen$Production)])
    
    return(topTen)
}


# Define UI for Socioeconomic Indicators - Unites States
ui <- fluidPage(
    
    theme = shinytheme("darkly"),
    # shinythemes::themeSelector(),
    
    # Application title
    titlePanel("Commodity Production (in million dollars) - United States"),
    
    sidebarPanel(
        selectInput("Commodity",label="Commodity", choices = commodity),
        uiOutput("year")
    ),
    
    # Showing all the plots
    mainPanel(
        tabsetPanel(
            tabPanel("Trends",plotlyOutput("Trends")),
            tabPanel("Nationwide",plotlyOutput("Nationwide")),
            tabPanel("Top Producing States",plotlyOutput("TopStates"))
        )
    )
)



# Defining server logic 
server <- function(input, output) {
    
    datasetInput <- reactive({
        
        selectedCommodity <- input$Commodity
        
        data <- agProduction %>% filter(Commodity == selectedCommodity)
        
        data
        
    })
    
    output$year <- renderUI({
        
        dataSelected <- datasetInput()
        yearsAvail <- yearCommodity(dataSelected)
        
        selectInput("Year",label="Year",choices = yearsAvail ,selected=yearsAvail[1])
        
    })
    
    output$Trends <- renderPlotly({
        
        selectedCommodity <- input$Commodity
        
        dataTrends <- trendsCommodityNation(com = selectedCommodity)
        
        fromYear <- dataTrends$Year[1]
        toYear <- dataTrends$Year[nrow(dataTrends)]
        brks <- c(seq(fromYear, toYear))
        
        plot <- dataTrends %>% ggplot(aes(x = Year, y = Production)) + geom_point(color = "firebrick") +
            geom_path(color = "firebrick") + scale_x_continuous(name = "Year", breaks = brks) +  
            scale_y_continuous(name = "Production (in Million $)") +theme_bw()+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(plot,height = 400)
        
    })
    
    output$Nationwide <- renderPlotly({
        
        selectedCommodity <- input$Commodity
        
        selectedYear <- input$Year
        
        completeDataSet <- statesMapData(com = selectedCommodity, yr = selectedYear)
        
        plot <- completeDataSet %>% ggplot(aes(x = long, y = lat, group = region)) +
            geom_polygon(aes(fill = Production), color="grey")  +
            scale_fill_gradient2(name=selectedCommodity,low = "lightgreen", high = "darkgreen") +
            ggthemes::theme_map() + theme(legend.position = c(0.8, 0))
        
        ggplotly(plot,height = 400)
        
    })
    
    output$TopStates <- renderPlotly({
        
        selectedCommodity <- input$Commodity
        
        selectedYear <- input$Year
        
        orderedData <- topTenStates(com = selectedCommodity, yr = selectedYear)
        
        plot <- orderedData %>% ggplot(aes(x=State,y=Production,fill=State))+
            geom_bar(stat="identity") + scale_y_continuous(name = "Production (in Million $)") + coord_flip() + labs(x="State") + theme_bw()
        
        ggplotly(plot,height = 400)
        
    })
    
    
}

# Bind ui and server together
shinyApp(ui, server)
