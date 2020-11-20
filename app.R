

library(shiny)
library(markdown)
library(ggplot2)
library(dplyr)
library(sf)
library(leaflet)



#####  Prepare Data for First Tab Panel #########################
### Load the Data
ContactManagement311 <- read.csv("311_Contact_Management_Cases.csv")


# prepare for list of years and months
years <- sort(unique(ContactManagement311$Entry_Year))
months <- sort(unique(ContactManagement311$Entry_Month))
# neeed to bin counts of calls by category by month and year
Contact <- ContactManagement311 %>%
  select(Entry_Month, Entry_Year, Work_Group_Description)
#####  End Prepare Data for First Tab Panel #########################

#####  Prepare Data for Second Tab Panel #########################
# load data of school boundaries and public facilities
myDist <- st_read("City_Council_Districts.shp") 
public.points <- read.csv("Public_Facilities.csv")

public.spatial <- public.points %>% 
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(st_crs(myDist))


#types <- sort(unique(public.points$POPL_TYPE))
types <- c("All", sort(unique(public.points$POPL_TYPE)))
#public.spatial$POPL_TYPE <- as.factor(public.spatial$POPL_TYPE)
#####  End Prepare Data for Second Tab Panel #########################

#####  Prepare Data for Third Tab Panel #########################
codes <- read.csv('Code_Enforcement_Cases.csv')

codes_agg <- codes %>%
  filter(Case_Year != 14) %>%
  mutate(Case_Year = Case_Year + 2000) %>%
  mutate(Case_Month = Case_Month) %>%
  group_by(Case_Year, Case_Month, Case_Type_Code_Description) %>%
  summarize(violations = n())

years <- sort(unique(codes_agg$Case_Year))
months <- sort(unique(codes_agg$Case_Month))
#####  End Prepare Data for Third Tab Panel #########################

#####  End Prepare Data for Fourth Tab Panel #########################
ab <- st_read("Abandoned_Property_Parcels.shp")
outcome <- sort(unique(ab$Outcome_St))
#####  End Prepare Data for Fourth Tab Panel #########################

# Define UI for application that draws a histogram
ui <- fluidPage( 
    navbarPage("Group 1",
                 
################################ 1st Person Start ###############     
      # First Tab Panel
     tabPanel("311 Trends",

     # Sidebar with a slider input for number of bins 
        headerPanel("311 Trends"),
        
             
          # Show a plot of the generated distribution
          mainPanel(
              plotOutput("circBarplot"),
              fluidRow(
                 #selectInput("year", 'Year:', years),
                 #selectInput("month", 'Month:', months)
                 column(3, 
                    sliderInput("year", "Year:",
                              min = 2013,
                              max = 2016,
                              value = c(2014,2015)),
                 ),  
                 column(4, offset = 1,   
                    sliderInput("month", "Month:",
                              min = 1,
                              max = 12,
                              value = c(1,6)),
                 )
             )
             
             )
         
     ),
################################ 1st Person Stop ############### 
################################ 3rd Person Start ###############            
# Third Tab Panel
tabPanel("Code Violations By Month/Year",
         
         # Application title
         titlePanel("Code Violations By Month/Year"),
         
         # Sidebar with a select input for number of bins 
         sidebarLayout(
           sidebarPanel(
             sliderInput("year2", 'Select Year:', min = 2008, max = 2013, value = c(2008, 2013)),
             sliderInput("month2", 'Select Month:', min = 1, max = 12, value = c(1,12)),
           ),
           
           
           mainPanel(
             plotOutput("distPlot")
           )
         )
),
################################ 3rd Person Stop ###############     

################################ 2nd Person Start ############### 
     # Second Tab Panel
     tabPanel("Public Facilities in Districts",
     
              # Application title
              titlePanel("Public Facilities in Districts"),
              
              # Sidebar with a selection input for the different facility types
              sidebarLayout(
                sidebarPanel(
                  selectInput("types", 'Facility Type:', types),
                  selectInput("district", 'District:', c("All", myDist$Dist)),
                ),
                
                # Show a plot of the generated map
                mainPanel(
                  leafletOutput(outputId = "mymap", height = 600)
                )
              )
     ),
################################ 2nd Person Stop ###############       


  
################################ 4 th Person Start ###############        
     # Fourth Tab Panel
     tabPanel("Map of Abandoned Parcels",
              
              # Application title
              titlePanel("Map of Abandoned Parcels"),
              
              # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput(inputId = "outcome",
                                     label = "Abandoned Property Outcome",
                                     choices = outcome,
                                     selected = outcome, )
                ),
                
                # Show a plot of the generated distribution
                mainPanel(leafletOutput(
                  outputId = "mymap2")
                )#end tabset panel
              )
     )
################################ 4th Person Stop ###############     
  ) # End navPage

) # End fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
################################ 1st Person Start ###############
  
  
   # Make the plot
   output$circBarplot <- renderPlot({
    
     ContactSmall <-Contact %>% filter(Entry_Month >= input$month) %>%
                                filter(Entry_Month <= input$month)%>%
                                filter(Entry_Year >= input$year) %>%
                                filter(Entry_Year <= input$year) %>%
                                group_by(Work_Group_Description) %>% 
                                summarise(Work_Order_Count = n())
     
     # lets go ahead and assign categorie to different 311 types
     ContactSmall$Categories <- factor(ContactSmall$Work_Group_Description, ordered = TRUE)
     levels(ContactSmall$Categories) <- list(Utilities = "Water Works",
                                        Utilities = "Solid Waste",
                                        Utilities = "Sewer Department",
                                        Safety = "Police Department",
                                        Safety = "Fire Department",
                                        Safety = "Animal Control",
                                        Safety = "Traffic and Lighting",
                                        Safety = "Code Enforcement",
                                        Safety = "Mayors 311",
                                        CityServices = "Parks & Recreation",
                                        CityServices = "Engineering",
                                        CityServices = "Public Works",
                                        CityServices = "Building Department",
                                        CityServices = "Street Department",
                                        CityServices = "Park Forestry",
                                        CityServices = "Community Investment",
                                        Admin = "Mayor's Office",
                                        Admin = "County Offices",
                                        Admin = "Admin & Finance",
                                        Admin = "Organic Resources",
                                        Admin = "Legal Department",
                                        Admin = "Clerks Office",
                                        Other = "",
                                        Other = "Other",
                                        Other = "Morris Performing Arts Center"
                                        )
     
     # now lets sort back by categories
     # ContactSmall <- ContactSmall %>% arrange(Categories)
     
     # Set a number of 'empty bar' to add at the end of each group
     
     empty_bar <- 0
     to_add <- data.frame( matrix(NA, empty_bar*nlevels(ContactSmall$Categories), ncol(ContactSmall)) )
     colnames(to_add) <- colnames(ContactSmall)
     to_add$Work_Group_Description <- rep(levels(ContactSmall$Categories), each=empty_bar)
     ContactSmall <- rbind(ContactSmall, to_add)
     ContactSmall <- ContactSmall %>% arrange(Categories)
     ContactSmall$id <- seq(1, nrow(ContactSmall))
     
     # Get the name and the y position of each label
     label_data <- ContactSmall
     number_of_bar <- nrow(label_data)
     angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     #
     label_data$hjust <- ifelse( angle < -90, 1, 0)
     label_data$angle <- ifelse(angle < -90, angle+180, angle)
     
     
      ggplot(ContactSmall, aes(x=as.factor(id), y=Work_Order_Count, fill=Categories)) +      
        geom_bar(stat="identity", alpha=1) +
        ylim(-100,120) +
        theme_minimal() +
        theme(
          #legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "in") 
        ) +
        coord_polar() + 
        geom_text(data=label_data, aes(x=id, y=Work_Order_Count+10, label=Work_Group_Description, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )
    },
    width = 700,
    height = 400,
    res = 72)
   
################################ 1st Person Stop ###############

################################ 3rd Person Start ############### 
   output$distPlot <- renderPlot({
     ggplot(codes_agg %>% 
              filter(Case_Year>=input$year2[1]) %>%
              filter(Case_Year<=input$year2[2]) %>%
              filter(Case_Month>=input$month2[1]) %>%
              filter(Case_Month<=input$month2[2]), 
            aes(y=Case_Type_Code_Description, x=violations, fill=Case_Type_Code_Description)) + 
       geom_bar(stat="identity") + 
       labs(y="Code Type", x="# of Violations") +
       theme(legend.position = 'none')
   })
################################ 3rd Person Stop ###############   
   
################################ 2nd Person Start ###############
   pal <- colorFactor(c("red", "orange", "navy"), domain = c("POLICE STATION", "FIRE STATION", "LIBRARY"))  
   my.data <- reactive({
     if (input$types == "All" & input$district == "All"){
       rtrn <- leaflet()  %>%
         addTiles()  %>%
         addPolygons(data = myDist) %>%
         addCircleMarkers(data = public.spatial[public.spatial$FID %in% as.numeric(unlist(st_intersects(myDist, public.spatial))), ] ,
                          popup = ~POPL_TYPE, color = ~pal(POPL_TYPE), stroke = FALSE, fillOpacity = 0.75)
       
     }else{
       if(input$types == "All" & input$district != "All"){
         
         filterDist <- myDist %>% 
           filter(Dist == input$district)
         
         rtrn <- leaflet()  %>%
           addTiles()  %>%
           addPolygons(data = filterDist) %>%
           addCircleMarkers(data = public.spatial[public.spatial$FID %in% st_intersects(filterDist, public.spatial)[[1]], ] ,
                            popup = ~POPL_TYPE, color = ~pal(POPL_TYPE), stroke = FALSE, fillOpacity = 0.75)
         
       }else{
         if(input$types != "All" & input$district == "All"){
           
           rtrn <- leaflet()  %>%
             addTiles()  %>%
             addPolygons(data = myDist) %>%
             addCircleMarkers(data = public.spatial[public.spatial$FID %in% as.numeric(unlist(st_intersects(myDist, public.spatial))), ] %>% 
                                filter(POPL_TYPE == input$types),
                              popup = ~POPL_TYPE, color = ~pal(POPL_TYPE), stroke = FALSE, fillOpacity = 0.75)
           
         }else{
           
           filterDist <- myDist %>% 
             filter(Dist == input$district)
           
           rtrn <- leaflet()  %>% 
             addTiles()  %>%
             addPolygons(data = filterDist) %>%
             addCircleMarkers(data = public.spatial[public.spatial$FID %in% st_intersects(filterDist, public.spatial)[[1]], ] %>% 
                                filter(POPL_TYPE == input$types),
                              popup = ~POPL_TYPE, color = ~pal(POPL_TYPE), stroke = FALSE, fillOpacity = 0.75)
         }
         
       }
     }
     return(rtrn)
   })
   
   output$mymap <- renderLeaflet({
     my.data()
   })
################################ 2nd Person Stop ############### 
 
################################ 4th Person Start ############### 
    # when the slider changes update the text box
    observe({
      output$mymap2 <- renderLeaflet({
        myDist <- ab %>% filter(Outcome_St == input$outcome)
        leaflet() %>% 
          addTiles() %>%
          addPolygons(data = myDist, popup = ~geometry)
      })
    })
################################ 4th Person Stop ############### 
}

# Run the application 
shinyApp(ui = ui, server = server)
