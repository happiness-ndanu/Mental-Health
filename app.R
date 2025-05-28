library(shiny)
library(tidyverse)
#library(summarytools)
#library(glue)
#library(patchwork)
library(readxl)
library(here)
library(janitor)
#library(graphics)
library(paletteer)
#library(lubridate)
library(sf)
#library(tmap)
library(leaflet)
#library(wesanderson)
#names(wes_palettes)
#library(rsconnect)

#Load Excel File
#here::i_am("MentalHealth/app.R")
mentalhealth<-read_excel("mental health records.xlsx") %>%  
  clean_names()
# mentalhealth<-read_excel("D:/Mental Health/mental health records.xlsx") %>%  
#   clean_names()
#Detect Missing Values
anyNA(mentalhealth)
#Sum of Missing Values
colSums(is.na(mentalhealth))
#What is the age distribution of most MH patients?

#Create the show distribution function
#generating the show distribution function

# show_distribution <- function(var_data, binwidth) {
#   
#   # Get summary statistics by first extracting values from the column
#   min_val <- min(pull(var_data))
#   max_val <- max(pull(var_data))
#   mean_val <- mean(pull(var_data))
#   med_val <- median(pull(var_data))
#   mod_val <- statip::mfv(pull(var_data))
#   
#   # Print the stats
#   stats <- glue::glue(
#     "Minimum: {format(round(min_val, 2), nsmall = 2)}
#    Mean: {format(round(mean_val, 2), nsmall = 2)}
#    Median: {format(round(med_val, 2), nsmall = 2)}
#    Mode: {format(round(mod_val, 2), nsmall = 2)}
#    Maximum: {format(round(max_val, 2), nsmall = 2)}"
#   )
#   
#   theme_set(theme_light())
#   # Plot the histogram
#   hist_gram <- ggplot(var_data) +
#     geom_histogram(aes(x = pull(var_data)), binwidth = binwidth,
#                    fill = "midnightblue", alpha = 0.7, boundary = 0.4) +
#     
#     # Add lines for the statistics
#     geom_vline(xintercept = min_val, color = "gray33",
#                linetype = "dashed", size = 1.3) +
#     geom_vline(xintercept = mean_val, color = "cyan",
#                linetype = "dashed", size = 1.3) +
#     geom_vline(xintercept = med_val, color = "red",
#                linetype = "dashed", size = 1.3) +
#     geom_vline(xintercept = mod_val, color = "yellow",
#                linetype = "dashed", size = 1.3) +
#     geom_vline(xintercept = max_val, color = "gray33",
#                linetype = "dashed", size = 1.3) +
#     
#     # Add titles and labels
#     ggtitle("Data Distribution") +
#     xlab("") +
#     ylab("Frequency") +
#     theme(plot.title = element_text(hjust = 0.5))
#   
#   # Plot the box plot
#   bx_plt <- ggplot(data = var_data) +
#     geom_boxplot(mapping = aes(x = pull(var_data), y = 1),
#                  fill = "#E69F00", color = "gray23", alpha = 0.7) +
#     
#     # Add titles and labels
#     xlab("Value") +
#     ylab("") +
#     theme(plot.title = element_text(hjust = 0.5))
#   
#   
#   # To return multiple outputs, use a `list`
#   return(
#     
#     list(stats,
#          hist_gram / bx_plt)) # End of returned outputs
# }

#Calculate The Age Distribution of Patients
ageMH<- mentalhealth %>%
  select(age)
# show_distribution(var_data = ageMH, binwidth = 10)

#A Visual Presentation of The Count of Different Age-Groups of Mental Health Patients
newMH <- mentalhealth %>% 
  mutate(age=as.numeric(as.character(age)))

newMH <- newMH %>% 
  mutate(age= case_when(
    age <= 12~"child",
    age >=13 & age<=19~"teenager",
    age>=20 & age<=35~"youth",
    age>=36~"adult"
  ))
# #MAKE GEOM BAR
# newMH %>% 
#   ggplot(aes(x=age))+
#   geom_bar(mapping = aes(fill=age),alpha= 0.7,data = NULL,stat = "count",position = "stack",na.rm = FALSE,orientation = NA, show.legend = NA,inherit.aes = TRUE)+
#   
#   labs(title = "COUNT OF DIFFERENT AGE GROUPS OF TB PATIENTS", x="AGE GROUPS", y="COUNT")+ 
#   theme(plot.background = element_rect(fill = "white"))+
#   paletteer::scale_fill_paletteer_d("ggsci::default_nejm")

#Which Sub-County Projected Many Cases of Mental Illness?
subcountycount <-newMH %>% 
  count(sub_county, sort = TRUE)
subcountycount

#Visual Representation of Number of Mental Illness Cases in the First Top Ten Sub-Counties
#making a geom col
# newMH %>% 
#   count(sub_county, sort = TRUE) %>% 
#   #make county a factor
#   mutate(sub_county=factor(sub_county)) %>% 
#   #rearrange in descending order
#   mutate(sub_county= fct_reorder(sub_county, n, .desc = TRUE)) %>% 
#   slice_head(n=10) %>% 
#   ggplot(mapping = aes(x= sub_county, y= n))+
#   geom_col(
#     aes(fill = sub_county), show.legend = FALSE) +
#   ggtitle("COUNT OF NUMBER OF MENTAL ILLNESS IN THE FIRST TOP 10 SUB-COUNTIES")+
#   xlab("SUB-COUNTIES")+
#   ylab("CASES COUNT")+
#   geom_text(aes(label = n))+
#   paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
#   theme( 
#     #rotate the x and y axis to make them eligible
#     plot.title = element_text(hjust = 0.5),
#     axis.text.x = element_text(angle = 90),
#     axis.text.y = element_text(angle = 0))

#Most Prevalent Type of Mental Illness in the Sub-County With the Highest Cases
diagnosiscount <-newMH %>%
  select(diagnosis, sub_county) %>% 
  filter(sub_county=="Thika") %>% 
  count(diagnosis, sort = TRUE) 

# diagnosiscount%>%  
#   #make county a factor
#   mutate(diagnosis=factor(diagnosis)) %>%
#   #rearrange in descending order
#   mutate(diagnosis= fct_reorder(diagnosis, n, .desc = TRUE)) %>%
#   slice_head(n=10) %>% 
#   ggplot(mapping = aes(x= diagnosis, y= n))+
#   geom_col(
#     aes(fill = diagnosis), show.legend = FALSE) +
#   ggtitle("COUNT OF TOP 10 MOST PREVALENT TYPE OF MENTAL ILLNESS IN THE SUB-COUNTY WITH HIGHEST CASES(THIKA)")+
#   xlab("DIAGNOSIS")+
#   ylab("CASES COUNT")+
#   geom_text(aes(label = n))+
#   paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
#   theme( 
#     #rotate the x and y axis to make them eligible
#     plot.title = element_text(hjust = 0.5),
#     axis.text.x = element_text(angle = 90),
#     axis.text.y = element_text(angle = 0))

#What Are The Type of Patients Recorded in the Top Ten Sub-Counties?
patientstypecount <-newMH %>% 
  select(type_of_patient, sub_county) %>% 
  count(sub_county,type_of_patient, sort = TRUE)

# patientstypecount %>% 
#   #make sub_county a factor
#   mutate(sub_county=factor(sub_county)) %>% 
#   #rearrange in descending order
#   mutate(sub_county= fct_reorder(sub_county, n, .desc = TRUE)) %>% 
#   slice_head(n=16) %>%
#   #making a geom bar
#   ggplot(mapping = aes(x=sub_county, y= n),)+
#   geom_bar(
#     stat="identity",aes(fill=type_of_patient),alpha=0.9)+
#   #add title
#   ggtitle("VISUAL REP. OF TYPE OF PATIENTS IN DIFF. SUB-COUNTIES")+
#   xlab("SUB-COUNTIES")+
#   ylab("TYPE OF PATIENT COUNT")+
#   geom_text(aes(label = n))+
#   paletteer::scale_fill_paletteer_d("ggthemes::wsj_red_green")+
#   theme(
#     #center the title
#     plot.title = element_text(hjust = 0.5),
#     #add a grid to the bars
#     panel.grid = element_blank(),
#     panel.grid.major.y= element_line(color = "gray",linetype = "dashed",size = 0.5),
#     
#     #rotate the x and y axis to make them eligible
#     axis.text.x = element_text(angle = 90),
#     axis.text.y = element_text(angle = 0))
# 
# #Counties rep. in the Various Mental Illnesses Recorded
# countycount <-newMH %>% 
#   count(county, sort = TRUE)
# countycount
# 
# #Visual Representation of Number of Mental Illness Cases in the Counties
# #making a geom col
# newMH %>% 
#   count(county, sort = TRUE) %>% 
#   #make county a factor
#   mutate(county=factor(county)) %>% 
#   #rearrange in descending order
#   mutate(county= fct_reorder(county, n, .desc = TRUE)) %>% 
#   ggplot(mapping = aes(x= county, y= n))+
#   geom_col(
#     aes(fill = county), show.legend = FALSE) +
#   ggtitle("COUNT OF NUMBER OF MENTAL ILLNESS IN THE COUNTIES")+
#   xlab("COUNTIES")+
#   ylab("CASES COUNT")+
#   geom_text(aes(label = n))+
#   paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
#   theme( 
#     #rotate the x and y axis to make them eligible
#     plot.title = element_text(hjust = 0.5),
#     axis.text.x = element_text(angle = 90),
#     axis.text.y = element_text(angle = 0))

# #DIAGNOSIS COUNT
# plotdia <- topdiagnosiscount <-newMH %>%
#   select(diagnosis, sub_county) %>% 
#   count(diagnosis, sort = TRUE) %>% 
#   #making a geom col
#   #make county a factor
#   mutate(diagnosis=factor(diagnosis)) %>%
#   #rearrange in descending order
#   mutate(diagnosis= fct_reorder(diagnosis, n)) %>%
#   slice_head(n=12) %>% 
#   ggplot(aes(x=n, y= diagnosis, fill= diagnosis))+
#   geom_col(show.legend = FALSE)+
#   ggtitle("COUNT OF THE TOP MENTAL HEALTH CASES RECORDED")+
#   xlab("COUNT")+
#   ylab("DIAGNOSIS")+
#   geom_text(aes(label=n),
#             hjust=1,nudge_x = -.5,
#             size = 3, fontface = "bold", family = "Fira Sans")+
#   paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
#   #scale_fill_manual(values = pal, guide = "none") +
#   theme_minimal()
# plotdia



#read in the kenyan shapefiles



#subcountySHP <- read_sf("currentshapefiles/Sub-Counties.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)

#subcountySHP <- read_sf("currentshapefiles/Sub-Counties.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)

subcountySHP <- readRDS("subcountySHP")

# View(subcountySHP %>% st_drop_geometry())

#inspect a few rows
print(subcountySHP[6:9], n = 3)

#inspect column names
colnames(subcountySHP)

#inspect the class shape files
class(subcountySHP)

#Look at the variable data types
glimpse(subcountySHP)

#View the geometry column
subcountySHP_geometry <- st_geometry(subcountySHP)
# View one geometry entry
subcountySHP_geometry[[1]]

#Geometry columns have their own class
class(subcountySHP) #sfc, the list-column with the geometries for each feature
#> [1] "sfc_MULTIPOLYGON" "sfc"

class(subcountySHP[[1]]) #sfg, the feature geometry of an individual simple feature
#> [1] "XY"           "MULTIPOLYGON" "sfg"

### This line is not necessary since the shapefile is already in the WGS 84 projection.

subcountySHP <- st_transform(subcountySHP, crs = 4326)

### Inspect the co-ordinate reference system
st_crs(subcountySHP)
# Coordinate Reference System:
#   User input: EPSG:4326 
# wkt:
#   GEOGCRS["WGS 84",
#           ENSEMBLE["World Geodetic System 1984 ensemble",
#                    MEMBER["World Geodetic System 1984 (Transit)"],
#                    MEMBER["World Geodetic System 1984 (G730)"],
#                    MEMBER["World Geodetic System 1984 (G873)"],
#                    MEMBER["World Geodetic System 1984 (G1150)"],
#                    MEMBER["World Geodetic System 1984 (G1674)"],
#                    MEMBER["World Geodetic System 1984 (G1762)"],
#                    MEMBER["World Geodetic System 1984 (G2139)"],
#                    ELLIPSOID["WGS 84",6378137,298.257223563,
#                              LENGTHUNIT["metre",1]],
#                    ENSEMBLEACCURACY[2.0]],
#           PRIMEM["Greenwich",0,
#                  ANGLEUNIT["degree",0.0174532925199433]],
#           CS[ellipsoidal,2],
#           AXIS["geodetic latitude (Lat)",north,
#                ORDER[1],
#                ANGLEUNIT["degree",0.0174532925199433]],
#           AXIS["geodetic longitude (Lon)",east,
#                ORDER[2],
#                ANGLEUNIT["degree",0.0174532925199433]],
#           USAGE[
#             SCOPE["Horizontal component of 3D system."],
#             AREA["World."],
#             BBOX[-90,-180,90,180]],
#           ID["EPSG",4326]]

# #Load the data that we are going to map
#View(newMH)

#Clean the data, so that the sub-counties match those in the shapefile.

### Inspect the sub-county names of the disability data
subcounties_df <- unique(newMH$sub_county)

### Inspect the sub-county names of the shape file
subcounties_KenyaSHP <- subcountySHP %>% 
  st_drop_geometry() %>% 
  select(NAME_2) %>% 
  pull() %>%
  unique()
# glimpse(subcounties_KenyaSHP)

### Convert the MH sucounty names to title case
mh_df <- newMH %>% 
  ungroup() %>% 
  mutate(sub_county = tools::toTitleCase(tolower(sub_county)))

### Inspect the subcounty names of the TB data again 
subcounties_mh_df <- unique(mh_df$sub_county)

### Inspect the subcounty names that are different in each of the datasets
unique(mh_df$sub_county)[which(!unique(mh_df$sub_county) %in% subcounties_KenyaSHP)]


### Clean the county names so that they match in both datasets
mh_df <- mh_df %>% 
  mutate(sub_county = ifelse(sub_county == "Machakos", "Machakos Town",
                             ifelse(sub_county == "Dagoreti", "Dagoretti North",
                                    ifelse(sub_county == "Thika", "Thika Town",
                                           ifelse(sub_county == "Muranga", "Maragwa",
                                                  ifelse(sub_county == "Nyeri Central", "Nyeri Town",
                                                         ifelse(sub_county == "Kirinyaga", "Kirinyaga Central",
                                                                ifelse(sub_county == "Maragua", "Maragwa",
                                                                       ifelse(sub_county == "Lunga Lunga", "Lungalunga",
                                                                              ifelse(sub_county == "Chuka", "Chuka/Igambang'Ombe",
                                                                                     ifelse(sub_county == "Githurai", "Kasarani",
                                                                                            ifelse(sub_county == "Gatura", "Gatanga", 
                                                                                                   ifelse(sub_county == "Kihumbuini", "Gatanga", 
                                                                                                          ifelse(sub_county == "Njiru", "Embakasi East", 
                                                                                                                 ifelse(sub_county == "Meru", "North Imenti", 
                                                                                                                        ifelse(sub_county == "Wote", "Kaiti", 
                                                                                                                               ifelse(sub_county == "Chania", "Gatundu North", 
                                                                                                                                      ifelse(sub_county == "Sagana", "Kirinyaga Central",
                                                                                                                                             ifelse(sub_county == "Ng'arua", "Laikipia West",
                                                                                                                                                    ifelse(sub_county == "Kirwara", "Gatanga",
                                                                                                                                                           ifelse(sub_county == "Gakoe", "Githunguri",
                                                                                                                                                                  ifelse(sub_county == "Kahuro", "Maragwa",
                                                                                                                                                                         ifelse(sub_county == "Nakuru", "Nakuru Town East", sub_county)))))))))))))))))))))))


### Inspect the county names again to ensure that they now match.
unique(mh_df$sub_county)[which(!unique(mh_df$sub_county) %in% subcounties_KenyaSHP)]

#make totals column
mh_dfcount <-mh_df %>% 
  count(sub_county, sort = TRUE)


#Join the shapefile and the data
### Rename the COUNTY variable, to match the variable name in the shapefile data
colnames(mh_dfcount)[1]="NAME_2"
#the above column has been renamed the same as the subcountiesSHP dataframe column that has names of sub counties
colnames(mh_dfcount)[2]="mh_totals"
#View(mh_dfcount)

### Ensure that there are no leading or trailing spaces in the subcounty variable
subcountySHP$NAME_2 <- trimws(subcountySHP$NAME_2)
mh_dfcount$NAME_2 <- trimws(mh_dfcount$NAME_2)

### Merge the data
merged_mh_df <- left_join(mh_dfcount, subcountySHP,by = "NAME_2")
# View(merged_mh_df)

### Sort the data so that the sub-county variable appears first
merged_mh_df <- merged_mh_df %>% 
  select(NAME_2, everything())



### Class of the merged data
class(merged_mh_df)
### Column names
colnames(merged_mh_df)



#Visualise the data
#plot()
#We are going to plot a base plot / map.

# plot(subcountySHP$geometry, lty = 3, col = "chocolate")

#ggplot2()
# map1 <- ggplot(data = merged_mh_df)+
#   geom_sf(aes(geometry = geometry, fill = mh_totals))+
#   theme_void()+
#   labs(title = "Distribution of Mental Illness Cases In Different Sub-Counties in Kenya",
#        caption = "By:Happiness Ndanu")+
#   theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
#         legend.title = element_blank(),
#         plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
#   #scale_fill_gradient(low = "darkgreen", high = "red")
#   scale_fill_viridis_c(option = "A")
# map1


#9.3 tmap()
# tmap_mode("plot") #Set tmap mode to static plotting or interactive viewing
# merged_mh_df <- st_sf(merged_mh_df)
# 
# map2 <- tm_shape(merged_mh_df) +
#   tm_fill("mh_totals",palette="YlOrRd",
#           title="Distribution of Mental Health Cases In Kenyan Sub-Counties",
#           id = "NAME_2") +
#   tm_borders(col = "blue",lty = 3)+
#   tm_layout(legend.position = c("left", "bottom"))+
#   tmap_mode(mode = "view")
# map2



#9.4 leaflet()

## Specify the color scheme
pal <-colorBin(
  palette = "Dark2",
  domain = merged_mh_df$mh_totals)

### Specify how labels will be displayed
labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  merged_mh_df$NAME_2, merged_mh_df$mh_totals
) %>% lapply(htmltools::HTML)


##Generate the graph
# leaflet(merged_mh_df) %>% 
#   addTiles() %>% 
#   addPolygons(color = "blue", weight = 1, dashArray = "3", fillColor = ~pal(mh_totals),
#               highlight= highlightOptions(
#                 weight = 4,
#                 color = "blue",
#                 dashArray = "",
#                 bringToFront = TRUE),
#               label = labels,
#               labelOptions = labelOptions(
#                 style = list("font-weight"="normal", padding="3px 8px"),
#                 textsize = "15px",
#                 direction = "auto")) %>%
#   addLegend(position = c("bottomright"), pal,values = ~mh_totals)

PlotMH <- newMH %>%
  count(sub_county, sex)





aging <- mentalhealth %>%
  select(sub_county, age) %>% 
  count(age)

fg <- mentalhealth %>%  left_join(aging, by= "age")


sub = fg %>%  
  distinct(sub_county) %>% 
  pull(sub_county)


gender = fg %>% 
  distinct(sex) %>% 
  pull(sex)


umri= aging %>% 
  distinct(age) %>% 
  pull(age)


# Define UI --------------------------------------------------------------------

ui <- navbarPage(
  
  title = HTML(paste("MENTAL HEALTH EDA- KIAMBU COUNTY")),
  
  tabPanel(
    title = "OVERVIEW",
    HTML(paste("According to various mental health researches made in Kenya, it is estimated that one in every ten Kenyans is suffering from a common mental health disorder.")),
    tags$a(href="https://apps.who.int/iris/bitstream/handle/10665/254610/WHO-MSD-MER-2017.2;jsessionid=EB4F7B3836BC9F361E94FB9F793A403A?sequence=1","Based on WHO ranking made in Africa,"),
    HTML(paste("Kenya has been ranked 5th with the highest number of cases of depression.  There are also rampant cases of many citizens indulging in substance abuse and experiencing high level of mental distress.This dashboard contains a detailed EDA based on data collected in a level 5 hospital based in Kiambu County, Kenya. The aim of this analysis is to have a detailed Exploratory Data Analysis on the rampant cases recorded of mental health issues in one of the counties in Kenya.")),
    
    br(),br(),
    
    tags$div(
      tags$h2("KIAMBU COUNTY MAP")),
    
    br(), br(),
    tags$img(height = 600, width = 600, src = "kmap.png"),
    
    br(), br(),),
  
  tabPanel(
    title = "PLOTS",
    
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput(
          inputId = "select_subcounty",
          label = "Select Sub County",
          choices = sub,
          multiple = FALSE,
          selected = "Chuka"
        ),
        
        selectInput(
          inputId = "select_sex",
          label = "Select Gender",
          choices = gender,
          multiple = TRUE,
          selected = "F"
        )),
      mainPanel(
        plotOutput(outputId = "geomcol"),
        plotOutput(outputId = "topcase"),
        plotOutput(outputId = "topsubcounties"),
        plotOutput(outputId = "genderdistribution"),
        plotOutput(outputId = "agedistribution"),
        plotOutput(outputId = "patienttype"),
        plotOutput(outputId = "counties")
      )
      
    ),
  ),
  
  
  navbarMenu("More",
             
             tabPanel(
               title = "Thika Data",
               sidebarPanel(
                 HTML(paste("<b>", "Brief Insights:","</b>")),
                 HTML(paste("Thika Sub-County had the highest cases of mental illnesses based on the data collected. The two most prevalent type of mental illness were schizophrenia and substance use disorder with youths and adults being the most affected age groups."))
                 
               ),
               mainPanel(
                 plotOutput(outputId = "commondisease"),
                 plotOutput(outputId = "agegender")
               )
               
             ),
             
             # End of panel 1 -- Put ',' and add another one as desired
             
             tabPanel(
               title = "Interactive Map",
               # sidebarLayout(
               #   
               #   sidebarPanel(
               #     
               #   ),
               HTML(paste("<b>","VISUAL REPRESENTATION OF MENTAL HEALTH CASES ACROSS DIFFERENT SUB- COUNTIES", "</b>")),
               br(), br(),
               leafletOutput("mymap")
               
             ),
             
             tabPanel(
               title = "Age Factor",
               sidebarLayout(
                 
                 sidebarPanel(
                   sliderInput(inputId = "age_factor",
                               label = h3("Select Age"),
                               min = 1,
                               max = 100,
                               value = c(1, 10)),
                   
                   HTML(paste("<b>","Select desired age range and view projections on the count in each sub-county", "</b>")),
                   br(), br(),
                   
                 ),
                 
                 
                 # Main panel for outputs
                 mainPanel(
                   plotOutput(outputId = "agefactor", width = "auto", height = "500px"),)
               ))
  ))

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  output$agegender <- renderPlot({
    AG_count <-newMH %>% 
      select(age,sex,sub_county) %>% 
      filter(sub_county=="Thika") %>% 
      count(age, sex, sort = TRUE)
    
    AG_count %>% 
      ggplot(mapping = aes(x=age, y= n),)+
      geom_bar(
        stat="identity",aes(fill= sex),alpha=0.9)+
      #add title
      ggtitle("VISUAL REP. OF AGE AND GENDER DISTRIBUTION IN THIKA SUB-COUNTY")+
      xlab("AGE GROUP")+
      ylab("GENDER COUNT")+
      geom_text(aes(label=n), position = position_stack(vjust = 0.5),
                size = 3, fontface = "bold", family = "Fira Sans")+
      paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
      #scale_fill_manual(values = pal, guide = "none") +
      theme_minimal()
    
  })
  
  
  
  output$commondisease <- renderPlot({
    diagnosiscount <-newMH %>%
      select(diagnosis, sub_county) %>% 
      filter(sub_county=="Thika") %>% 
      count(diagnosis, sort = TRUE) 
    
    diagnosiscount%>%  
      #make county a factor
      mutate(diagnosis=factor(diagnosis)) %>%
      #rearrange in descending order
      mutate(diagnosis= fct_reorder(diagnosis, n, .desc = TRUE)) %>%
      slice_head(n=10) %>% 
      ggplot(mapping = aes(x= diagnosis, y= n))+
      geom_col(
        aes(fill = diagnosis), show.legend = FALSE) +
      ggtitle("COUNT OF TOP 10 MOST PREVALENT TYPE OF MENTAL ILLNESS IN THIKA SUB-COUNTY")+
      xlab("DIAGNOSIS")+
      ylab("CASES COUNT")+
      geom_text(aes(label = n))+
      paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
      theme( 
        #rotate the x and y axis to make them eligible
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 0))
  })
  
  output$geomcol <- renderPlot({
    
    PlotMH %>%
      filter(sub_county %in% input$select_subcounty) %>%
      filter(sex %in% input$select_sex) %>%
      mutate(sex= fct_reorder(sex, n)) %>%
      ggplot(aes(x= sex, y = n)) +
      geom_col(aes(fill = sex),
               position = "dodge",
               show.legend = TRUE,
               alpha = 0.5)+
      theme_minimal() +
      paletteer::scale_fill_paletteer_d("ggthemes::Classic_Green_Orange_6")+
      theme(
        axis.text.x = element_text(angle = 90)
      )+
      labs(
        x= 'GENDER',
        y= 'NUMBER OF CASES PER SUB COUNTY',
        title = 'MENTAL HEALTH CASES PER SUB-COUNTY AGAINIST GENDER'
      )+
      geom_text(aes(label = n, y =n))+
      theme(panel.grid = element_blank())
    
  })
  
  #######TOP CASES########
  
  output$topcase <- renderPlot({
    
    newMH %>%
      select(diagnosis, sub_county) %>% 
      count(diagnosis, sort = TRUE) %>% 
      #making a geom col
      #make county a factor
      mutate(diagnosis=factor(diagnosis)) %>%
      #rearrange in descending order
      mutate(diagnosis= fct_reorder(diagnosis, n)) %>%
      slice_head(n=12) %>% 
      ggplot(aes(x=n, y= diagnosis, fill= diagnosis))+
      geom_col(show.legend = FALSE)+
      ggtitle("TOP MENTAL HEALTH CASES RECORDED")+
      xlab("COUNT")+
      ylab("DIAGNOSIS")+
      geom_text(aes(label=n),
                hjust=1,nudge_x = -.5,
                size = 3, fontface = "bold", family = "Fira Sans")+
      paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
      #scale_fill_manual(values = pal, guide = "none") +
      theme_minimal()
    
  })
  
  ########## TOP SUB-COUNTIES ########
  output$topsubcounties <- renderPlot({
    
    newMH %>% 
      count(sub_county, sort = TRUE) %>% 
      #make county a factor
      mutate(sub_county=factor(sub_county)) %>% 
      #rearrange in descending order
      mutate(sub_county= fct_reorder(sub_county, n)) %>% 
      slice_head(n=10) %>% 
      ggplot(aes(x=n, y= sub_county, fill= sub_county))+
      geom_col(show.legend = FALSE)+
      ggtitle("COUNT OF MENTAL ILLNESS CASES IN THE FIRST TOP 10 SUB-COUNTIES")+
      xlab("SUB-COUNTIES")+
      ylab("CASES COUNT")+
      geom_text(aes(label=n),
                hjust=1,nudge_x = -.5,
                size = 3, fontface = "bold", family = "Fira Sans")+
      paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
      #scale_fill_manual(values = pal, guide = "none") +
      theme_minimal()  
  })
  
  
  ########### GENDER DISTRIBUTION #############
  
  output$genderdistribution <- renderPlot({
    
    newMH %>% 
      count(diagnosis, sex, sort = TRUE) %>% 
      #make diagnosis a factor
      mutate(diagnosis=factor(diagnosis)) %>% 
      #rearrange in descending order
      mutate(diagnosis= fct_reorder(diagnosis, n, .desc = TRUE)) %>% 
      #grouping by the diagnosis 
      #NB: remember to ungroup after using group_by()
      group_by(diagnosis)%>% 
      #Add a new column that does the totals of the gender
      mutate(totals=sum(n),
             percent= round(n/totals*100)) %>%
      ungroup() %>%
      slice_max(order_by = totals, n=20) %>% 
      ggplot(mapping = aes(x= diagnosis, y= percent))+
      geom_col(
        aes(fill = sex),position ="dodge",show.legend = TRUE) +
      ggtitle("GENDER DISTRIBUTION OF TOP MENTAL HEALTH CASES")+
      xlab("DIAGNOSIS")+
      ylab("GENDER PERCENTAGE COUNT")+
      geom_text(aes(label=percent))+
      theme( 
        #rotate the x and y axis to make them eligible
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 0))
  })
  
  ########AGE DISTRIBUTION ##########
  output$agedistribution <- renderPlot({
    
    newMH %>% 
      count(diagnosis, age, sort = TRUE) %>% 
      #make diagnosis a factor
      mutate(diagnosis=factor(diagnosis)) %>% 
      #rearrange in descending order
      mutate(diagnosis= fct_reorder(diagnosis, n, .desc = TRUE)) %>% 
      #grouping by the diagnosis 
      #NB: remember to ungroup after using group_by()
      group_by(diagnosis)%>% 
      #Add a new column that does the totals of the age
      mutate(totals=sum(n),
             percent= round(n/totals*100)) %>%
      ungroup() %>%
      slice_max(order_by = totals, n=20) %>% 
      ggplot(mapping = aes(age, percent, fill= age))+
      ggtitle("AGE DISTRIBUTION OF TOP MENTAL HEALTH CASES")+
      geom_col()+
      facet_wrap(vars(diagnosis))+
      paletteer::scale_fill_paletteer_d("ggsci::default_nejm")
  })
  
  ########TYPE OF PATIENT########
  output$patienttype <- renderPlot({
    
    patientstypecount <-newMH %>% 
      select(type_of_patient, sub_county) %>% 
      count(sub_county,type_of_patient, sort = TRUE)
    
    patientstypecount %>%
      #make sub_county a factor
      mutate(sub_county=factor(sub_county)) %>%
      #rearrange in descending order
      mutate(sub_county= fct_reorder(sub_county, n, .desc = TRUE)) %>%
      slice_head(n=16) %>%
      #making a geom bar
      ggplot(mapping = aes(x=sub_county, y= n),)+
      geom_bar(
        stat="identity",aes(fill=type_of_patient),alpha=0.9)+
      #add title
      ggtitle("VISUAL REP. OF TYPE OF PATIENTS IN DIFF. SUB-COUNTIES")+
      xlab("SUB-COUNTIES")+
      ylab("TYPE OF PATIENT COUNT")+
      geom_text(aes(label=n), position = position_stack(vjust = 0.5),
                size = 3, fontface = "bold", family = "Fira Sans")+
      paletteer::scale_fill_paletteer_d("ggthemes::wsj_red_green")
  })
  
  #########COUNTIES###########
  output$counties <- renderPlot({
    
    newMH%>% 
      count(county, sort = TRUE) %>% 
      #make county a factor
      mutate(county=factor(county)) %>% 
      #rearrange in descending order
      mutate(county= fct_reorder(county, n, .desc = FALSE)) %>% 
      ggplot(mapping = aes(x=n, y= county, fill= county))+
      geom_col(show.legend = FALSE)+
      ggtitle("VISUAL REP. OF NUMBER OF MENTAL ILLNESS IN THE COUNTIES")+
      xlab("COUNTIES")+
      ylab("COUNT")+
      geom_text(aes(label=n),
                hjust=1,nudge_x = -.5,
                size = 3, fontface = "bold", family = "Fira Sans")+
      theme_minimal()
    
  })
  
  #######AGE FACTOR########
  
  output$agefactor <- renderPlot({
    req(input$age_factor)
    fg %>% 
      filter(age %in% seq(input$age_factor[1], input$age_factor[2])) %>% 
      count(sub_county) %>% 
      # at least two cases, right?
      filter(n > 1) %>% 
      slice_head(n = 10) %>% 
      mutate(sub_county = fct_reorder(sub_county, n)) %>% 
      # slice_head(n= input$age_factor) %>% 
      # mutate(sub_county= fct_reorder(sub_county, n)) %>% 
      ggplot(mapping = aes(y= sub_county,n))+
      ggtitle("VISUAL REP. OF CASES COUNT IN DIFFERENT SUBCOUNTIES AGAINIST AGE")+
      xlab("COUNT")+
      ylab("SUB-COUNTIES")+
      geom_point(size= 3, color="black")+
      geom_segment(aes(y= sub_county, yend= sub_county, x=0, xend= n), size=2, color="darkgreen", alpha= 0.7)
    ## Haha, couldn't pass the chance of changing it to yellow :) :) !
  })
  
  
  output$mymap <- renderLeaflet({
    
    merged_mh_df %>% st_as_sf() %>% 
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(color = "blue", weight = 1, dashArray = "3", fillColor = ~pal(mh_totals),
                  highlight= highlightOptions(
                    weight = 4,
                    color = "blue",
                    dashArray = "",
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight"="normal", padding="3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(position = c("bottomright"), pal,values = ~mh_totals)
    
  })
  
  
  
}


# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)



