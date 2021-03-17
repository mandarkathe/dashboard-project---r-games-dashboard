library(ggplot2)
library(plotly)
library(dplyr)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(reshape2)
library(tidyverse)

#Create App
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

#Read in data/wrangle
game <- read_csv('vgsales.csv')
game_melt <- melt(data=game,id.vars = c("Rank","Name","Platform","Year","Genre","Publisher"),measure.vars=c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales"))
game_melt$Year <- as.integer(game_melt$Year)
colnames(game_melt)[7] <- "Region"
colnames(game_melt)[8] <- "Copies Sold"

#game_melt <- tidyr::gather(game, key = "Region", value = "Sales", NA_Sales, EU_Sales, Global_Sales, JP_Sales, Other_Sales)
#genre_sales <- aggregate(Global_Sales ~ Genre, game, sum)
#sorted_genre_totalsales <- genre_sales[order(-genre_sales$Global_Sales),]$Genre

#Data wrangling
#sales_data <- game_melt[!(game_melt$Region=="Global_Sales"),]
#sales_data_platform <- aggregate(Sales ~ Platform+Year+Genre+Region, game_melt, sum)
#sales_data_publisher <- aggregate(Sales ~ Publisher+Year+Genre+Region, game_melt, sum)

#Nested Lists for Filters
platform_filter <- unique(game$Platform) %>%
    purrr::map(function(col) list(label = col, value = col))
platform_filter <- append(platform_filter,list(list(label="All",value="all")))

genre_filter <- unique(game$Genre) %>%
    purrr::map(function(col) list(label = col, value = col))
genre_filter <- append(genre_filter,list(list(label="All",value="all")))

publisher_filter <- unique(game$Publisher) %>%
    purrr::map(function(col) list(label = col, value = col))
publisher_filter <- append(publisher_filter,list(list(label="All",value="all")))


## Dropdown modules

dropdown_region = dccDropdown(id='region_selector',
    options = list(list(label="North America",value="NA_Sales"),
                   list(label="Europe",value="EU_Sales"),
                   list(label="Japan",value="JP_Sales"),
                   list(label="Other",value="Other_Sales"),
                   list(label="Global", value = "Global_Sales")),
    value='Global_Sales',
    multi=TRUE)

dropdown_platform = dccDropdown(id='platform_selector',options = platform_filter,value="all",multi=TRUE)

dropdown_genre = dccDropdown(id='genre_selector',options = genre_filter,value="all",multi=TRUE)

dropdown_publisher = dccDropdown(id='publisher_selector',options = publisher_filter,value="all",multi=TRUE)

# Range slider modules

range_slider_timeseries = dccRangeSlider(id = "year_selector",
    min = 1980,
    max = 2017,
    marks = list("1980" = "1980",
                 "1985" = "1985",
                 "1990" = "1990",
                 "1995" = "1995",
                 "2000" = "2000",
                 "2005" = "2005",
                 "2010" = "2010",
                 "2015" = "2015"),
    value = list(1980,2017))

clearing_filters_button = dbcButton("Reset Filters",id="reset_button")

# Tab modules


tab1_components = 
    htmlDiv(list(
        htmlBr(),
        htmlLabel('Time Range'),
        range_slider_timeseries,
        htmlBr(),
        htmlLabel("Plot 1: Copies Sold vs Time"),
        dccGraph(id='plot-area'),
        htmlBr(),
        htmlLabel("Plot 2: Number of Games Released vs Time"),
        dccGraph(id='plot-area2'),
        htmlBr(),
        htmlLabel("Plot 3: Number of Platforms, Genres and Publishers with games selling over 100,000 copies."),
        dccGraph(id='plot-area3')
    ))

first_tab_sidebar_Card = dbcCard(
    dbcCardBody(htmlDiv(
        list(htmlH4("Dashboard for Video Games Statistics")
        )
    ))
)

first_tab_sidebar_Card_2 = dbcCard(
    dbcCardBody(htmlDiv(
        list(clearing_filters_button,htmlBr(),htmlBr(),
             htmlLabel("Select your region of interest:"),
             dropdown_region,
             htmlBr(),
             htmlLabel("Select your Platform of interest:"),
             dropdown_platform,
             htmlBr(),
             htmlLabel("Select your Genre of interest:"),
             dropdown_genre,
             htmlBr(),
             htmlLabel("Select your Publisher of interest:"),
             dropdown_publisher,
             htmlBr()
        )
    ))
)




first_tab_figures_card = dbcCard(
    dbcCardBody(htmlDiv(list(tab1_components))))


row_tab1 = dbcRow(list(
    dbcCol(first_tab_sidebar_Card, width = 3),
    dbcCol(first_tab_figures_card), width = 9))


tab_1 = dccTab(label='Number of copies released',children=list(first_tab_figures_card))

tab_2 = dccTab(label='Number of copies sold', children=list(
    dccGraph(
        id='example-graph-1',
        figure=list(
            'data'= list(
                list('x'= c(1, 2, 3), 'y'= c(1, 4, 1),
                     'type'= 'bar', 'name'= 'SF'),
                list('x'= c(1, 2, 3), 'y'= c(1, 2, 3),
                     'type'= 'bar', 'name'= 'Montréal')
            )
        )
    )
))

tab_3 = dccTab(label='Top Game titles, Platforms and Publishers across Genres', children=list(
    dccGraph(
        id='example-graph-2',
        figure=list(
            'data'= list(
                list('x'= c(1, 2, 3), 'y'= c(1, 4, 1),
                     'type'= 'bar', 'name'= 'SF'),
                list('x'= c(1, 2, 3), 'y'= c(1, 2, 3),
                     'type'= 'bar', 'name'= 'Montréal')
            )
        )
    )
))

app$layout(dbcRow(list(
    dbcCol(list(
           htmlDiv(list(first_tab_sidebar_Card, htmlBr(), first_tab_sidebar_Card_2, htmlBr()))),width = 3),
    dbcCol(dbcContainer(
    (htmlDiv(htmlDiv(list(
        dccTabs(id="tabs", children=list(
            tab_1,tab_2,tab_3
    ))
    ))
))), width = 9)

)))

#Callback for Button
app$callback(
    list(output('region_selector', 'value'),
         output('platform_selector', 'value'),
         output('genre_selector', 'value'),
         output('publisher_selector', 'value'),
         output('year_selector', 'value')),
    list(input('reset_button','n_clicks')),
    function(n_clicks){
        #Input: if button is clicked
        #Output: Default values for all filters
        #
        #If clicked - return default values to filters
        return (list("Global_Sales","all","all","all",list(1980,2017)))
    }
)

#Callback for all plots
app$callback(
    list(output('plot-area', 'figure'),
         output('plot-area2', 'figure'),
         output('plot-area3', 'figure')),
    list(input('region_selector', 'value'),
         input('platform_selector', 'value'),
         input('genre_selector', 'value'),
         input('publisher_selector', 'value'),
         input('year_selector', 'value')),
    function(reg,plat,gen,pub,years) {
        # Input: List of Regions, Platforms, Genres, Publishers, Min and Max Year
        # Output: Graph
        #
        # Create subset based on filters 
        # Pass to graph
        # Output graph
        if ("Global_Sales" %in% reg){
            filter_region = list("Global_Sales")
        } else {
            filter_region = reg
        }
        if ("all" %in% plat){
            filter_plat = unique(game_melt$Platform)
        } else {
            filter_plat = plat
        }
        if ("all" %in% gen){
            filter_gen = unique(game_melt$Genre)
        } else {
            filter_gen = gen
        }
        if ("all" %in% pub){
            filter_pub = unique(game_melt$Publisher)
        } else {
            filter_pub = pub
        }
        min_year = years[1]
        max_year = years[2]
        
        graph1 <- game_melt[,3:8] %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Year,Genre) %>%
            summarise("Copies Sold" = sum(`Copies Sold`)) %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Copies Sold`,
                fill = Genre,
                text = paste("Year: ",as.factor(Year),
                             "<br>Copies Sold: ",`Copies Sold`,
                             "<br>Genre: ", Genre)) + 
            geom_bar(stat="identity")+
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2)) +
            ylab("Number of Copies Sold (in millions)")+
            xlab("Year")
        graph1 <- ggplotly(graph1,tooltip="text")
        
        graph2 <- game_melt[,3:8] %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Year,Genre) %>%
            count(Year,Genre) %>%
            rename(`Number of Releases`="n") %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Number of Releases`,
                fill = Genre,
                text = paste("Year: ",as.factor(Year),
                             "<br>No. of Games Released: ",`Number of Releases`,
                             "<br>Genre: ", Genre)) + 
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
            ylab("Number of Games Released")+
            xlab("Year")
        graph2<-ggplotly(graph2,tooltip="text")
        
        graph3 <- game_melt[,3:8] %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Year)%>%
            melt(id.vars=c("Year"),measure.vars=c("Genre","Platform","Publisher")) %>%
            rename(Category='variable') %>% 
            group_by(Year,Category) %>%
            unique() %>%
            count(Year,Category) %>%
            rename(`Counts of Genres, Publishers and Platforms`= n) %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Counts of Genres, Publishers and Platforms`,
                fill = Category,
                text = paste("Year: ",as.factor(Year),
                             "<br>No. of Succesful Gen, Publ, Plat: ",`Counts of Genres, Publishers and Platforms`,
                             "<br>Category: ", Category)) + 
            geom_bar(stat="identity")+
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
            ylab("Counts of Sucessful Genres, Publishers and Platforms")+
            xlab("Year")
        graph3<-ggplotly(graph3,tooltip="text")
        
        return(list(graph1,graph2,graph3))
    }
)

# #Callback for Plot2
# app$callback(
#     output('plot-area2', 'figure'),
#     list(input('region_selector', 'value'),
#          input('platform_selector', 'value'),
#          input('genre_selector', 'value'),
#          input('publisher_selector', 'value'),
#          input('year_selector', 'value')),
#     function(reg,plat,gen,pub,years) {
#         # Input: List of Regions, Platforms, Genres, Publishers, Min and Max Year
#         # Output: Graph
#         #
#         # Create subset based on filters 
#         # Pass to graph
#         # Output graph
#         if ("Global_Sales" %in% reg){
#             filter_region = list("Global_Sales")
#         } else {
#             filter_region = reg
#         }
#         if ("all" %in% plat){
#             filter_plat = unique(game_melt$Platform)
#         } else {
#             filter_plat = plat
#         }
#         if ("all" %in% gen){
#             filter_gen = unique(game_melt$Genre)
#         } else {
#             filter_gen = gen
#         }
#         if ("all" %in% pub){
#             filter_pub = unique(game_melt$Publisher)
#         } else {
#             filter_pub = pub
#         }
#         min_year = years[1]
#         max_year = years[2]
#         
#         graph2 <- game_melt[,3:8] %>% 
#             subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
#             group_by(Year,Genre) %>%
#             count(Year,Genre) %>%
#             rename(`Number of Releases`="n") %>% 
#             ggplot() +
#             aes(x=as.factor(Year),
#                 y=`Number of Releases`,
#                 fill = Genre,
#                 text = paste("Year: ",as.factor(Year),
#                              "<br>No. of Games Released: ",`Number of Releases`,
#                              "<br>Genre: ", Genre)) + 
#             geom_bar(stat="identity") +
#             theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
#             ylab("Number of Games Released")+
#             xlab("Year")
#         
#         return (ggplotly(graph2,tooltip="text"))
#     }
# )
# 
# #Callback for Plot3
# app$callback(
#     output('plot-area3', 'figure'),
#     list(input('region_selector', 'value'),
#          input('platform_selector', 'value'),
#          input('genre_selector', 'value'),
#          input('publisher_selector', 'value'),
#          input('year_selector', 'value')),
#     function(reg,plat,gen,pub,years) {
#         # Input: List of Regions, Platforms, Genres, Publishers, Min and Max Year
#         # Output: Graph
#         #
#         # Create subset based on filters 
#         # Pass to graph
#         # Output graph
#         if ("Global_Sales" %in% reg){
#             filter_region = list("Global_Sales")
#         } else {
#             filter_region = reg
#         }
#         if ("all" %in% plat){
#             filter_plat = unique(game_melt$Platform)
#         } else {
#             filter_plat = plat
#         }
#         if ("all" %in% gen){
#             filter_gen = unique(game_melt$Genre)
#         } else {
#             filter_gen = gen
#         }
#         if ("all" %in% pub){
#             filter_pub = unique(game_melt$Publisher)
#         } else {
#             filter_pub = pub
#         }
#         min_year = years[1]
#         max_year = years[2]
#         
#         graph3 <- game_melt[,3:8] %>% 
#             subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
#             group_by(Year)%>%
#             melt(id.vars=c("Year"),measure.vars=c("Genre","Platform","Publisher")) %>%
#             rename(Category='variable') %>% 
#             group_by(Year,Category) %>%
#             unique() %>%
#             count(Year,Category) %>%
#             rename(`Counts of Genres, Publishers and Platforms`= n) %>% 
#             ggplot() +
#             aes(x=as.factor(Year),
#                 y=`Counts of Genres, Publishers and Platforms`,
#                 fill = Category,
#                 text = paste("Year: ",as.factor(Year),
#                              "<br>No. of Succesful Gen, Publ, Plat: ",`Counts of Genres, Publishers and Platforms`,
#                              "<br>Category: ", Category)) + 
#             geom_bar(stat="identity")+
#             theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
#             ylab("Counts of Sucessful Genres, Publishers and Platforms")+
#             xlab("Year")
#         
#         return (ggplotly(graph3,tooltip="text"))
#     }
# )

app$run_server(host = '127.0.0.1')