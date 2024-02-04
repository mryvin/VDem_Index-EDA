library(rmdformats)
library(vdemdata)
library(ggplot2)
library(httr)
library(rvest)
library(countrycode)
library(plotly)
library(shiny)
library(tidyverse)
library(ggbeeswarm)
library(tidyr)
library(dplyr)
library(stringr)
library(tm)
library(RColorBrewer)
library(ggthemes)

data <- vdem
data$continent <- countrycode(data$country_text_id, "iso3c", "continent")


######################################################################################################################################################################################################################################################################## 
################## Democracy Indices by Continent
######################################################################################################################################################################################################################################################################## 

# Define a function to create the plots
create_plot <- function(data, index, title) {
  data %>%
    filter(year == 2022 & continent != "NA") %>%
    select(index, year, continent) %>%
    na.omit() %>%
    ggplot(aes(continent, get(index), fill = continent)) +
    geom_boxplot(color = "black", size = 0.8) +
    scale_fill_manual(values = c("Europe" = "dodgerblue2", "Americas" = "chocolate1", "Asia" = "firebrick2", "Africa" = "chartreuse3", "Oceania" = "darkorchid1")) +
    labs(x = "Continent", y = "Polyarchy", title = title) +
    theme_fivethirtyeight()
}

##### Electoral Democracy Index
####################################################

create_plot(data, "v2x_polyarchy", "Electoral Democracy Index by Continent")

##### Liberal Democracy Index
####################################################

create_plot(data, "v2x_libdem", "Liberal Democracy Index by Continent")

##### Participatory Democracy Index
####################################################

create_plot(data, "v2x_partipdem", "Participatory Democracy Index by Continent")

##### Deliberative Democracy Index
####################################################

create_plot(data, "v2x_delibdem", "Deliberative Democracy Index by Continent")

##### Egalitarian Democracy Index
####################################################

create_plot(data, "v2x_egaldem", "Egalitarian Democracy Index by Continent")

######################################################################################################################################################################################################################################################################## 
################## Major vs High Fluctuating Countries
######################################################################################################################################################################################################################################################################## 

# Subset the high level democracy indices
High_Level_Indices <- data[c("country_name","year","v2x_polyarchy","v2x_libdem","v2x_partipdem","v2x_delibdem","v2x_egaldem")]

# Define a function that takes the data and a column name and returns the countries with the highest fluctuation in that column
top_fluctuation_countries <- function(data, column) {
  data %>%
    group_by(country_name) %>%
    summarize(min_value = min(.data[[column]], na.rm = TRUE),
              max_value = max(.data[[column]], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(diff = max_value - min_value) %>%
    arrange(desc(diff)) %>%
    select(country_name) %>%
    head(3)
}

# Define a function to create the plots
create_plot <- function(data, index, title, countries) {
  data %>%
    filter(country_name %in% countries & year >= 1900) %>%
    ggplot(aes(x=year, y=get(index), color=country_name)) +
    geom_line(size = 1) +
    labs(x="Year", y=index, color = "Country") +
    theme_fivethirtyeight() +
    ggtitle(title)
}

# Major countries
major_countries <- c("India","France", "Russia","United States of America","United Kingdom","China")

# Indices
indices <- c("v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem")

# Titles
titles <- c("Electoral Democracy Index in Major Countries", "Liberal Democracy Index in Major Countries", "Participatory Democracy Index in Major Countries", "Deliberative Democracy Index in Major Countries", "Egalitarian Democracy Index in Major Countries")

##### Major Countries
####################################################

for(i in 1:length(indices)) {
  print(create_plot(High_Level_Indices, indices[i], titles[i], major_countries))
}

##### High Fluctuation Countries
####################################################
# Use the function to create the plots for high fluctuation countries
for(i in 1:length(indices)) {
  fluctuation_countries <- top_fluctuation_countries(High_Level_Indices, indices[i])$country_name
  print(create_plot(High_Level_Indices, indices[i], paste("Top Fluctuating Countries by", indices[i]), fluctuation_countries))
}

######################################################################################################################################################################################################################################################################## 
################## Slope Coefficients
######################################################################################################################################################################################################################################################################## 

# Define a function to create the models
create_model <- function(data, index) {
  data %>%
    filter(!is.na(continent) & year >= 1900) %>%
    select(country_name, continent, year, get(index)) %>%
    nest(data = c(year, get(index))) %>%
    mutate(model = map(data, ~lm(get(index) ~ year, data = .))) %>%
    mutate(tidy_model = map(model, broom::tidy)) %>%
    unnest(cols = tidy_model) %>%
    filter(term == "year") %>%
    select(continent, country_name, estimate, statistic)
}

# Indices
indices <- c("v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem")

# Use the function to create the models for each index
models <- lapply(indices, create_model, data = data)

# Define colors
colors <- c("Europe" = "dodgerblue2", "Americas" = "chocolate1", "Asia" = "firebrick2", "Africa" = "chartreuse3", "Oceania" = "darkorchid1")

# Beeswarm plot for slope coefficient
lapply(models, function(model) {
  ggplot(model, aes(x = continent, y = estimate, color = continent)) +
    geom_beeswarm() +
    scale_color_manual(values = colors) +
    labs(x = "Continent", y = "Slope Coefficient", title = "Slope Coefficient by Continent") +
    theme_fivethirtyeight()
})

######################################################################################################################################################################################################################################################################## 
################## Top Countries by Democracy Index
######################################################################################################################################################################################################################################################################## 

# Define a function to get the flag image URL
get_flag_url <- function(country_name) {
  # make the wikipedia URL for the country
  url <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", country_name))
  
  # Special cases
  if (country_name == "Georgia") {
    url = "https://en.wikipedia.org/wiki/Georgia_(country)"
  }
  if (country_name == "Ireland") {
    url = "https://en.wikipedia.org/wiki/Republic_of_Ireland"
  }
  if (grepl("Palestine", country_name)) {
    url <- "https://en.wikipedia.org/wiki/State_of_Palestine"
  }
  
  page <- read_html(url)
  
  # find the main image element
  image_elem <- page %>%
    html_nodes(".infobox img:first-child")
  
  # if image element exists, extract image URL 
  if (length(image_elem) > 0) {
    image_url <- image_elem %>% first() %>% html_attr("src")
    return(paste0("http:", image_url))
  } else {
    cat("No image found for ", country_name, "\n")
    return(NA)
  }
}

# Get the flag image URLs
data$flag_image <- sapply(data$country_name, get_flag_url)

# Define a function to download the flag image
download_flag <- function(flag_image, country_name) {
  filename <- paste0("flag_", country_name, ".png")
  filename <- gsub("/", "_", filename)
  
  # Download the image
  # Uncomment the following lines to download the images
  # link <- flag_image
  # GET(link, write_disk(filename, overwrite = TRUE))
  
  # Return the filename
  return(filename)
}

# Download the flag images and update the flag_image column to the local filename
# Uncomment the following line to download the images
# data$flag_image <- mapply(download_flag, data$flag_image, data$country_name)

##### Static Image
####################################################

colors <- c("Europe" = "dodgerblue2", "Americas" = "chocolate1", "Asia" = "firebrick2", "Africa" = "chartreuse3", "Oceania" = "darkorchid1")

# Include only 2022
data2 <- subset(data, year == 2022)

# Order countries by Electoral Democracy Indices
data2 <- data2[order(-data2$v2x_polyarchy),]
data2$country_name <- reorder(data2$country_name, -data2$v2x_polyarchy)

# Select top 10 countries
top_countries <- head(data2, 10)

# Set the flag image size
img_height <- 0.8
img_width <- 0.9

# Function to read and embed the flag image in the plot
embed_image <- function(img_path, x, y) {
  img_raw <- readBin(img_path, "raw", file.info(img_path)$size)
  encoded_image <- base64enc::dataURI(img_raw, mime = "image/png")
  list(source = encoded_image, xref = "x", yref = "y", xanchor = "center", yanchor = "middle",
       sizex = img_width, sizey = img_height, x = x, y = y)
}

# Code to be used to embed the images
img_sources <- list()
for (i in 1:nrow(top_countries)) {
  img_path <- top_countries$flag_image[i]
  x <- top_countries$country_name[i]
  y <- max(top_countries$v2x_polyarchy) +0.1
  img_sources[[i]] <- embed_image(img_path, x = x, y = y)
}

# Make the bar plot
plot <- plotly::plot_ly(data = top_countries, x = ~country_name, y = ~v2x_polyarchy, color = ~continent, colors = colors[unique(top_countries$continent)],
                        type = "bar", source = "bar_source", text = ~country_name, textposition = "inside",
                        textfont = list(size = 14, color = "white")) %>%
  plotly::layout(title = list(text = "Electoral Democracy Index by Country", y = 0.975), xaxis = list(title = "Country", showticklabels = FALSE),
                 yaxis = list(title = "Electoral Democracy Index", range = c(0, 1.1)), barmode = "group", images = img_sources)

plot


##### Shiny Object
####################################################


# UI to select continent
dropdown_ui <- selectInput("continent", "Select a continent:", choices = c("All", levels(factor(data$continent, exclude = NA))), selected = "All")

# UI to select year
slider_ui <- sliderInput("year", "Choose a year:", min = min(data$year), max = max(data$year), value = max(data$year), step = 1)

# UI to select which Democracy index
variable_ui <- selectInput("variable", "Select a variable to plot:", choices = c("Electoral democracy index" = "v2x_polyarchy", 
                                                                                 "Liberal democracy index" = "v2x_libdem", 
                                                                                 "Participatory democracy index" = "v2x_partipdem", 
                                                                                 "Egalitarian democracy index" = "v2x_egaldem",
                                                                                 "Deliberative democracy index" = "v2x_delibdem"), 
                           selected = "Electoral democracy index")

server <- function(input, output, session) {
  
  # Subset data based on the selected year and continent
  data_subset <- reactive({
    if (input$continent == "All") {
      subset(data, year == input$year)
    } else {
      subset(data, year == input$year & continent == input$continent)
    }
  })
  
  # Order data by the selected democracy index
  data_ordered <- reactive({
    data_subset() %>%
      arrange(desc(get(input$variable)))
  })
  
  # Top 10 countries based on the democracy index
  top_countries <- reactive({
    head(data_ordered(), 10)
  })
  
  # Set the flag image size
  img_height <- 0.8
  img_width <- 0.9
  
  # Function to read and embed the flag image in the plot
  embed_image <- function(img_path, x, y) {
    img_raw <- readBin(img_path, "raw", file.info(img_path)$size)
    encoded_image <- base64enc::dataURI(img_raw, mime = "image/png")
    list(source = encoded_image, xref = "x", yref = "y", xanchor = "center", yanchor = "middle",
         sizex = img_width, sizey = img_height, x = x, y = y)
  }
  
  # Code to be used to embed the images
  img_sources <- reactive({
    img_sources_list <- list()
    for (i in 1:nrow(top_countries())) {
      img_path <- top_countries()$flag_image[i]
      x <- top_countries()$country_name[i]
      y <- max(top_countries()[, input$variable]) + 0.12
      img_sources_list[[i]] <- embed_image(img_path, x = x, y = y)
    }
    img_sources_list
  })
  
  # Make the bar plot
  output$plot <- renderPlotly({
    # Reorder country_name based on the selected variable
    top_countries_ordered <- top_countries() %>%
      arrange(desc(get(input$variable)))
    
    plotly::plot_ly(data = top_countries_ordered, x = ~reorder(country_name, -get(input$variable)), y = ~get(input$variable), color = ~continent, 
                    colors = colors[unique(top_countries()$continent)], type = "bar", source = "bar_source", 
                    text = ~country_name, textposition = "inside", textfont = list(size = 14, color = "white")) %>%
      plotly::layout(title = list(text = paste(switch(input$variable,
                                                      "v2x_polyarchy" = "Electoral democracy index",
                                                      "v2x_libdem" = "Liberal democracy index",
                                                      "v2x_partipdem" = "Participatory democracy index",
                                                      "v2x_egaldem" = "Egalitarian democracy index",
                                                      "v2x_delibdem" = "Deliberative democracy index"), "by Country"), y = 0.975), 
                     xaxis = list(title = "Country", showticklabels = FALSE, tickangle = -45, tickfont = list(size = 12)),
                     yaxis = list(title = switch(input$variable,
                                                 "v2x_polyarchy" = "Electoral democracy index",
                                                 "v2x_libdem" = "Liberal democracy index",
                                                 "v2x_partipdem" = "Participatory democracy index",
                                                 "v2x_egaldem" = "Egalitarian democracy index",
                                                 "v2x_delibdem" = "Deliberative democracy index"), range = c(0, 1.1)), barmode = "group", images = img_sources())
    
  })
  
}

# Run the Shiny app with all three UIs
shinyApp(ui = fluidPage(variable_ui, dropdown_ui, slider_ui, plotlyOutput("plot")), server = server)

######################################################################################################################################################################################################################################################################## 
################## Country Names over Time
########################################################################################################################################################################################################################################################################   

# Split histname into unique words and keep only the histname and year columns
# Words between brackets were removed, as we decided that these should not count as part of the historical name
data_clean <- data %>% 
  select(histname, year) %>% 
  mutate(histname = gsub("\\[.*?\\]", "", histname)) %>% 
  mutate(histname = str_replace_all(histname, "[^[:alpha:]]", " ")) %>% 
  mutate(histname = tolower(histname)) %>%
  mutate(histname = trimws(histname)) %>%
  separate_rows(histname, sep = " ")


# Calculate frequency of each word per year
# Because earlier years in the dataset have more countries, it would be easier 
# to interpret change by using the frequency of a word rather than the absolute number
word_freq <- data_clean %>% 
  filter(!(histname == "" | histname %in% stopwords("en"))) %>% 
  group_by(histname, year) %>% 
  summarize(n = n()) %>% 
  group_by(year) %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  select(word = histname, year, freq) %>% 
  filter(freq > 0)

# Spread the data to have one column for each year
df_freq <- word_freq %>% 
  spread(year, freq, fill = 0)

# Words were only kept if they existed in at least 5.5% of the data in any given year
# This number was chosen to limit the graph to a smaller number of lines
df_filtered <- df_freq %>% 
  filter_if(is.numeric, any_vars(. > 0.055))

df_reformatted <- df_filtered %>% 
  pivot_longer(cols = -word, names_to = "year", values_to = "freq") %>% 
  mutate(year = as.numeric(year))

colors <- c("firebrick3", "darkorange2", "gold2", "forestgreen", "dodgerblue3", "darkorchid2")

ggplot(df_reformatted, aes(x = year, y = freq, color = word)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(min(df_reformatted$year), max(df_reformatted$year), by = 25),
                     labels = function(x) format(x, format = "%Y")) +
  scale_color_manual(values = colors) +  # Set the color palette
  ggtitle("Words in country name over time") +
  labs(x = "Year", y = "Frequency", color = "Word") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme_fivethirtyeight(base_size = 12) +
  geom_text(aes(label = "Republic", x = 1962, y = 0.24),
            color = "darkorchid2", size = 4, hjust = 0, fontface = "bold", angle = 35) +
  geom_text(aes(label = "Kingdom", x = 1810, y = 0.123),
            color = "forestgreen", size = 4, hjust = 0, fontface = "bold", angle = 5) +
  geom_text(aes(label = "Empire", x = 1870, y = 0.073),
            color = "gold3", size = 4, hjust = 0, fontface = "bold", angle = 357) +
  geom_text(aes(label = "Colony", x = 1951, y = 0.090),
            color = "darkorange2", size = 4, hjust = 0, fontface = "bold", angle = 330) +
  geom_text(aes(label = "British", x = 1901, y = 0.10),
            color = "firebrick3", size = 4, hjust = 0, fontface = "bold", angle = 355) +
  geom_text(aes(label = "Protectorate", x = 1923, y = 0.035),
            color = "dodgerblue3", size = 4, hjust = 0, fontface = "bold", angle = 349) +
  guides(color = "none")

######################################################################################################################################################################################################################################################################## 
################## Leaders and Democracy Index
########################################################################################################################################################################################################################################################################   

##### United States of America
####################################################

usa_data <- subset(data, country_text_id == "USA" & year >= 1977)

# Order the Presidents in chronological order
ordered_v2exnamhos <- unique(usa_data$v2exnamhos[order(usa_data$year)])[!is.na(unique(usa_data$v2exnamhos[order(usa_data$year)]))]

# Used in case a single person serves nonconsecutive terms
usa_data$consec_years <- with(usa_data, ave(year, v2exnamhos, FUN = function(x) cumsum(c(1, diff(x) != 1))))

usa_data$v2exnamhos <- ifelse(usa_data$v2exnamhos == "James Earl Carter", "Jimmy Carter", usa_data$v2exnamhos)
usa_data$v2exnamhos <- ifelse(usa_data$v2exnamhos == "Ronald Wilson Reagan", "Ronald Reagan", usa_data$v2exnamhos)
usa_data$v2exnamhos <- ifelse(usa_data$v2exnamhos == "George Herbert Walker Bush", "George H. W. Bush", usa_data$v2exnamhos)
usa_data$v2exnamhos <- ifelse(usa_data$v2exnamhos == "William Jefferson Clinton", "Bill Clinton", usa_data$v2exnamhos)
usa_data$v2exnamhos <- ifelse(usa_data$v2exnamhos == "George Walker Bush", "George W. Bush", usa_data$v2exnamhos)
usa_data$v2exnamhos <- ifelse(usa_data$v2exnamhos == "Barack Hussein Obama", "Barack Obama", usa_data$v2exnamhos)
usa_data$v2exnamhos <- ifelse(usa_data$v2exnamhos == "Donald John Trump", "Donald Trump", usa_data$v2exnamhos)
usa_data$v2exnamhos <- ifelse(usa_data$v2exnamhos == "Joseph Robinette Biden Jr.", "Joe Biden", usa_data$v2exnamhos)

v2ex_levels <- unique(usa_data$v2exnamhos)

# Color palette
num_colors <- length(v2ex_levels)
palette <- brewer.pal(num_colors, "Set2")

# Plot the lines
ggplot(usa_data, aes(x = year, y = v2x_polyarchy, color = v2exnamhos, 
                     group = interaction(v2exnamhos, consec_years))) + 
  geom_line(size = 1.5) +
  geom_point() +
  labs(x = "Year", y = "Electoral Democracy Index", color = "US President") +
  ggtitle("Electoral Democracy Index by US President Over Time") +
  theme_bw() +
  scale_color_manual(values = setNames(palette, v2ex_levels)) +
  theme_fivethirtyeight(base_size = 12) +
  geom_text(aes(label = "Carter", x = 1976.5, y = 0.839),
            color = "#66C2A5", size = 4, hjust = 0, fontface = "bold", angle = 47) +
  geom_text(aes(label = "Reagan", x = 1982.5, y = 0.855),
            color = "#FC8D62", size = 4, hjust = 0, fontface = "bold", angle = 7) +
  geom_text(aes(label = "HW Bush", x = 1988.5, y = 0.869),
            color = "#8DA0CB", size = 3, hjust = 0, fontface = "bold", angle = 25) +
  geom_text(aes(label = "Clinton", x = 1994.5, y = 0.866),
            color = "#E78AC3", size = 4, hjust = 0, fontface = "bold", angle = 358) +
  geom_text(aes(label = "W Bush", x = 2003, y = 0.868),
            color = "#A6D854", size = 4, hjust = 0, fontface = "bold", angle = 55) +
  geom_text(aes(label = "Obama", x = 2010, y = 0.893),
            color = "#FFD92F", size = 4, hjust = 0, fontface = "bold", angle = 345) +
  geom_text(aes(label = "Trump", x = 2016.1, y = 0.815),
            color = "#E5C494", size = 4, hjust = 0, fontface = "bold", angle = 303) +
  geom_text(aes(label = "Biden", x = 2021.5, y = 0.808),
            color = "#B3B3B3", size = 4, hjust = 0, fontface = "bold", angle = 52) +
  guides(color = "none")

##### China
####################################################

chn_data <- subset(data, country_text_id == "CHN" & year >= 1928)

chn_data$v2exnamhos[chn_data$year <= 1992 & chn_data$year >= 1959] <- chn_data$v2exnamhog[chn_data$year <= 1992 & chn_data$year >= 1959]

# Order the Leaders in chronological order
ordered_v2exnamhos <- unique(chn_data$v2exnamhos[order(chn_data$year)])[!is.na(unique(chn_data$v2exnamhos[order(chn_data$year)]))]

# Used in case a single person serves nonconsecutive terms
chn_data$consec_years <- with(chn_data, ave(year, v2exnamhos, FUN = function(x) cumsum(c(1, diff(x) != 1))))

v2ex_levels <- unique(chn_data$v2exnamhos)

# Color palette
num_colors <- length(v2ex_levels)
palette <- brewer.pal(num_colors, "Set2")

# Plot the lines
ggplot(chn_data, aes(x = year, y = v2x_polyarchy, color = v2exnamhos, 
                     group = interaction(v2exnamhos, consec_years))) + 
  geom_line(size = 1.5) +
  geom_point() +
  labs(x = "Year", y = "Electoral Democracy Index", color = "Chinese Leader") +
  ggtitle("Electoral Democracy Index by Chinese Leader Over Time") +
  theme_bw() +
  scale_color_manual(values = setNames(palette, v2ex_levels)) +
  theme_fivethirtyeight(base_size = 12.5) +
  geom_text(aes(label = "Chiang Kai-shek", x = 1948, y = 0.145),
            color = "#66C2A5", size = 3.5, hjust = 0, fontface = "bold", angle = 0) + 
  geom_text(aes(label = "Chiang Kai-shek", x = 1929, y = 0.085),
            color = "#66C2A5", size = 3.5, hjust = 0, fontface = "bold", angle = 338) +
  geom_text(aes(label = "Lin Sen", x = 1933, y = 0.097),
            color = "#FC8D62", size = 3.5, hjust = 0, fontface = "bold", angle = 7) +
  geom_text(aes(label = "Mao", x = 1959, y = 0.077),
            color = "#8DA0CB", size = 4, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Hua Guofeng", x = 1976.5, y = 0.066),
            color = "#E78AC3", size = 2.5, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Deng", x = 1981, y = 0.104),
            color = "#A6D854", size = 3.5, hjust = 0, fontface = "bold", angle = 22) +
  geom_text(aes(label = "Jiang", x = 1994, y = 0.107),
            color = "#FFD92F", size = 4, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Hu", x = 2006.5, y = 0.091),
            color = "#E5C494", size = 4, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Xi", x = 2016.6, y = 0.085),
            color = "#B3B3B3", size = 4, hjust = 0, fontface = "bold", angle = 316) +
  guides(color = "none")

##### India
####################################################

ind_data <- subset(data, country_text_id == "IND" & year >= 1989)

# Order the Prime Ministers in chronological order
ordered_v2exnamhog <- unique(ind_data$v2exnamhog[order(ind_data$year)])[!is.na(unique(ind_data$v2exnamhog[order(ind_data$year)]))]

# Used in case a single person serves nonconsecutive terms
ind_data$consec_years <- with(ind_data, ave(year, v2exnamhog, FUN = function(x) cumsum(c(1, diff(x) != 1))))

v2ex_levels <- unique(ind_data$v2exnamhog)

# Color palette
num_colors <- length(v2ex_levels)
palette <- brewer.pal(num_colors, "Set2")

# Plot the lines
ggplot(ind_data, aes(x = year, y = v2x_polyarchy, color = v2exnamhog, 
                     group = interaction(v2exnamhog, consec_years))) + 
  geom_line(size = 1.5) +
  geom_point() +
  labs(x = "Year", y = "Electoral Democracy Index", color = "Indian Prime Minister") +
  ggtitle("Electoral Democracy Index by Indian Prime Minister Over Time") +
  theme_bw() +
  scale_color_manual(values = setNames(palette, v2ex_levels)) +
  theme_fivethirtyeight(base_size = 11.5) +
  geom_text(aes(label = "Modi", x = 2016.7, y = 0.54),
            color = "#B3B3B3", size = 4, hjust = 0, fontface = "bold", angle = 320) +
  geom_text(aes(label = "Singh", x = 2007.1, y = 0.695),
            color = "#E5C494", size = 4, hjust = 0, fontface = "bold", angle = 340) +
  geom_text(aes(label = "Vajpayee", x = 1998.5, y = 0.72),
            color = "#FFD92F", size = 4, hjust = 0, fontface = "bold", angle = 355) +
  geom_text(aes(label = "Gujral", x = 1996.8, y = 0.765),
            color = "#A6D854", size = 3, hjust = 0, fontface = "bold", angle = 13) +
  geom_text(aes(label = "Deve Gowda", x = 1995.8, y = 0.73),
            color = "#E78AC3", size = 3, hjust = 0, fontface = "bold", angle = 330) +
  geom_text(aes(label = "Rao", x = 1992, y = 0.75),
            color = "#8DA0CB", size = 4, hjust = 0, fontface = "bold", angle = 10) +
geom_text(aes(label = "Shekhar", x = 1990.5, y = 0.69),
            color = "#FC8D62", size = 3, hjust = 0, fontface = "bold", angle = 340) +
  geom_text(aes(label = "V.P. Singh", x = 1989, y = 0.655),
            color = "#66C2A5", size = 3, hjust = 0, fontface = "bold", angle = 340) +
  guides(color = "none")

##### Russia/Soviet Union
####################################################

rus_data <- subset(data, country_text_id == "RUS" & year >= 1953)

# Order the Leaders in chronological order
ordered_v2exnamhos <- unique(rus_data$v2exnamhos[order(rus_data$year)])[!is.na(unique(rus_data$v2exnamhos[order(rus_data$year)]))]

# Used in case a single person serves nonconsecutive terms (Such as Putin)
rus_data$consec_years <- with(rus_data, ave(year, v2exnamhos, FUN = function(x) cumsum(c(1, diff(x) != 1))))

rus_data$v2exnamhos <- ifelse(rus_data$v2exnamhos == "Nikita Sergeyevich Khrushchev", "Nikita Khrushchev", rus_data$v2exnamhos)
rus_data$v2exnamhos <- ifelse(rus_data$v2exnamhos == "Leonid Ilich Brezhnev", "Leonid Brezhnev", rus_data$v2exnamhos)
rus_data$v2exnamhos <- ifelse(rus_data$v2exnamhos == "Yury Vladimirovich Andropov", "Yuri Andoropov", rus_data$v2exnamhos)
rus_data$v2exnamhos <- ifelse(rus_data$v2exnamhos == "Konstantin Ustinovich Chernenko", "Konstantin Chernenko", rus_data$v2exnamhos)
rus_data$v2exnamhos <- ifelse(rus_data$v2exnamhos == "Mikhail Sergeyevich Gorbachev", "Mikhail Gorbachev", rus_data$v2exnamhos)
rus_data$v2exnamhos <- ifelse(rus_data$v2exnamhos == "Boris Nikolayevich Yeltsin", "Boris Yeltsin", rus_data$v2exnamhos)
rus_data$v2exnamhos <- ifelse(rus_data$v2exnamhos == "Vladimir Vladimirovich Putin", "Vladimir Putin", rus_data$v2exnamhos)
rus_data$v2exnamhos <- ifelse(rus_data$v2exnamhos == "Dmitry Anatolyevich Medvedev", "Dmitry Medvedev", rus_data$v2exnamhos)

v2ex_levels <- unique(rus_data$v2exnamhos)

# Color palette
num_colors <- length(v2ex_levels)
palette <- brewer.pal(num_colors, "Set2")

# Plot the lines
ggplot(rus_data, aes(x = year, y = v2x_polyarchy, color = v2exnamhos, 
                     group = interaction(v2exnamhos, consec_years))) + 
  geom_line(size = 1.5) +
  geom_point() +
  labs(x = "Year", y = "Electoral Democracy Index", color = "Russian/Soviet Leader") +
  ggtitle("Electoral Democracy Index by Russian/Soviet Leader Over Time") +
  theme_bw() +
  scale_color_manual(values = setNames(palette, v2ex_levels)) +
  theme_fivethirtyeight(base_size = 11.5) +
  geom_text(aes(label = "Medvedev", x = 2008, y = 0.33),
            color = "#B3B3B3", size = 3, hjust = 0, fontface = "bold", angle = 5) +
  geom_text(aes(label = "Putin", x = 1999.5, y = 0.358),
            color = "#E5C494", size = 4, hjust = 0, fontface = "bold", angle = 310) +
  geom_text(aes(label = "Putin", x = 2014, y = 0.25),
            color = "#E5C494", size = 4, hjust = 0, fontface = "bold", angle = 357) +
  geom_text(aes(label = "Yeltsin", x = 1992.7, y = 0.51),
            color = "#FFD92F", size = 4, hjust = 0, fontface = "bold", angle = 348) +
  geom_text(aes(label = "Gorbachev", x = 1988, y = 0.122),
            color = "#A6D854", size = 4, hjust = 0, fontface = "bold", angle = 65) +
  geom_text(aes(label = "Chernenko", x = 1984.5, y = 0.09),
            color = "#E78AC3", size = 2.5, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Andropov", x = 1982, y = 0.12),
            color = "#8DA0CB", size = 2.5, hjust = 0, fontface = "bold", angle = 65) +
  geom_text(aes(label = "Brezhnev", x = 1968, y = 0.122),
            color = "#FC8D62", size = 4, hjust = 0, fontface = "bold", angle = 7) +
  geom_text(aes(label = "Khrushchev", x = 1953, y = 0.124),
            color = "#66C2A5", size = 4, hjust = 0, fontface = "bold", angle = 5) +
  guides(color = "none")

##### Brazil
####################################################

bra_data <- subset(data, country_text_id == "BRA" & year >= 1985)

# Order the Leaders in chronological order
ordered_v2exnamhog <- unique(bra_data$v2exnamhos[order(bra_data$year)])[!is.na(unique(bra_data$v2exnamhos[order(bra_data$year)]))]

# Used in case a single person serves nonconsecutive terms (Such as Putin)
bra_data$consec_years <- with(bra_data, ave(year, v2exnamhos, FUN = function(x) cumsum(c(1, diff(x) != 1))))

bra_data$v2exnamhos <- ifelse(bra_data$v2exnamhos == "Fernando Alfonso Collor de Mello", "Fernando Collor de Mello", bra_data$v2exnamhos)
bra_data$v2exnamhos <- ifelse(bra_data$v2exnamhos == "Dilma Vana Rousseff", "Dilma Rousseff", bra_data$v2exnamhos)
bra_data$v2exnamhos <- ifelse(bra_data$v2exnamhos == "Jair Messias Bolsonaro", "Jair Bolsonaro", bra_data$v2exnamhos)

v2ex_levels <- unique(bra_data$v2exnamhos)

# Color palette
num_colors <- length(v2ex_levels)
palette <- brewer.pal(num_colors, "Set2")

# Plot the lines
ggplot(bra_data, aes(x = year, y = v2x_polyarchy, color = v2exnamhos, 
                     group = interaction(v2exnamhos, consec_years))) + 
  geom_line(size = 1.5) +
  geom_point() +
  labs(x = "Year", y = "Electoral Democracy Index", color = "Brazilian President") +
  ggtitle("Electoral Democracy Index by Brazilian President Over Time") +
  theme_bw() +
  scale_color_manual(values = setNames(palette, v2ex_levels)) +
  theme_fivethirtyeight(base_size = 12) +
  geom_text(aes(label = "Bolsonaro", x = 2018.5, y = 0.66),
            color = "#B3B3B3", size = 4, hjust = 0, fontface = "bold", angle = 350) +
  geom_text(aes(label = "Temer", x = 2016.3, y = 0.8),
            color = "#E5C494", size = 3, hjust = 0, fontface = "bold", angle = 340) +
  geom_text(aes(label = "Rousseff", x = 2011, y = 0.854),
            color = "#FFD92F", size = 4, hjust = 0, fontface = "bold", angle = 355) +
  geom_text(aes(label = "Lula de Silva", x = 2003.5, y = 0.9),
            color = "#A6D854", size = 3.7, hjust = 0, fontface = "bold", angle = 3) +
  geom_text(aes(label = "Cardoso", x = 1996.5, y = 0.825),
            color = "#E78AC3", size = 4, hjust = 0, fontface = "bold", angle = 3) +
  geom_text(aes(label = "Franco", x = 1991.8, y = 0.87),
            color = "#8DA0CB", size = 3, hjust = 0, fontface = "bold", angle = 4) +
  geom_text(aes(label = "Collor", x = 1990.5, y = 0.813),
            color = "#FC8D62", size = 3, hjust = 0, fontface = "bold", angle = 340) +
  geom_text(aes(label = "Sarney", x = 1985.2, y = 0.5),
            color = "#66C2A5", size = 4, hjust = 0, fontface = "bold", angle = 70) +
  guides(color = "none")

##### United Kingdom
####################################################

gbr_data <- subset(data, country_text_id == "GBR" & year >= 1979)

# Order the Leaders in chronological order
ordered_v2exnamhog <- unique(gbr_data$v2exnamhog[order(gbr_data$year)])[!is.na(unique(gbr_data$v2exnamhog[order(gbr_data$year)]))]

# Used in case a single person serves nonconsecutive terms
gbr_data$consec_years <- with(gbr_data, ave(year, v2exnamhog, FUN = function(x) cumsum(c(1, diff(x) != 1))))

gbr_data$v2exnamhog <- ifelse(gbr_data$v2exnamhog == "Anthony Blair", "Tony Blair", gbr_data$v2exnamhog)

v2ex_levels <- unique(gbr_data$v2exnamhog)

# Color palette
num_colors <- length(v2ex_levels)
palette <- brewer.pal(num_colors, "Set2")

# Plot the lines
ggplot(gbr_data, aes(x = year, y = v2x_polyarchy, color = v2exnamhog, 
                     group = interaction(v2exnamhog, consec_years))) + 
  geom_line(size = 1.5) +
  geom_point() +
  labs(x = "Year", y = "Electoral Democracy Index", color = "British Prime Minister") +
  ggtitle("Electoral Democracy Index by British Prime Minister Over Time") +
  theme_bw() +
  scale_color_manual(values = setNames(palette, v2ex_levels)) +
  theme_fivethirtyeight(base_size = 11.5) +
  geom_text(aes(label = "Sunak", x = 2021, y = 0.839),
            color = "#B3B3B3", size = 3, hjust = 0, fontface = "bold", angle = 5) +
  geom_text(aes(label = "Johnson", x = 2018.5, y = 0.855),
            color = "#E5C494", size = 3.5, hjust = 0, fontface = "bold", angle = 7) +
  geom_text(aes(label = "May", x = 2015.1, y = 0.854),
            color = "#FFD92F", size = 3.5, hjust = 0, fontface = "bold", angle = 353) +
  geom_text(aes(label = "Cameron", x = 2011, y = 0.878),
            color = "#A6D854", size = 4, hjust = 0, fontface = "bold", angle = 340) +
  geom_text(aes(label = "Brown", x = 2006.5, y = 0.865),
            color = "#E78AC3", size = 3.5, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Blair", x = 2002, y = 0.87),
            color = "#8DA0CB", size = 4, hjust = 0, fontface = "bold", angle = 4) +
  geom_text(aes(label = "Major", x = 1991.5, y = 0.818),
            color = "#FC8D62", size = 4, hjust = 0, fontface = "bold", angle = 7) +
  geom_text(aes(label = "Thatcher", x = 1981.5, y = 0.8184),
            color = "#66C2A5", size = 4, hjust = 0, fontface = "bold", angle = 350) +
  guides(color = "none")

##### South Korea
####################################################

kor_data <- subset(data, country_text_id == "KOR" & year >= 1993)

# Order the Leaders in chronological order
ordered_v2exnamhos <- unique(kor_data$v2exnamhos[order(kor_data$year)])[!is.na(unique(kor_data$v2exnamhos[order(kor_data$year)]))]

# Used in case a single person serves nonconsecutive terms
kor_data$consec_years <- with(kor_data, ave(year, v2exnamhos, FUN = function(x) cumsum(c(1, diff(x) != 1))))

v2ex_levels <- unique(kor_data$v2exnamhos)

# Color palette
num_colors <- length(v2ex_levels)
palette <- brewer.pal(num_colors, "Set2")

# Plot the lines
ggplot(kor_data, aes(x = year, y = v2x_polyarchy, color = v2exnamhos, 
                     group = interaction(v2exnamhos, consec_years))) + 
  geom_line(size = 1.5) +
  geom_point() +
  labs(x = "Year", y = "Electoral Democracy Index", color = "South Korean President") +
  ggtitle("Electoral Democracy Index by South Korean President Over Time") +
  theme_bw() +
  scale_color_manual(values = setNames(palette, v2ex_levels)) +
  theme_fivethirtyeight(base_size = 11) +
  geom_text(aes(label = "Yoon", x = 2021.3, y = 0.805),
            color = "#B3B3B3", size = 3, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Moon", x = 2018.2, y = 0.845),
            color = "#E5C494", size = 4, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Hwang", x = 2016.2, y = 0.74),
            color = "#FFD92F", size = 3.5, hjust = 0, fontface = "bold", angle = 353) +
  geom_text(aes(label = "Park", x = 2013.5, y = 0.73),
            color = "#A6D854", size = 3.5, hjust = 0, fontface = "bold", angle = 350) +
  geom_text(aes(label = "Lee", x = 2009.5, y = 0.763),
            color = "#E78AC3", size = 4, hjust = 0, fontface = "bold", angle = 7) +
  geom_text(aes(label = "Roh", x = 2004.2, y = 0.855),
            color = "#8DA0CB", size = 4, hjust = 0, fontface = "bold", angle = 0) +
  geom_text(aes(label = "Kim Dae-jung", x = 1998.4, y = 0.813),
            color = "#FC8D62", size = 3.5, hjust = 0, fontface = "bold", angle = 30) +
  geom_text(aes(label = "Kim Young-Sam", x = 1992.5, y = 0.745),
            color = "#66C2A5", size = 3.3, hjust = 0, fontface = "bold", angle = 20) +
  guides(color = "none")

##### South Africa
####################################################

zaf_data <- subset(data, country_text_id == "ZAF" & year >= 1984)

# Order the Leaders in chronological order
ordered_v2exnamhos <- unique(zaf_data$v2exnamhos[order(zaf_data$year)])[!is.na(unique(zaf_data$v2exnamhos[order(zaf_data$year)]))]

# Used in case a single person serves nonconsecutive terms
zaf_data$consec_years <- with(zaf_data, ave(year, v2exnamhos, FUN = function(x) cumsum(c(1, diff(x) != 1))))

v2ex_levels <- unique(zaf_data$v2exnamhos)

# Color palette
num_colors <- length(v2ex_levels)
palette <- brewer.pal(num_colors, "Set2")

# Plot the lines
ggplot(zaf_data, aes(x = year, y = v2x_polyarchy, color = v2exnamhos, 
                     group = interaction(v2exnamhos, consec_years))) + 
  geom_line(size = 1.5) +
  geom_point() +
  labs(x = "Year", y = "Electoral Democracy Index", color = "South African President") +
  ggtitle("Electoral Democracy Index by South African President Over Time") +
  theme_bw() +
  scale_color_manual(values = setNames(palette, v2ex_levels)) +
  theme_fivethirtyeight(base_size = 11) +
  geom_text(aes(label = "Ramaphosa", x = 2018, y = 0.78),
            color = "#E5C494", size = 3.5, hjust = 0, fontface = "bold", angle = 350) +
  geom_text(aes(label = "Zuma", x = 2011, y = 0.74),
            color = "#FFD92F", size = 4, hjust = 0, fontface = "bold", angle = 353) +
  geom_text(aes(label = "Motlanthe", x = 2008.2, y = 0.8),
            color = "#A6D854", size = 3, hjust = 0, fontface = "bold", angle = 8) +
  geom_text(aes(label = "Mbeke", x = 2001.5, y = 0.715),
            color = "#E78AC3", size = 4, hjust = 0, fontface = "bold", angle = 10) +
  geom_text(aes(label = "Mandela", x = 1994, y = 0.7),
            color = "#8DA0CB", size = 4, hjust = 0, fontface = "bold", angle = 2) +
  geom_text(aes(label = "De Klerk", x = 1990, y = 0.195),
            color = "#FC8D62", size = 4, hjust = 0, fontface = "bold", angle = 30) +
  geom_text(aes(label = "Botha", x = 1984.5, y = 0.21),
            color = "#66C2A5", size = 4, hjust = 0, fontface = "bold", angle = 0) +
  guides(color = "none")
