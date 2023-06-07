# Define server logic
server <- function(input, output) {
  library(shinydashboard)
  library(markdown)
  library(thematic)
  library(rsconnect)
  thematic_on()
  
  data <- read.csv("StaffingDataSet.csv")
  data <- data[rowSums(is.na(data))==0,]
  data$Median.Household.Income <- as.numeric(gsub("\\$","", gsub(",","", 
                                                                 data$Median.Household.Income)))
  data$X..change.18.23 <- as.numeric(gsub("%","",data$X..change.18.23))
  data$X..change.20.23 <- as.numeric(gsub("%","",data$X..change.20.23))
  
  output$sum <- renderTable({
    subset_data <- subset(data, data$X2018.2023 == input$direction)
    subsum <- as.matrix(summary(subset_data$X..change.18.23))
    colnames(subsum) <- c("Quantity")
    subsum
  }, rownames = TRUE)  
  
  output$vars <- renderTable({
    variables <- matrix(c("Column one contains the name of the school district(character)","Column two contains the name of the town that attends the district(character)", "Column three has the population of the town (integer)", "Column four has the median household income of the town (numeric)", "Column five has the equalized net grand list per capita of the town (numeric)", "Columns six through ten have the number of staff in the district in the 2018-19, 2019-20, 2020-21, 2021-22, and 2022-23 school years (all numeric)", "Columns eleven and twelve both contain strings that either state increased or decreased, this is indicating if the number of staff increased or decreased from 2018 to 2023 and 2020 to 2023 respectively", "Columns thirteen and fourteen have the percent change in the number of staff from 2018-2023 and 2020-2023 respectively (numeric)"),ncol=1, byrow=TRUE)
    colnames(variables) <- c('Variable Description')
    rownames(variables) <- c('District Name', 'Town Name', 'Population', 'Median Household Income', 'Equalized Grand Net List Per Capita', 'Number of Staff', 'Increase or Decrease', 'Percent Change')
    variables
  }, rownames = TRUE)
  
  output$freq <- renderTable({
    if(input$years == 1){
      var <- data$X2018.2023
    } else{
      var <- data$X2020.2023
    }
    frequ <- as.matrix(with(data, table(var)))
    colnames(frequ) <- c("Number of Districts")
    frequ
  }, rownames = TRUE)
  
  output$box1 <- renderPlot({
    boxplot(data$X..change.18.23, data$X..change.20.23, main="Boxplots of Changes in Staffing Levels", 
            ylab="Percent Change", names=c("2018-2023", "2020-2023"), col="lightskyblue3")
  })
  
  output$hists <- renderPlot({
    if(input$category == 1){
      hist(data$Population, main="Histogram of Population", 
           xlab="Population Size", col="lightskyblue3")
    } else if(input$category == 2){
      hist(data$Median.Household.Income, main="Histogram of Median Household Income", 
           xlab="Median Household Income ($)", col="lightskyblue3")
    } else{
      hist(data$Equalized.Net.Grand.List.Per.Capita, main="Histogram of Equalized Net Grand List", 
           xlab="Equalized Net Grand List per Capita ($)", col="lightskyblue3")
    }
  })
  
  output$means <- renderPlot({
    yearsdf <- data.frame(years = c('2018-19', '2019-20', '2020-21','2021-22',
                                    '2022-23'), means = c(mean(data$X2018.19),
                                                          mean(data$X2019.20), mean(data$X2020.21),
                                                          mean(data$X2021.22), mean(data$X2022.23)))
    plot(yearsdf$means, pch=20, xlab="Years", ylim = c(80, 115),
         ylab="Average Number of Staff", xaxt="n", main = "Average Number of Staff for Each Year Across All Districts", col="lightskyblue3")
    axis(1, at=1:5, labels=yearsdf$years)
    lines(yearsdf$means,xlim=range(yearsdf$means), pch=16)
    
  })
  output$histsizetitle <- renderText({
    if(input$size == 1){
      "Number of staff in towns with a population < 7,000 (histograms)"
    } else if(input$size == 2){
      "Number of staff in towns with 7,000<Population<20,000 (histograms)"
    } else{
      "Number of staff in towns with Population > 20,000 (histograms)"
    }
  })
  
  output$sizeyearstitle <- renderText({
    if(input$size == 1){
      "Number of staff in towns with a population < 7,000 over the years"
    } else if(input$size == 2){
      "Number of staff in towns with 7,000<Population<20,000 over the years"
    } else{
      "Number of staff in towns with Population > 20,000 over the years"
    }
  })
  
  output$histsize <- renderPlot({
    smallpop <- subset(data, data$Population<=7000)
    largepop <- subset(data, data$Population>=20000)
    medpop <- subset(data, data$Population>7000 & data$Population<20000)
    if(input$size == 1){
      par(mfrow=c(2,3))
      hist(smallpop$X2018.19, main='2018-2019', col="lightskyblue3")
      hist(smallpop$X2019.20, main='2019-2020', col="lightskyblue3")
      hist(smallpop$X2020.21, main='2020-2021', col="lightskyblue3")
      hist(smallpop$X2021.22, main='2021-2022', col="lightskyblue3")
      hist(smallpop$X2022.23, main='2022-2023', col="lightskyblue3")
    } else if(input$size == 2){
      par(mfrow=c(2,3))
      hist(medpop$X2018.19, main='2018-2019', col="lightskyblue3")
      hist(medpop$X2019.20, main='2019-2020', col="lightskyblue3")
      hist(medpop$X2020.21, main='2020-2021', col="lightskyblue3")
      hist(medpop$X2021.22, main='2021-2022', col="lightskyblue3")
      hist(medpop$X2022.23, main='2022-2023', col="lightskyblue3")
    } else{
      par(mfrow=c(2,3))
      hist(largepop$X2018.19, main='2018-2019', col="lightskyblue3")
      hist(largepop$X2019.20, main='2019-2020', col="lightskyblue3")
      hist(largepop$X2020.21, main='2020-2021', col="lightskyblue3")
      hist(largepop$X2021.22, main='2021-2022', col="lightskyblue3")
      hist(largepop$X2022.23, main='2022-2023', col="lightskyblue3")
    }
  })
  
  output$yearsbysize <- renderPlot({
    smallpop <- subset(data, data$Population<=7000)
    largepop <- subset(data, data$Population>=20000)
    medpop <- subset(data, data$Population>7000 & data$Population<20000)
    if(input$size == 1){
      smallpopavg <- data.frame(years = c('2018-19', '2019-20', '2020-21','2021-22',
                                          '2022-23'), means = c(mean(smallpop$X2018.19),
                                                                mean(smallpop$X2019.20), mean(smallpop$X2020.21),
                                                                mean(smallpop$X2021.22), mean(smallpop$X2022.23)))
      plot(smallpopavg$means, pch=20, xlab="Years", ylab="Average Number of Staff",
           xaxt="n", main="Number of staff in small towns over the years", col="lightskyblue3")
      axis(1, at=1:5, labels=smallpopavg$years)
      lines(smallpopavg$means,xlim=range(smallpopavg$means), pch=16)
    } else if(input$size == 2){
      medpopavg <- data.frame(years = c('2018-19', '2019-20', '2020-21','2021-22',
                                        '2022-23'), means = c(mean(medpop$X2018.19),
                                                              mean(medpop$X2019.20), mean(medpop$X2020.21),
                                                              mean(medpop$X2021.22), mean(medpop$X2022.23)))
      plot(medpopavg$means, pch=20, xlab="Years", ylab="Average Number of Staff",
           xaxt="n", main="Number of staff in medium towns over the years", col="lightskyblue3")
      axis(1, at=1:5, labels=medpopavg$years)
      lines(medpopavg$means,xlim=range(medpopavg$means), pch=16)
    } else{
      largepopavg <- data.frame(years = c('2018-19', '2019-20', '2020-21','2021-22',
                                          '2022-23'), means = c(mean(largepop$X2018.19),
                                                                mean(largepop$X2019.20), mean(largepop$X2020.21),
                                                                mean(largepop$X2021.22), mean(largepop$X2022.23)))
      plot(largepopavg$means, pch=20, xlab="Years", ylab="Average Number of Staff",
           xaxt="n", main="Number of staff in large towns over the years", col="lightskyblue3")
      axis(1, at=1:5, labels=largepopavg$years)
      lines(largepopavg$means,xlim=range(largepopavg$means), pch=16)
    }})
  





