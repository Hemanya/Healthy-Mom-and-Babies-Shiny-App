library(magrittr)
library(dplyr)
library(shiny)
library(sqldf)
# library(maps)
# library(mapproj)
# library(plotly)
library(leaflet)
library(rgdal)
# library(rgeos)
library(sp)
# library(mapdata)
# library(maptools)
# library(ggthemes)
library(scales)
library(sf)
library(tigris)
library(openxlsx)
library(h2o)
library(rsconnect)

library(caret)

#This function is not called. THis is just show the data visualization done for ML model used in shiny app. 
#The result of this preprocessing is stored in tr,csv which is imported in this app.
# Also, the following file  - indiana-births-and-infant-deaths.csv - is not uploaded on git
# It is not required for the execution of this file but still can be downloaded from https://hub.mph.in.gov/dataset/indiana-births-and-infant-deaths
preprocessForML <- function(){
  data <- read.csv('indiana-births-and-infant-deaths.csv')
  tr<-data
  
  realBlanks <- c(colnames(tr))
  
  for (i in 1:length(realBlanks)){
    tr[which(tr[,realBlanks[i]]=='Unknown'),realBlanks[i]] <- NA
  }
  
  #set all columns to factor
  s <- c(colnames(tr))
  
  for (i in 1:length(s)){
    tr[,s[i]] <- as.factor(tr[,s[i]])
  }
  
  #then set 1 column to numeric
  tr$NUM_BIRTHS_BY_MOTHER <- as.numeric(tr$NUM_BIRTHS_BY_MOTHER)
  
  #remove the Id columns and x30 column in tr
  tr$MOTHER_ID <- NULL
  tr$CHILD_ID <-NULL
  tr$X30 <- NULL
  
  tr<-as.data.frame(tr)
  tr<- tr[,c(27,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]
  
  
  # tr <- read.csv('tr.csv')
  
  return(tr)
  
}

plotFeatureImportance <- function(gbm){
  gbm_var<-h2o.varimp(gbm)
  gbm_var1<- gbm_var[1:23,]
  gbm_var2<- gbm_var1[c(2,3,4,6,8,9,10,12,13,14,15,16,18,19,20,21,22),]
  par(mar=c(10,10,10,10))
  coln<-colnames(gbm_var2)
  gbm_var2<-as.data.frame(gbm_var2)
  gbm_var2$variable <- factor(gbm_var2$variable, levels = unique(gbm_var2$variable)[order(gbm_var2$relative_importance, decreasing = FALSE)])
  plot_ly(
    
    y = gbm_var2$variable,
    
    x = gbm_var2$relative_importance,
    
    names = "Variable Importance",
    
    type = "bar",
    
    orientation = 'h'
    
  )%>%
    layout(
      title = "Variable Importance")
  
}

preprocessForML2<-function(){
  m2data<- read.csv("infant_death_by_county.csv")
  m2data$CNT_BY_UNDERLYING_COD<-NULL
  m2data$CHILD_DEATH_CNTY_POP<- NULL
  m2data$CNTY_YR_DEATH_TOTAL<-NULL
  m2data$PCT_CNTY_YR_TOTAL<-NULL
  m2data$CHILD_DEATH_YR<-NULL
  m2datac<-distinct(m2data)
  m2datac$CHILD_DEATH_CNTY_POP<-NULL
  m2datac$PCT_CNTY_YR_TOTAL<-NULL
  names(m2datac)[1] <-"CNTY"
  dummies <- dummyVars(CNTY_YR_DEATH_TOTAL_PER_10000_CNTY_RES ~ .,data = m2datac)
  ex <- data.frame(predict(dummies, newdata = m2datac))
  names(ex) <- gsub("\\.", "", names(ex))
  m2datac_encoded <- cbind(m2datac$CNTY_YR_DEATH_TOTAL_PER_10000_CNTY_RES,ex)
  names(m2datac_encoded)[1] <- 'y'
  
  
  m2datac_new_x <- m2datac_encoded[,2:ncol(m2datac_encoded)]
  preprocessValues <- preProcess(m2datac_new_x,method = c("range"))
  m2datac_new_x <- predict(preprocessValues,m2datac_new_x)
  
  m2datac_final <- cbind(m2datac_encoded$y, m2datac_new_x)
  names(m2datac_final)[1] <- 'y'
  return(m2datac_final)
}

getDataToPlot <- function(){
  df_deaths_by_counties = read.csv('infant_death_by_county.csv')
  names(df_deaths_by_counties)[1] <-'CNTY'
  df_ob_gyn = read.csv('ob_gyn.csv')
  names(df_ob_gyn)[1] <- 'CNTY'
  df_births = read.csv('birth_count.csv')
  df_births$COUNTY <- trimws(df_births$COUNTY, which = c("right"))
  
  df_mother_stress = read.csv('mother_stress.csv')
  df_mother_stress$COUNTY <- trimws(df_mother_stress$COUNTY, which = c("right"))
  df_mother_er = read.csv('mother_er.csv')
  df_mother_er$COUNTY <- trimws(df_mother_er$COUNTY, which = c("right"))

  df_mother_obese = read.csv('mother_obese.csv')
  df_mother_obese$COUNTY <- trimws(df_mother_obese$COUNTY, which = c("right"))
  df_hospital <- read.xlsx('hospital_inpatient_delivery.xlsx')
  df_hospital$County.Name <- toupper(df_hospital$County.Name)
  names(df_hospital)<- c('COUNTY', 'INPATIENT_DELIVERY', 'HOSPITAL')
  
  df_new <- df_deaths_by_counties %>%
    group_by(CNTY, UNDERLYING_COD) %>%
    summarise_each(funs(sum))

  df_total <- df_deaths_by_counties %>%
    group_by(CNTY) %>%
    summarise_each(funs(mean))

  df_total <- df_total[,c('CNTY', 'CHILD_DEATH_CNTY_POP')]


  df_death_cause <- sqldf("select max(CNT_BY_UNDERLYING_COD) as DEATHS_BY_LEADING_CAUSE,CNTY, UNDERLYING_COD from df_new group by CNTY")
  df_merge <- sqldf("select * from df_death_cause LEFT OUTER JOIN df_ob_gyn on df_death_cause.CNTY = df_ob_gyn.CNTY")
  df_merge[4]<- NULL
  df_merge <- as.data.frame(df_merge)
  df_merge <- as.data.frame(sqldf("select * from df_merge LEFT OUTER JOIN df_births on df_merge.CNTY = df_births.COUNTY"))
  df_merge['COUNTY']<-NULL
  names(df_merge)[5]<- 'BIRTHS'
  df_merge <- as.data.frame(sqldf("select * from df_merge LEFT OUTER JOIN df_mother_stress on df_merge.CNTY = df_mother_stress.COUNTY"))
  df_merge['COUNTY']<-NULL
  names(df_merge)[6]<- 'MOTHERS_STRESS'
  df_merge <- as.data.frame(sqldf("select * from df_merge LEFT OUTER JOIN df_mother_er on df_merge.CNTY = df_mother_er.COUNTY"))
  df_merge['COUNTY']<-NULL
  names(df_merge)[7]<- 'MOTHERS_ER'
  df_merge <- as.data.frame(sqldf("select * from df_merge LEFT OUTER JOIN df_mother_obese on df_merge.CNTY = df_mother_obese.COUNTY"))
  df_merge['COUNTY']<-NULL
  names(df_merge)[8]<- 'MOTHERS_OBESE'
  df_merge <- as.data.frame(sqldf("select * from df_merge LEFT OUTER JOIN df_hospital on df_merge.CNTY = df_hospital.COUNTY"))
  df_merge['COUNTY']<-NULL
  df_merge$INPATIENT_DELIVERY = ifelse(df_merge$INPATIENT_DELIVERY == 'Y', 1,0)
  df_merge$HOSPITAL = ifelse(df_merge$HOSPITAL == 'Y', 1,0)
  df_merge <- as.data.frame(sqldf("select * from df_merge LEFT OUTER JOIN df_total on df_merge.CNTY = df_total.CNTY"))
  df_merge[11] <- NULL
   
  # write.csv(df_merge, paste0(getwd(), '/df_merge.csv'))
  return(df_merge)
  
  
}

tr<- read.csv('tr.csv')
tr <- na.omit(tr)
tr[1]<- NULL
names(tr)[1] <- 'y'

tr_new_x <- tr[,2:ncol(tr)]
preprocessValues <- preProcess(tr_new_x,method = c("range"))
tr_new_x <- predict(preprocessValues,tr_new_x)
tr_final <- cbind(tr$y, tr_new_x)
names(tr_final)[1] <- 'y'




trainSet <- createDataPartition(y = tr_final$y, p = 0.7, list = F)
trainData <- tr_final[trainSet,]
testData <- tr_final[-trainSet,]
uptrain<- upSample(x=trainData[,3:ncol(tr_final)],y=trainData$y)
str(tr_final)

# plot(trainData$y)


# h2o part ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

h2o.init()
h2o_data<- as.h2o(tr_final)

# oversampling ~~~
y <- "y"                                # target variable to learn
x <- setdiff(names(h2o_data), y)


# h2o split dataset ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




parts <- h2o.splitFrame(h2o_data, 0.7, seed=99) 
train <- parts[[1]]                         
test <- parts[[2]]   


#h2o model building~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# h2o gradient boost~~~~~~~~~~~~~~~~~~~~~~~~



gbm<- h2o.gbm(x,y,train,validation_frame = test, ntrees=200, sample_rate = 0.7,
              balance_classes = TRUE, col_sample_rate = 0.3,  
              learn_rate=0.01 ,max_depth=8,
              keep_cross_validation_predictions=TRUE)
h2o.performance(gbm, train)
h2o.performance(gbm, test)

plotFeatureImportance(gbm)



# # SECOND MODEL
# 

m2datac_final = preprocessForML2()
# h2o start
h2o.init()
h2o_data1<- as.h2o(m2datac_final)


y <- "y"                                # target variable to learn
x <- setdiff(names(h2o_data1), y)


parts <- h2o.splitFrame(h2o_data1, 0.7, seed=99)
train <- parts[[1]]
test <- parts[[2]]
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# 
 h2o.init()
# 
# 
cntdata<- h2o.importFile(paste0(getwd(), "/countydataclean.csv"))

y <- "CNTY_YR_DEATH_TOTAL_PER_10000_CNTY_RES"                                # target variable to learn
x <- setdiff(names(cntdata), y)
parts <- h2o.splitFrame(cntdata, 0.7, seed=99)
train <- parts[[1]]
test <- parts[[2]]


m2gbm<- h2o.gbm(x,y,train,validation_frame = test,
                nfolds=5,
                score_tree_interval=5,
                fold_assignment='Modulo',
                ntrees=47,
                max_depth=15,
                min_rows = 100,
                stopping_rounds=3,
                stopping_metric='deviance',
                stopping_tolerance= 0.02469323991623974,
                seed =5802230333091106000,
                distribution= 'gaussian',
                sample_rate= 0.8,
                col_sample_rate= 0.8,
                col_sample_rate_per_tree= 0.8)

h2o.performance(m2gbm, train)
h2o.performance(m2gbm, test)

df_merge <- getDataToPlot()
# df_merge <- read.csv('df_merge.csv')
# df_merge[1] <- NULL #Run this line if you are importing the csv instead of calling the function

#Get the shape file for Indiana counties map
raw_tract <- readOGR(dsn = 'cb_2014_us_county_5m', layer = 'cb_2014_us_county_5m')

#Merge the spatial dataframe with df_merge to plot our data on the map
temp <- st_as_sf(raw_tract)
nhgeo <- filter(temp, STATEFP=="18")
nhgeo$NAME <- toupper(nhgeo$NAME)
merge <- geo_join(nhgeo,df_merge, "NAME", "CNTY")
names(merge)[10]<- "DEATHS_BY_LEADING_CAUSE"
names(merge)[19] <- "AVERAGE_CHILDREN_DEATH"


#UI For Shiny App
#A navbar page with 2 tabPanels. One for visualization. Other for ML

ui <- navbarPage("Healthy Babies and Moms",
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = 'drop', choices = c('Births Between July 2016 and 2017','Average Yearly Deaths between 2010 and 2018',
                                                                        'Leading Cause of Death'), label = 'Text Options', selected = 'Average Yearly Deaths between 2010 and 2018', multiple = FALSE),
                              selectInput(inputId = 'drop2', choices = c('Counties Having Hospitals','Counties Having Inpatient Delivery',
                                                                         'Count of Mothers Diagnosed with Obesity', 'Mothers with an ER Visit',
                                                                         'Average Yearly Deaths between 2010 and 2018',
                                                                         'Mothers with a Stress Related Claim',
                                                                         'Death by Leading Cause'
                              ), label = 'Color Options', selected = 'Average Yearly Deaths between 2010 and 2018',
                              multiple = FALSE))
                            ,
                            mainPanel(
                              leafletOutput("mymap", height = 800)
                            )
                          )
                 ),
                 tabPanel("Machine Learning",
                          sidebarLayout(
                            
                            
                            sidebarPanel(
                              
                              radioButtons("low_w", "If your baby born with low weight?",choices = c("N", "Y")),
                              
                              numericInput('numbirth', "number births by mom ", value = 2,min=1, max=6),
                              
                              selectInput('diab', 'If mother have diabetes ? ',
                                          choices = c('N',"Y"),
                                          multiple =FALSE),
                              
                              selectInput('chdtran', 'If the child been transferred? ',
                                          choices = c('N',"Y"),
                                          multiple =FALSE),
                              selectInput('smk', 'Did the mom smoke ? ',
                                          choices = c('N',"Y"),
                                          multiple =FALSE),
                              selectInput('gen', 'what is the gender of the child ? ',
                                          choices = c('Male',"Female"),
                                          multiple =FALSE),
                              selectInput('year', 'what is year that child birth ? ',
                                          choices = c('2010 - 2012',"2013 - 2015",'2016 - 2018'),
                                          multiple =FALSE),
                              
                              radioButtons('chdbre', "If the child has been brestfed? " ,
                                           choices=c('N',"Y"))
                              
                            ),
                            
                            mainPanel(
                              
                              dataTableOutput("table")
                              # verbatimTextOutput ('test')
                            )
                            
                          )
                 )
)


server <- function(input, output, session) {
  
  output$mymap <- isolate(renderLeaflet({
    # pal<- colorNumeric( palette = "Reds",
    #      domain = merge$AVERAGE_CHILDREN_DEATH)
    popup <- isolate(paste0("County: ", merge$NAME, 
                            "<br>","Average Children Death: ", merge$AVERAGE_CHILDREN_DEATH))
    if (input$drop == 'Average Yearly Deaths between 2010 and 2018') {
      popup <- isolate(paste0("County: ", merge$NAME, 
                              "<br>","Average Children Death: ", merge$AVERAGE_CHILDREN_DEATH))
      
    }
    if (input$drop == 'Leading Cause of Death') {
      popup <- isolate(paste0("County: ", merge$NAME, 
                              "<br>","Deaths by leading cause: ", merge$DEATHS_BY_LEADING_CAUSE,
                       "<br>","Leading cause: ", merge$UNDERLYING_COD))
      
    }
    if(input$drop == 'Births Between July 2016 and 2017')
    {
      popup <- isolate(paste0("County: ", merge$NAME, 
                              "<br>","Births Between July 2016 and 2017: ", merge$BIRTHS))
      
    }
    
    l= leaflet()
    
    if (input$drop2 == 'Average Yearly Deaths between 2010 and 2018' ){
      pal<- colorNumeric( palette = "Reds",
                          domain = merge$AVERAGE_CHILDREN_DEATH)
      
    l %>%
        
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = merge, 
                  fillColor = ~pal(AVERAGE_CHILDREN_DEATH), 
                  color = "#696969", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup) %>%
      addLegend(pal = pal, 
                values = merge$AVERAGE_CHILDREN_DEATH, 
                position = "bottomleft", 
                title = 'Average infant mortality per year between 2010 and 2018'
      ) 
    }
    else if (input$drop2 == 'Counties Having Hospitals' ){
      pal<- colorNumeric( palette = "Reds",
                          domain = merge$HOSPITAL)
      l %>%
        
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = merge, 
                    fillColor = ~pal(HOSPITAL), 
                    color = "#696969", # you need to use hex colors
                    fillOpacity = 0.7, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup = popup) %>%
        addLegend(pal = pal, 
                  values = merge$HOSPITAL, 
                  position = "bottomleft", 
                  title = 'Counties with Hospitals
                  1 = Yes
                  0 = No'
        ) 
    }
    else if (input$drop2 == 'Counties Having Inpatient Delivery' ){
      pal<- colorNumeric( palette = "Reds",
                          domain = merge$INPATIENT_DELIVERY)
      l%>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = merge, 
                    fillColor = ~pal(INPATIENT_DELIVERY), 
                    color = "#696969", # you need to use hex colors
                    fillOpacity = 0.7, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup = popup) %>%
        addLegend(pal = pal, 
                  values = merge$INPATIENT_DELIVERY, 
                  position = "bottomleft", 
                  title = 'Counties with Inpatient Deliveries
                  1 = Yes
                  0 = No'
        ) 
    }
    else if (input$drop2 == 'Mothers with an ER Visit' ){
      pal<- colorNumeric( palette = "Reds",
                          domain = merge$MOTHERS_ER)
      l %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = merge, 
                    fillColor = ~pal(MOTHERS_ER), 
                    color = "#696969", # you need to use hex colors
                    fillOpacity = 0.7, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup = popup) %>%
        addLegend(pal = pal, 
                  values = merge$MOTHERS_ER, 
                  position = "bottomleft", 
                  title = 'Mothers with an ER visit 2 years prior to delivery'
        ) 
    }
    else if (input$drop2 == 'Count of Mothers Diagnosed with Obesity' ){
      pal<- colorNumeric( palette = "Reds",
                          domain = merge$MOTHERS_OBESE)
      l %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = merge, 
                    fillColor = ~pal(MOTHERS_OBESE), 
                    color = "#696969", # you need to use hex colors
                    fillOpacity = 0.7, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup = popup) %>%
        addLegend(pal = pal, 
                  values = merge$MOTHERS_OBESE, 
                  position = "bottomleft", 
                  title = 'Count of Mothers Diagnosed with Obesity'
        ) 
    }
    else if (input$drop2 == 'Mothers with a Stress Related Claim' ){
      pal<- colorNumeric( palette = "Reds",
                          domain = merge$MOTHERS_STRESS)
      l %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = merge, 
                    fillColor = ~pal(MOTHERS_STRESS), 
                    color = "#696969", # you need to use hex colors
                    fillOpacity = 0.7, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup = popup) %>%
        addLegend(pal = pal, 
                  values = merge$MOTHERS_STRESS, 
                  position = "bottomleft", 
                  title = 'Mothers with a Stress Related Claim'
        ) 
    }
    else if (input$drop2 == 'Death by Leading Cause' ){
      pal<- colorNumeric( palette = "Reds",
                          domain = merge$DEATHS_BY_LEADING_CAUSE)
      l %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = merge, 
                    fillColor = ~pal(DEATHS_BY_LEADING_CAUSE), 
                    color = "#696969", # you need to use hex colors
                    fillOpacity = 0.7, 
                    weight = 1, 
                    smoothFactor = 0.2,
                    popup = popup) %>%
        addLegend(pal = pal, 
                  values = merge$DEATHS_BY_LEADING_CAUSE, 
                  position = "bottomleft", 
                  title = 'Deaths by the Leading Cause'
        ) 
    }
  }))
  output$table<- renderDataTable({
    tr_final[45834,'LOW_BIRTH_WEIGHT']<-input$low_w
    tr_final[45834,'NUM_BIRTHS_BY_MOTHER']<-input$numbirth
    tr_final[45834,'CHILD_TRANSFERRED']<-input$chdtran
    tr_final[45834,'CHILD_BREASTFED']<-input$chdbre
    year<- as.factor(input$year)
    tr_final[45834,'CHILD_BIRTH_YR_GRP'] <- year
    tr_final[45834,'MOTHER_AGE_GRP']<-'25-35 Years'
    tr_final[45834,'MOTHER_MARITALSTATUS_AT_BIRTH']<-'Married'
    tr_final[45834,'MOTHER_RESID_COUNTY_TYPE']<-'Urban'
    tr_final[45834,'BIRTH_COMPLICATIONS']<-'N'
    tr_final[45834,'DIABETES_RISK_PREPREGNANCY']<-input$diab
    tr_final[45834,'DIABETES_RISK_GESTATIONAL']<-'N'
    tr_final[45834,'SMOKING_IND']<-input$smk
    tr_final[45834,'SMOKING_DURING_PREG_IND']<-input$smk
    tr_final[45834,'SMOKING_BEFORE_PREG_IND']<-input$smk
    tr_final[45834,'VISITS_IN_1ST_TRIMESTER_IND']<-'Y'
    tr_final[45834,'UNDERGRAD_DEGREE_IND']<-'Y'
    tr_final[45834,'GRADUATE_DEGREE_IND']<-'Y'
    tr_final[45834,'COLLEGE_ENROLLMENT_IND']<-'Y'
    tr_final[45834,'HIGH_SCHOOL_DIPLOMA_IND']<-'Y'
    tr_final[45834,'HIGH_SCHOOL_NON_COMPLETER_IND']<-'Y'
    tr_final[45834,'WAGE_BIRTH_QTR_IND']<-'Y'
    tr_final[45834,'MULTI_WAGE_BIRTH_QTR_IND']<-'Y'
    tr_final[45834,'WAGE_THRD_QTR_BEFORE_BIRTH_IND']<-'Y'
    tr_final[45834,'MULTI_WAGE_THRD_QTR_BEFORE_BIRTH_IND']<-'Y'
    tr_final[45834,'CHILD_GENDER']<-input$gen
    tr_final[45834,'CHILD_ALIVE']<-'Y'
    
    
    newob<-tr_final[nrow(tr_final),]
    newob<-as.h2o(newob)
    pre<-h2o.predict(gbm,newob)
    
    
    names(pre)[3]<- 'Risk'
    
    
    pre[,c("Risk")]
    
    
  } )
}

shinyApp(ui, server)


