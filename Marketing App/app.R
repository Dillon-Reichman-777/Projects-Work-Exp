

library(shiny)
library(shinythemes)
library(plotly)
library(readxl)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(waiter)
library(htmlwidgets)
library(DT)

library(bslib) # for shiny themes

library(readxl)

library(e1071)

library(dplyr) # data manipulation
library(stringr)
library(caTools) #roc curve and auc
library(caret) # classification and regression
library(ROCR)

options(scipen=999) # Turn off scientific mode

## Remove the following to be able to view in own window:

# viewer <- getOption("viewer")
# viewer("http://127.0.0.1:5148")
#runApp("C:\\Users\\User\\Documents\\Marketing_Research_App", launch.browser = rstudioapi::viewer)

###################################### Loading and Preparing Data ####################################                 


cleaned_1 <- read_excel("cleaned_1.xlsx", 
                        sheet = "Data")

data_agg <- read_excel("final_cleaned.xlsx", 
                       sheet = "Aggregation")

Success_Rates <- read_excel("Success_Rates.xlsx")

Estimates <- read_excel("Estimates.xlsx")

source("Functions.R")

cleaned_1<-data.frame(cleaned_1)

names(cleaned_1)[names(cleaned_1) == 'AcceptedCmp1'] <- 'Response to Campaign 1'
names(cleaned_1)[names(cleaned_1) == 'AcceptedCmp2'] <- 'Response to Campaign 2'
names(cleaned_1)[names(cleaned_1) == 'AcceptedCmp3'] <- 'Response to Campaign 3'
names(cleaned_1)[names(cleaned_1) == 'AcceptedCmp4'] <- 'Response to Campaign 4'
names(cleaned_1)[names(cleaned_1) == 'AcceptedCmp5'] <- 'Response to Campaign 5'

cleaned_1$"Response to Campaign 1"=factor(cleaned_1$"Response to Campaign 1",levels=c(0, 1),labels=c("No", "Yes"))
cleaned_1$"Response to Campaign 2"=factor(cleaned_1$"Response to Campaign 2",levels=c(0, 1),labels=c("No", "Yes"))
cleaned_1$"Response to Campaign 3"=factor(cleaned_1$"Response to Campaign 3",levels=c(0, 1),labels=c("No", "Yes"))
cleaned_1$"Response to Campaign 4"=factor(cleaned_1$"Response to Campaign 4",levels=c(0, 1),labels=c("No", "Yes"))
cleaned_1$"Response to Campaign 5"=factor(cleaned_1$"Response to Campaign 5",levels=c(0, 1),labels=c("No", "Yes"))

cleaned_1$AcceptedCmp6<-factor(cleaned_1$AcceptedCmp6,levels=c(0, 1),labels=c("No", "Yes"))
cleaned_1$Country<-factor(cleaned_1$Country,levels=c("AUS", "CAN", "DEU", "ESP", "IND", "SAU", "USA"),labels=c("Australia","Canada","Germany","Spain","India","Saudi Arabia","USA"))
cleaned_1$Education<-factor(cleaned_1$Education, labels = c("Basic","Graduate","Masters","PhD"))
cleaned_1$Marital_Status<-as.factor(cleaned_1$Marital_Status)
cleaned_1$Teenhome<-as.factor(cleaned_1$Teenhome)
cleaned_1$Complain<-factor(cleaned_1$Complain, levels=c(0,1),labels=c("No","Yes"))

names(cleaned_1)[names(cleaned_1) == 'Education'] <- 'Education Level'
names(cleaned_1)[names(cleaned_1) == 'Complain'] <- 'Customer made a complaint in the last 2 years'
names(cleaned_1)[names(cleaned_1) == 'Marital_Status'] <- 'Marital Status'
names(cleaned_1)[names(cleaned_1) == 'MntWines'] <- 'Amount spent on wine'
names(cleaned_1)[names(cleaned_1) == 'MntFruits'] <- 'Amount spent on fruit'
names(cleaned_1)[names(cleaned_1) == 'MntMeatProducts'] <- 'Amount spent on meat'
names(cleaned_1)[names(cleaned_1) == 'MntFishProducts'] <- 'Amount spent on fish'
names(cleaned_1)[names(cleaned_1) == 'MntSweetProducts'] <- 'Amount spent on sweets'
names(cleaned_1)[names(cleaned_1) == 'MntGoldProds'] <- 'Amount spent on gold products'
names(cleaned_1)[names(cleaned_1) == 'NumDealsPurchases'] <- 'Number of purchases made with discounts'
names(cleaned_1)[names(cleaned_1) == 'NumWebPurchases'] <- 'Number of purchases made through website'
names(cleaned_1)[names(cleaned_1) == 'NumCatalogPurchases'] <- 'Number of purchases made using a catalogue'
names(cleaned_1)[names(cleaned_1) == 'NumStorePurchases'] <- 'Number of purchases made directly in stores'
names(cleaned_1)[names(cleaned_1) == 'Total_Camp'] <- 'Number of previous campaigns taken up the offer for'
names(cleaned_1)[names(cleaned_1) == 'Total_Spent'] <- 'Total Spent'
names(cleaned_1)[names(cleaned_1) == 'Total_Purchases'] <- 'Total Purchases'
names(cleaned_1)[names(cleaned_1) == 'NumWebVisitsMonth'] <- "Number of visits to company's website in the last month"
names(cleaned_1)[names(cleaned_1) == 'Teenhome'] <- "Number of teenagers in customer's household"
names(cleaned_1)[names(cleaned_1) == 'AcceptedCmp6'] <- "Response to Campaign 6"

cleaned_1<-subset(cleaned_1,select=-Country_2)

# Reording columns in data set

cleaned_1 = cleaned_1[c( "ID", "Year_Birth",                                          
                         "Age" ,"Education Level" ,                                       
                         "Marital Status" , "Income","Country",                                              
                         "Number of teenagers in customer's household",  "Recency" ,  "Customer made a complaint in the last 2 years" ,                                              
                         "Amount spent on wine" , "Amount spent on fruit",                                  
                         "Amount spent on meat", "Amount spent on fish",                                   
                         "Amount spent on sweets" ,"Amount spent on gold products",                          
                         "Total Spent","Number of purchases made with discounts",                
                         "Number of purchases made through website" ,"Number of purchases made using a catalogue" ,            
                         "Number of purchases made directly in stores","Total Purchases",                                        
                         "Number of visits to company's website in the last month", "Response to Campaign 1" ,                                
                         "Response to Campaign 2","Response to Campaign 3" ,                                
                         "Response to Campaign 4", "Response to Campaign 5",                                 
                         "Number of previous campaigns taken up the offer for", "Response to Campaign 6")]

# Creating a new data set consisting of only the numeric variables for heatmap

dataset_numeric = select_if(cleaned_1, is.numeric)

dataset_numeric = subset(dataset_numeric,select=-c(ID,Year_Birth))

dataset_numeric=dataset_numeric[,order(colnames(dataset_numeric))]

names(dataset_numeric)[names(dataset_numeric) == 'Number of purchases made with discounts'] <- '# of purchases made with discounts'
names(dataset_numeric)[names(dataset_numeric) == 'Number of purchases made through website'] <- '# of purchases made through website'
names(dataset_numeric)[names(dataset_numeric) == 'Number of purchases made using a catalogue'] <- '# of purchases made using a catalogue'
names(dataset_numeric)[names(dataset_numeric) == 'Number of purchases made directly in stores'] <- '# of purchases made directly in stores'
names(dataset_numeric)[names(dataset_numeric) == 'Number of previous campaigns taken up the offer for'] <- '# of previous campaigns taken up the offer for'
names(dataset_numeric)[names(dataset_numeric) == "Number of visits to company's website in the last month"] <- "# of visits to company's website in the last month"
names(dataset_numeric)[names(dataset_numeric) == "Number of teenagers in customer's household"] <- "# of teenagers in customer's household"

correlation <- round(cor(dataset_numeric), 3)

pal=c("red","green")

######################### Preparing LR Model Results ###################

# Preparing dataframe with model's covariates

final_data= subset(cleaned_1,select=-c(ID,Year_Birth,`Response to Campaign 1`,`Response to Campaign 2`,
                                       `Response to Campaign 3`,`Response to Campaign 4`,`Response to Campaign 5`,Age,
                                       `Total Purchases`,`Income`,`Amount spent on wine`,`Amount spent on fruit`,`Amount spent on fish`,`Amount spent on sweets`,`Amount spent on meat`,`Amount spent on gold products`))


final_data$`Number of teenagers in customer's household`<- as.numeric(final_data$`Number of teenagers in customer's household`)

predict_data=subset(final_data,select = -`Response to Campaign 6`)

predict_data_names = names(predict_data)

# model training and evaluation

set.seed(42)
sampleSplit <- sample.split(Y=final_data$`Response to Campaign 6`, SplitRatio=0.7)
trainSet <- subset(x=final_data, sampleSplit==TRUE)
testSet <- subset(x=final_data, sampleSplit==FALSE)


# Trained model

logistic_trained<-glm(`Response to Campaign 6`~.,family = "binomial",data = trainSet)
summary(logistic_trained)

# Creating table of results

results=data.frame(summary(logistic_trained)$coefficients) #save the model output as a dataframe

colnames(results)[4] <- "P-value"  # rename p-value column which is in 3rd position
results$`P-value`=round(results$`P-value`,3)
results$Estimate=round(results$`Estimate`,3)
results$Std..Error=round(results$`Std..Error`,3)
results$z.value=round(results$`z.value`,3)
colnames(results) <- c("Estimate","Standard Error", "Z-value","P-value")

rownames(results)[2] <- "Education Level (ref = Basic level): Graduate"
rownames(results)[3] <- "Education Level (ref = Basic level): Masters"
rownames(results)[4] <- "Education Level (ref = Basic level): PhD"
rownames(results)[5] <- "Marital Status (ref = Divorced): Married"
rownames(results)[6] <- "Marital Status (ref = Divorced): Single"
rownames(results)[7] <- "Marital Status (ref = Divorced): Together"
rownames(results)[8] <- "Marital Status (ref = Divorced): Window"
rownames(results)[9] <- "Country (ref = Australia): Canada"
rownames(results)[10] <- "Country (ref = Australia): Germany"
rownames(results)[11] <- "Country (ref = Australia): Spain"
rownames(results)[12] <- "Country (ref = Australia): India"
rownames(results)[13] <- "Country (ref = Australia): Saudi Arabia"
rownames(results)[14] <- "Country (ref = Australia): USA"
rownames(results)[15] <- "Number of teenagers in customer's household"
rownames(results)[17] <- "Customer made a complaint in the last 2 years (ref = No)"
rownames(results)[18] <- "Total spend"
rownames(results)[19] <- "Number of purchases made with discounts"
rownames(results)[20] <- "Number of purchases made through website"
rownames(results)[21] <- "Number of purchases made using a catalogue"
rownames(results)[22] <- "Number of purchases made directly in stores"
rownames(results)[23] <- "Number of visits to company's website in the last month"
rownames(results)[24] <- "Number of previous campaigns taken up the offer for"

results$`P-value` <- ifelse(results$`P-value`<0.00001, "<.001",results$`P-value`)

results$`P-value` <- paste(results$`P-value`, ifelse(results$`P-value`<0.05,"*",""))

# model performance


trainSet$prediction <- predict(logistic_trained, newdata = trainSet, type = "response")
testSet$prediction  <- predict(logistic_trained, newdata = testSet , type = "response")

######################### Preparing Table for Model prediction ##########################

c <-c("Education Level: Basic", "Education Level: Graduate",  "Education Level: Masters", "Education Level: PhD",                
      "Marital Status: Divorced", "Marital Status: Married", "Marital Status: Single", "Marital Status: Together","Marital Status: Window" ,"Country: Australia",                 
      "Country: Canada", "Country: Germany","Country: Spain", "Country: India" ,                       
      "Country: Saudi Arabia", "Country: USA","Number of teenagers in customer's household" , "Recency",                                                 
      "Did the customer made a complaint in the last 2 years?", "Total spend" ,"Number of purchases made with discounts", "Number of purchases made through website",                
      "Number of purchases made using a catalogue", "Number of purchases made directly in stores",            
      "Number of visits to company's website in the last month",  "Number of previous campaigns taken up the offer for" )

d <- c(0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,2,1,0,1560,5,8,1,11,15,3)

matrix = matrix(nrow = 26,
                ncol = 1)



matrix = matrix(nrow = 26,
                ncol = 1)


Table =data.frame(matrix)
Table$Attribute= c


names(Table)[[1]] <- "Observation"

Table = Table[c("Attribute","Observation")]
Table$Observation=d

Estimate <- results[1,1]+ sum(Table$Observation*Estimates$Estimate)

Predicted_probability <- exp(Estimate)/(1+exp(Estimate))




######################### Preparing Environments for Shiny Links ###################

code <- list(
    r = paste0(readLines("R/shinyLink.R"), collapse = "\n"),
    js = paste0(readLines("www/shinyLink.js"), collapse = "\n")
)

############################### UI #################################################

ui <- tagList(
    tags$head(
        tags$link(rel = "stylesheet"),
        tags$title("Marketing Research")
    ),
    

    
    fluidPage(#downloadButton("report", "Generate report")
      
      tags$head(
        tags$style(HTML("
                    #contents
                    {
                      color: black;
                      background: grey;
                      font-family: 'calibri';
                      font-size: 16px;
                      #font-weight: bold;
                    font-style: italic;
                    } "))),
      
       #tags$style(type = 'text/css', '#contents {font-size: 16px; font-family: calibri light; background-color: rgba(255,255,255,0.40); color: black; border-style: none;}'),
      
      #theme = bs_theme(),
      
      #theme = bs_theme(version = 4, bootswatch = "minty"),
      
      theme = shinytheme("flatly"),
           # tags$head(
            #  tags$link(rel = "stylesheet",type="text/css",href="bootstrap.min.css")) , 
      
        #shinythemes::themeSelector(),
        #use_waiter(),
        titlePanel(tags$desc(tags$img(src = "lion.png",height="50px",width="50px"),"Marketing Predictive Analytics")),
############################### Overview Page #######################################    
        
        #Overview page
        tabsetPanel(
            tabPanel("Overview",
                     fluidRow(
                         column(8, 
                                verticalLayout(h2("Description of Project"),
                                               p(style="text-align: justify;font-size:110%",align="left",
                                                 "This dashboard presents some visualizations 
                                              and predictive data analytics pertaining to marketing 
                                              research. The purpose of the project was to determine 
                                              the characteristics and spending behaviour of customers that influence 
                                              their response to a marketing campaign."),HTML("<br>"),
                                               
                                               p(style="text-align: justify;font-size:110%",align="left",
                                                 "The data was collected through a loyalty program for a big
                                              supermarket chain. Every time a customer shops at one of the stores
                                              and swipes their loyalty card, information pertaining to that spend
                                              is collected. The data collected is then used towards the supermarket's
                                              marketing research efforts."),HTML("<br>"),
                                               
                                               p(style="text-align: justify;font-size:110%",align="left",
                                                 "For this current marketing research initiative, a sample of the customers on
                                              the loyaty program were each exposed to 6 marketing campaigns at various time points.
                                              A customer's response to each campaign was recorded, where they either
                                              took up the offer of the campaign or not. Other information recorded in
                                              the data included some of the customers' characteristics, such as basic
                                              demographic information and country of residence, as well as their spending
                                              habits on various products at this supermarket chain. The data collected can
                                              be viewed in the", shinyLink(to = "Table", label = "Data Page"),". Descriptions and visualisations
                                              pertaining to the basic characteristics of the sample are presented in the",
                                                 shinyLink(to = "Characteristics", label = "Data Characteristics Page"),"."),HTML("<br>"),
                                               
                                               p(style="text-align: justify;font-size:110%",align="left",
                                                 "To aid in creating a more targeted marketing approach, the characteristics
                                             and spending behaviour of a customer are further used to predict
                                             the probability of them taking up an offer to a campaign, where
                                             only those customers with a high likelihood of taking up the
                                             offer will be exposed to the campaign in future. Particular focus is on
                                             the customer's response to the last campaign, given by campaign 6.
                                             The", shinyLink(to = "Vis1", label = "Visualisations Page"), "provides
                                             some graphical displays of the customer's response to campaing 6 according to
                                             their characteristics and spending habits. The results of the predictive
                                              model are presented in the", shinyLink(to = "Model", label = "Model Results Page"),".
                                             Lastly, the", shinyLink(to = "Pred", label = "Model Prediction Page"), "is used to
                                              classify a customer's response to a campaign based on the inputted attributes of
                                             the customer.")
                                               
                                               
                                               
                                               
                                )),  
                         
                         column(3, HTML("<br><br><br><br><br><br>"),  tags$img(src = "lion.png",height="400px",width="400px", 
                                                                               style="display: block; margin-left: auto; margin-right:
                                  auto;")),
                     )),
            
############################### Data page ###########################################                 
            
            
            tabPanel(title="Data",value="Table",dataTableOutput("marketing_data")),
            
            
            
            
############################### Data Characteristics page ######################################                 
            
            
   tabPanel(title="Data Characteristics", value="Characteristics",HTML("<br>"),
                     p(style="text-align: justify;font-size:110%",align="left",
                       "This section presents some basic information of the sample and visualisations 
                       describing the characteristics of the customers that were included in the sample."),HTML("<br>"),
                     
                     tabsetPanel( 
                         
                         tabPanel(
                             title = "Description of Sample",
                             value = "sample", 
                             column(8, 
                                    verticalLayout(HTML("<br>"),h2("About the Sample"),HTML("<br>"),
                                        p(style="text-align: justify;font-size:110%",align="left",
                                                     "The sample consisted of 2008 customers from 7 countries: 
                                                     Australia, Canada, Germany, Spain, India, Saudi Arabia and the USA. 
                                                     The geomap below displays how the sample was distributed across the countries, 
                                                     which is indicated by the size of the markers. The majority of the 
                                                     customers resided in Spain (49.6%).",HTML("<br><br>"),
                                                     "The table on the right presents the success rates 
                                                     for each campaign. Campaign 6 had the highest rate of success
                                                     at 14.61%. The geomap below also displays the success rate of
                                                     campaign 6 according to the customer's country of residence. Saudi Arabia 
                                                     had the highest rate of success for campaign 6 at 16.7%. Additional information
                                                    pertaining to the average income, average spend and average number of purchase for 
                                                    each country can be viewed by hovering the cursor over the country in the map."))),
                         column(4, dataTableOutput("Success_Rates")),
                             
                             column(11,plotlyOutput("geomap"))),
                            
                         
                        tabPanel(
                             title = "Customer Spending Behaviour",
                             value = "barcharts",
                             column(12, 
                                    verticalLayout(HTML("<br>"),h2("Total Spend by Customers"),HTML("<br>"),
                                                   p(style="text-align: justify;font-size:110%",align="left",
                                                     "The total spend on purchases made by the customers over the last two years was collected. 
                                                     The following visual forms part of a series of plots. A bar  
                                                     can be selected which brings up an additional visual underneath.  
                                                     ",HTML("<br><br>")))),
                             column(12,plotlyOutput("category"),HTML("<br>")),
                             column(12,plotlyOutput("sub_category"),HTML("<br>")),
                            column(12,plotlyOutput("sub_category_2"))), 
                             
                             
                         
                         tabPanel(
                             title = "Relationships Among Customer Attributes",
                             value = "correlations",
                             column(12, 
                                    verticalLayout(HTML("<br>"),h2("Relationships Among Quantitative Attributes"),HTML("<br>"),
                                                   p(style="text-align: justify;font-size:110%",align="left",
                                                     "The following heatmap presents the correlations among the quatitative 
                                                     attributes of a customer. When a region in this heatmap is selected, a scatter 
                                                     plot between the two corresponding attributes is prodcued below.
                                                     ",HTML("<br><br>")))),
                             column(12,plotlyOutput("heat"), HTML("<br><br><br><br><br><br><br><br>")),
                             column(12,plotlyOutput("scatterplot"),HTML("<br><br>"))))),
            
            
############################### Visualizations page ####################################                 
            
            
            # tabPanel("Visualisations",value="Vis1",
            #          
            #          sidebarLayout(
            #              sidebarPanel(width =3,h2("Boxplot Controls"),h1(" "),
            #                           #selectInput("choice_y", "Choose a y-axis attribute", choices =list("Age","Income","Recency","MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds","NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","Total_Camp" ), selected = NULL),
            #                           selectInput("choice_y", "Choose a y-axis attribute", choices =list("Age","Income","Recency","Amount spent on wine","Amount spent on fruit","Amount spent on meat","Amount spent on fish","Amount spent on sweets","Number of purchases made with discounts","Number of purchases made through website","Number of purchases made using a catalogue","Number of purchases made directly in stores","Number of previous campaigns taken up the offer for"), selected = NULL),
            #                           selectInput("choice_x", "Choose a x-axis attribute", 
            #                                       choices =list("Education Level",
            #                                                     "Marital Status",
            #                                                     "Customer made a complaint in the last 2 years",
            #                                                     "Country"), selected = NULL),
            #                           #actionButton("submit_boxplot", "Submit"),
            #                           HTML("<br>"),h2("Barchart Controls"),
            #                           HTML("<br>"),
            #                           sliderInput("age",label="Select age range",value =c(min(cleaned_1$Age),
            #                                                                               max(cleaned_1$Age)),
            #                                       min =min(cleaned_1$Age),max(cleaned_1$Age)),
            #                           selectInput("choice_bar", "Choose an attribute", 
            #                                       choices = c("Education Level",
            #                                                   "Marital Status",
            #                                                   "Response to Campaign 1",
            #                                                   "Response to Campaign 2",
            #                                                   "Response to Campaign 3",
            #                                                   "Response to Campaign 4" ,
            #                                                   "Response to Campaign 5"))),
            #                           #submitButton(text ="Apply changes" )),
            #                           #actionButton("submit_barchart", "Submit")),
            #              
            #              mainPanel(verticalLayout(HTML("<br>"),(plotlyOutput("boxplot")),HTML("<br><br><br>"),
            #                                       plotlyOutput("barchart")),HTML("<br><br>"))
            #          )),



           


tabPanel("Visualisations",value="Vis1",
         
         fluidRow(
           column(12, verticalLayout(h3("Patterns Between Attributes and Response to Campaign 6"),p("The following plots consider the 
                                                              patterns between the customer's attributes and their response to campaign 6. 
                                                              The attributes presented in each of the plots can be selected in the controls on the left hand side."))),
           column(3, 
                  verticalLayout(HTML("<br><br><br>"),h4(strong("Boxplot Controls:")),HTML("<br>"),
                                 selectInput("choice_y", "Choose a y-axis attribute", choices =list("Age","Income","Recency","Amount spent on wine","Amount spent on fruit","Amount spent on meat","Amount spent on fish","Amount spent on sweets","Number of purchases made with discounts","Number of purchases made through website","Number of purchases made using a catalogue","Number of purchases made directly in stores","Number of previous campaigns taken up the offer for"), selected = NULL),
                                 selectInput("choice_x", "Choose a x-axis attribute", 
                                             choices =list("Education Level",
                                                           "Marital Status",
                                                           "Customer made a complaint in the last 2 years",
                                                           "Country"), selected = NULL))),
           column(9, 
                  verticalLayout(plotlyOutput("boxplot"),HTML("<br><br><br>"))),
            
            column(3, 
                         verticalLayout(HTML("<br><br><br>"),h4(strong("Barchart Controls:")),HTML("<br>"),
                                        sliderInput("age",label="Select age range",value =c(min(cleaned_1$Age),
                                                                                            max(cleaned_1$Age)),
                                                    min =min(cleaned_1$Age),max(cleaned_1$Age)),
                                        selectInput("choice_bar", "Choose an attribute", 
                                                    choices = c("Education Level",
                                                                "Marital Status",
                                                                "Response to Campaign 1",
                                                                "Response to Campaign 2",
                                                                "Response to Campaign 3",
                                                                "Response to Campaign 4" ,
                                                                "Response to Campaign 5")))),
            column(9, 
                   verticalLayout(plotlyOutput("barchart"),HTML("<br><br><br>"))))),
            
         
         
         
         
         # sidebarLayout(
         #     sidebarPanel(width =3,h2("Boxplot Controls"),h1(" "),
         #                  #selectInput("choice_y", "Choose a y-axis attribute", choices =list("Age","Income","Recency","MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds","NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","Total_Camp" ), selected = NULL),
         #                  selectInput("choice_y", "Choose a y-axis attribute", choices =list("Age","Income","Recency","Amount spent on wine","Amount spent on fruit","Amount spent on meat","Amount spent on fish","Amount spent on sweets","Number of purchases made with discounts","Number of purchases made through website","Number of purchases made using a catalogue","Number of purchases made directly in stores","Number of previous campaigns taken up the offer for"), selected = NULL),
         #                  selectInput("choice_x", "Choose a x-axis attribute", 
         #                              choices =list("Education Level",
         #                                            "Marital Status",
         #                                            "Customer made a complaint in the last 2 years",
         #                                            "Country"), selected = NULL),
         #                  #actionButton("submit_boxplot", "Submit"),
         #                  HTML("<br>"),h2("Barchart Controls"),
         #                  HTML("<br>"),
         #                  sliderInput("age",label="Select age range",value =c(min(cleaned_1$Age),
         #                                                                      max(cleaned_1$Age)),
         #                              min =min(cleaned_1$Age),max(cleaned_1$Age)),
         #                  selectInput("choice_bar", "Choose an attribute", 
         #                              choices = c("Education Level",
         #                                          "Marital Status",
         #                                          "Response to Campaign 1",
         #                                          "Response to Campaign 2",
         #                                          "Response to Campaign 3",
         #                                          "Response to Campaign 4" ,
         #                                          "Response to Campaign 5"))),
         #     #submitButton(text ="Apply changes" )),
         #     #actionButton("submit_barchart", "Submit")),
         #     
         #     mainPanel(verticalLayout(HTML("<br>"),(plotlyOutput("boxplot")),HTML("<br><br><br>"),
         #                              plotlyOutput("barchart")),HTML("<br><br>"))
         # )),


            
############################### Model page #################################################                 
            
            # tabPanel("Model Results", value="Model",
            #          sidebarLayout(
            #              sidebarPanel(),
            #              mainPanel(verticalLayout(dataTableOutput("LR_results")))
            #          )),

tabPanel("Model Results", value="Model",h2("Fitting the Model"),HTML("<br>"),
         p(style="text-align: justify;font-size:110%",align="left",
           "As primary interest was in determining which factors were significantly associated 
           with a customer's response to campaign 6, which is a binary outcome, a logistic regression model 
           was applied to the data. Only the explanatory variables that had an
           association with the response at a relaxed p-value of 20% were entered into the
           final logistic regression model. This model was then used to classify a customer
           as either accepting the offer to campaign 6 or not. To train and evaluate the model,
           prior to fitting the final logistic regression model, the data was split into a 
           training set and a testing set. The training data set was used to train the model and 
           obtain the model's parameter estimates, while the test data set was used to evaluate 
           the accuracy of this model."),HTML("<br>"),
         
         tabsetPanel( 
             
             tabPanel(
                 title = "Logistic Regression Results",
                 value = "Results", 
                 column(12, 
                        verticalLayout(HTML("<br>"),#h2("About the Sample"),HTML("<br>"),
                                       p(style="text-align: justify;font-size:110%",align="left",
                                "The results of the logistic regression model fitted 
                                to the training data set are presented in Table 2 below. The factors that were significantly associated with the customer's response
                                to campaign 6 were marital status, the number of teenagers in customer's household,
                                the number of purchases made directly in stores, recency (number of days since the 
                                customer's last purchase), total spend on products over the past two years, the
                                number of purchases made with discounts, the number of visits to the company's website 
                                in the last month, the number of previous campaigns taken up the offer for, education level and
                                the number of purchases made through the company's website."))),
                 column(12, verticalLayout(dataTableOutput("LR_results"),HTML("<br>")))),

          tabPanel(
                 title = "Model Evaluation",
                 value = "Evaluation", 
                 column(12, 
                        verticalLayout(HTML("<br>"),h3("Determining the Cutoff Value"),HTML("<br>"),
                               p(style="text-align: justify;font-size:110%",align="left",
                                "The fitted logistic regression model is used to obtain the predicted probability
                                 that a customer will accept the offer to the campaign. Using this predicted
                                probability, we can then classify a customer as either accepting the 
                                offer to the campaign, or not accepting the offer, based on a cutoff value or threshold.
                                If the predicted proability is higher than the cutoff value, then the customer is 
                                classified as positive (they will accept the offer), or negative (they will not
                                accept the offer) if the predicted probability is below the cutoff.",HTML("<br><br>"),
                                
                                "The optimal cutoff is determined based on the value that balances the false positive rate
                                and the false negative rate. This is done by minimizing a cost function, which is related to the rate
                                of both errors. For this cost function, more emphasis can be placed on either error. In our context, 
                                we place more emphasis on making a false negative which can be regarded as a more serious error 
                                as it would result in missing out on targeting customers who would have accepted the 
                                offer to the campaign. However, a false positive would simply result in a few wasted resources 
                                from incorrectly targeting customers that would not actually accept the offer. The weighting associated
                                with the two errors can be changed below.",HTML("<br>"),
                               
                                
                                
                                 HTML("<br>"),h4(strong("Specify the false positive and false negative rates:")),
                                "Insert a numerical value between 1 and 10 for the cost of making each type of error below.",HTML("<br><br>"),
                                numericInput(inputId = "cost_fp", label = "False Positive Rate",min = 1,max = 10,step = 1,value=1,width='20%'),
                                numericInput(inputId = "cost_fn", label = "False Negative Rate",min = 1,max = 10,step = 1,value=2,width='20%'),
                                
                                p("Below, we make use of a ROC curve to visualize and quantify the tradeoff between the two measures in order to find the optimal cutoff value:",HTML("<br>"))
                                
                                ))),
                                
                 column(12, verticalLayout(plotOutput("roc"),HTML("<br><br>"),
                                           textOutput("cutoff_value"))),
                 
                 column(12, 
                        verticalLayout(HTML("<br>"),h3("Evaluating the Model"),HTML("<br>"),
                                p(style="text-align: justify;font-size:110%",align="left",
                                 "To assess the performance of the model on both the training and test data sets, we 
                                 consider a confusion matrix and several model performance measures. 
                                 The figures below are an illustration of the confusion matrices for the training and test data."))),
                 
                 column(12, verticalLayout(HTML("<br>"),plotOutput("confusion_train"),plotOutput("confusion_test"))),
                 
                 column(6, verticalLayout(dataTableOutput("measures"),HTML("<br>")))
                 
                 
                 ) 
             
)),




    # tabPanel("Model Results", value="Model",
    #          fluidRow(
    #              column(12, verticalLayout(HTML("<br>"),h2("Logistic Regression Results"),HTML("<br>"),
    #                                        p(style="text-align: justify;font-size:110%",align="left",
    #                             "As primary interest was in determining which factors were significantly associated 
    #                             with a customer's response to campaign 6, a logistc regression model 
    #                             was applied to the data. Only the explanatory variables that had a bivariate
    #                             association with the response at a relaxed p-value of 20% were entered into the
    #                             final logistic regression model. This model was then used to classify a customer
    #                             as either accepting the offer to campaign 6 or not. To train and evaluate the model,
    #                             prior to fitting the final logistic regression model, the data was split into a 
    #                             training set and a testing set. The training data set was used to train the model and 
    #                             obtain the model's parameter estimates, while the test data set was be used to evaluate 
    #                             the accuracy of this model. .",HTML("<br><br>"),
    #                             
    #                             "The results of the logistic regression model fitted 
    #                             to the training data set are presented in Table 2 below. The factors that were significantly associated with the customer's response
    #                             to campaign 6 were marital status, the number of teenagers in customer's household,
    #                             the number of purchases made directly in stores, recency (number of days since the 
    #                             customer's last purchase), total spend on products over the past two years, the
    #                             number of purchases made with discounts, the number of visits to the company's website 
    #                             in the last month, the number of previous campaigns taken up the offer for, education level and
    #                             the number of purchases made through website." ))),
    #                     column(12, verticalLayout(dataTableOutput("LR_results"),HTML("<br>"))))),

            
            
############################### Model Prediction page ####################################                 
            
#            
 tabPanel("Model Prediction", value="Pred",
#                   fluidRow(
#                       column(12, verticalLayout(HTML("<br>"),#h2("Logistic Regression Results"),HTML("<br>"),
#                                                 p(style="text-align: justify;font-size:110%",align="left",
#                                      "Explain how to use table",HTML("<br><br>")
#                                       ))),
#                              column(8, verticalLayout(dataTableOutput("prediction"),HTML("<br>"))))
#          
#          
#          )

sidebarLayout(
  sidebarPanel(style = "height: 75vh; overflow-y: auto;",width =4,h3("Enter the customer's attributes below"),
               selectInput("education", "Choose an education level", choices =levels(cleaned_1$`Education Level`), selected = NULL),
               selectInput("marital", "Choose a marital status", 
                           choices =levels(cleaned_1$`Marital Status`), selected = NULL),
               selectInput("country", "Choose a country of residence", 
                           choices =levels(cleaned_1$`Country`), selected = NULL),
               selectInput("complaint", "Customer made a complaint in the last 2 years?", 
                           choices =levels(cleaned_1$`Customer made a complaint in the last 2 years`), selected = NULL),
               numericInput("teenagers", "Number of teenagers in household",min = 1,max = 100,step = 1,value=0),
               numericInput("recency", "Number of number of days since the 
                                customer's last purchase",min = 1,max = 100,step = 1,value=0),
               numericInput("spend", "Total spend over the last two years",min = 1,max = 100000,step = 1,value=0),
               numericInput("discounts", "Number of purchases made with discounts",min = 1,max = 100,step = 1,value=0),
               numericInput("website", "Number of purchases made through website",min = 1,max = 100,step = 1,value=0),
               numericInput("catalogue", "Number of purchases made using a catalogue",min = 1,max = 100,step = 1,value=0),
               numericInput("stores", "Number of purchases made directly in stores",min = 1,max = 100,step = 1,value=0),
               numericInput("visits", "Number of visits to company's website in the last month",min = 1,max = 100,step = 1,value=0),
               numericInput("previous", "Number of previous campaigns taken up the offer for",min = 1,max = 100,step = 1,value=0),
               actionButton("submit_prediction", "Submit")),
  
               
  mainPanel(tags$label(h2('Predicted Outcome')), # Status/Output Text Box
            verbatimTextOutput('contents'),
            tableOutput('tabledata')
            #dataTableOutput("prediction")# Prediction results table
  )))

)),
    
########################### Creating the Java Script file with links ######################           
    
    
    tags$script(src = "shinyLink.js")
    
    
)




######################################## Axis titles & Font ####################################           



axis_titles <- . %>%
    layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Spend ($)"))

t <- list(
    family = "Arial",
    size = 12
    #, color = 'blue'
)




###################################### Server ##########################################                 


server<- function(input,output){
    
  #bs_themer()
    w = Waiter$new() # 2. Initialize
    w$show() # 3. Program
    Sys.sleep(2)
    w$hide() 
    

############################################## Geopmap ####################################                 
    
    
    output$geomap<-renderPlotly({
    g <- list(type="natural earth",showcountries=TRUE,scope="world",showframe=FALSE,fitbounds="geojson",countrywidth=0.5,countrycolor='black',framecolor='white',showcoastlines=TRUE,coastlinecolor="black",bgcolor='slategray',landcolor='red',showocean=TRUE,oceancolor='white')
        
    map <- plot_geo(data_agg,sizes=c(1,1000),locationmode="country names",
                    marker=list(sizeref=1)) %>% 
        add_markers(x=~Long,y=~Lat,size=~`% of Sample Size`,color=~`Success rate of Campaign 6`,
                    colors = 'Reds',hoverinfo="text",
        text=~paste("Country:",data_agg$Country,"<br />",round(data_agg$`% of Sample Size`,2),
                    "% of overall sample size","<br />",round(data_agg$`Success rate of Campaign 6`,2)
                    ,"% success rate of Campaign 6","<br />",
                    "Average Income: $",round(data_agg$`Average Income`,2),"<br />",
                    "Average Total Spend: $",round(data_agg$`Average Spend`,2),"<br />",
                    "Average Number of Purchases:",round(data_agg$`Average Number of Purchases`,2)))%>%
        layout(geo=g) %>% 
        config(modeBarButtons = list(list("toImage","resetScale2d","zoomIn2d","zoomOut2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "Geomap.png")) 
    map
       })
    
################################### Country/Education/Marital Status Drill Down plots ####################################                 
    
    
    Country <- reactiveVal()
    Education <- reactiveVal()
    Marital_Status<-reactiveVal()
    
    # when clicking on a category, 
    observeEvent(event_data("plotly_click", source = "Country"), {
        Country(event_data("plotly_click", source = "Country")$x)
        Education(NULL)
    })
    
    observeEvent(event_data("plotly_click", source = "Education"), {
        Education(
            event_data("plotly_click", source = "Education")$x
        )
    })
    
    observeEvent(event_data("plotly_click", source = "Marital Status"), {
        Education(
            event_data("plotly_click", source = "Marital Status")$x
        )
    })
    
    output$category <- renderPlotly({
        cleaned_1 %>%
            group_by(Country) %>%summarize(mean_total_spent=round(mean(`Total Spent`),2)) %>% 
            plot_ly(x = ~Country, y = ~mean_total_spent,color =~Country, source = "Country",hovertemplate = "%{fullData.name} average spend: $%{y} <extra></extra>") %>%
            axis_titles() %>% config(modeBarButtons = list(list("toImage","resetScale2d","zoomIn2d","zoomOut2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "Country_Average_Spend.png")) %>% 
            layout(title = "Average total spend over the past 2 years according to country",legend = list(title = list(text = "Country"),x = 100, y = 0.5))  
            #onRender("function(el, x) {Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')}") #to change the cursor 
        
    })
    
    output$sub_category <- renderPlotly({
        if (is.null(Country())) return(NULL)
        
        cleaned_1 %>%
            filter(Country %in% Country()) %>%
            group_by(`Education Level`) %>% summarize(mean_spent_educ=round(mean(`Total Spent`),2)) %>%
            plot_ly(x = ~`Education Level`, y = ~mean_spent_educ,color = ~`Education Level`, source = "Education",hovertemplate = "%{fullData.name} education level average spend: $%{y} <extra></extra>") %>%
            axis_titles() %>% config(modeBarButtons = list(list("toImage","resetScale2d","zoomIn2d","zoomOut2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "Average_Spend_by_Education.png")) %>% 
            layout(title = paste("Average total spend for customers in",Country(), "according to education level" ),legend = list(title = list(text = "Education Level"),x = 100, y = 0.5))
    })
    
    output$sub_category_2<- renderPlotly({
        if (is.null(Education())) return(NULL)
        
        cleaned_1 %>%
            filter(`Education Level`%in% Education()) %>%
            group_by(`Marital Status`) %>% summarize(mean_spent_Marital=round(mean(`Total Spent`),2)) %>% setNames(c("labels","values"))  %>% 
            plot_ly() %>% add_pie(labels = ~labels,values = ~values,customdata=~labels)  %>% 
            axis_titles() %>% config(modeBarButtons = list(list("toImage","resetScale2d","zoomIn2d","zoomOut2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "Distribution_of_Average_Spend.png")) %>% 
            layout(title =paste("Distribution of average total spend for customers in",Country(), "with a", Education(), "education level according to marital status"),legend = list(title = list(text = "Marital Status"),x = 100, y = 0.5))
    })
    
    # hoverinfo = 'text',text = ~paste('Average spend: $', values, percent)
    # config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"),displaylogo = FALSE)
    # config(displayModeBar = FALSE) %>% # this disables modebar (the small menu on the top right hand side that shows up)
    # config(modeBarButtons = list(list("toImage","resetScale2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "plotOutput.png"))
    
    
    ####################################### Heatmap and scatter drill down plots ##############################################  
    
    
    output$heat <- renderPlotly({
        plot_ly(source = "heat_plot") %>%
            add_heatmap(
                x = names(dataset_numeric), 
                y = names(dataset_numeric), 
                z = correlation,coloraxis = 'coloraxis')%>% config(modeBarButtons = list(list("toImage","resetScale2d","zoomIn2d","zoomOut2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "Heatmap.png")) %>% 
             layout(coloraxis=list(colorscale='Jet'),width=1100,
                         height=550)
    })
    
    output$scatterplot <- renderPlotly({
        # if there is no click data, render nothing!
        clickData <- event_data("plotly_click", source = "heat_plot")
        if (is.null(clickData)) return(NULL)
        
        # Obtain the clicked x/y variables and fit linear model
        vars <- c(clickData[["x"]], clickData[["y"]])
        d <- setNames(dataset_numeric[vars], c("x", "y"))
        yhat <- fitted(lm(y ~ x, data = d))
        
        # scatterplot with fitted line
        plot_ly(d, x = ~x,marker = list(size = 10,
                                        color = 'rgba(255, 182, 193, .9)',
                                        line = list(color = 'rgba(152, 0, 0, .8)',
                                                    width = 2))) %>% config(modeBarButtons = list(list("toImage","resetScale2d","zoomIn2d","zoomOut2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "Scatter_plot.png")) %>%
            add_markers(y = ~y) %>% 
            add_lines(y = ~yhat,colors=pal) %>%
            layout(
                xaxis = list(title = clickData[["x"]]),
                yaxis = list(title = clickData[["y"]]),
                title=paste(list(title = clickData[["x"]])," vs ",list(title = clickData[["y"]])),
                showlegend = FALSE
            ) 
    })
    
    
    
###################################### Boxplots ####################################                 
    
    
    output$boxplot<-renderPlotly({
        #req(input$submit_boxplot) # This is to allow the action/submit button
        x<-list(
            title=paste(input$choice_x)
        )
        
        y<-list(
            title=paste(input$choice_y)
        )
        
        fig<-cleaned_1 %>% plot_ly(x=~get(input$choice_x),y=~get(input$choice_y),color = ~`Response to Campaign 6`,type="box",colors = "Set1",mode = "markers",boxmean = T) %>% 
            config(modeBarButtons = list(list("toImage","resetScale2d","zoomIn2d","zoomOut2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "Boxplots.png")) %>% 
            layout(title = paste("Boxplots of",input$choice_y, "according to",HTML("<br>"), input$choice_x, "and Response to Campaign 6",HTML("<br><br><br>")),xaxis=x,yaxis=y,boxmode="group",legend = list(title = list(text = "Acceptance of Campaign 6"),x = 100, y = 0.5))
        fig
    })
    
    
###################################### Bar chart ####################################                 
    
    
    output$barchart<-renderPlotly({  
        #req(input$submit_barchart) # This is to allow the action/submit button
        Data <- cleaned_1 %>%
            filter(between(Age, input$age[1],input$age[2]))
        x_bar<-list(title=paste(input$choice_bar))
        
        table <- round(prop.table(table(Data[[x_bar[[1]]]],Data$`Response to Campaign 6`),margin=1)*100,2)
        
        table=data.frame(table)
        barchart <- table %>%
            filter(Var2=="Yes") %>%
            plot_ly(x = ~Var1,
                    y = ~Freq,
                    type = "bar",
                    color = ~Var1) %>%
            config(modeBarButtons = list(list("toImage","resetScale2d","zoomIn2d","zoomOut2d")), displaylogo = FALSE, toImageButtonOptions = list(filename = "Barcharts.png")) %>% 
            layout(title = paste("Success rate of Campaign 6 according to",input$choice_bar,HTML("<br>")),xaxis = x_bar,yaxis = list(title ="Percentage (%)"),legend = list(x = 100, y = 0.5))
        barchart
        
    })
    
###################################### Model Evaluation Plots ##############################                 
    output$roc<-renderPlot({
      
      cm_info<- ConfusionMatrixInfo_train( data = trainSet, predict = "prediction", 
                                            actual = "Response to Campaign 6", cutoff = 0.6 )
        roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                             actual = "actual", cost.fp = input$cost_fp, cost.fn = input$cost_fn)
        
        grid.draw(roc_info$plot)}) 
  
    output$confusion_train<-renderPlot({
      
      cm_info<- ConfusionMatrixInfo_train( data = trainSet, predict = "prediction", 
                                           actual = "Response to Campaign 6", cutoff = 0.6 )
        
        roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                             actual = "actual", cost.fp = input$cost_fp, cost.fn = input$cost_fn)
      
        
        cm_info_train <- ConfusionMatrixInfo_train( data = trainSet, predict = "prediction", 
                                         actual = "Response to Campaign 6", cutoff = roc_info$cutoff)
        
        
        cm_info_train$plot
    
        }) 
    
    output$confusion_test<-renderPlot({
      
      cm_info<- ConfusionMatrixInfo_train( data = trainSet, predict = "prediction", 
                                           actual = "Response to Campaign 6", cutoff = 0.6 )
      
      roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                           actual = "actual", cost.fp = input$cost_fp, cost.fn = input$cost_fn)
      
      
     
      cm_info_test <- ConfusionMatrixInfo_test( data = testSet, predict = "prediction", 
                                                actual = "Response to Campaign 6", cutoff = roc_info$cutoff)
      
      
      cm_info_test$plot
    })
    
    # value <- reactiveVal({
    #     roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
    #                          actual = "actual", cost.fp = input$cost_fp, cost.fn = input$cost_fn)
    #     print(round(roc_info$cutoff,2))
    #             }) 
    
    output$cutoff_value <- renderText({
      
      cm_info<- ConfusionMatrixInfo_train( data = trainSet, predict = "prediction", 
                                           actual = "Response to Campaign 6", cutoff = 0.6 )
        roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                             actual = "actual", cost.fp = input$cost_fp, cost.fn = input$cost_fn)
        cutoff=round(roc_info$cutoff,2)
        paste("Based on the specified false positive and false negative rates, the opitmal cutoff point is given by:",cutoff)
    })
    
    
    # output$predicted_prob <- renderText({
    #   
    #   
    #   Estimate <- results[1,1]+ sum(Table$Observation*Estimates$Estimate)
    #   
    #   Predicted_probability <- exp(Estimate)/(1+exp(Estimate))
    #   
    #   
    #   paste("The predicted probability of this customer accepting the offer is",Predicted_probability)
    # })
    
    
    
###################################### Tables ####################################       
    
    output$marketing_data<-renderDataTable({cleaned_1}, 
                                           options=list(columnDefs = list(list(className = 'dt-center', targets = "_all")) ))
    
    output$Success_Rates<-renderDataTable({Success_Rates}, filter = "none",caption = 'Table 1: Success Rate of Campaigns',width = NULL,rownames = FALSE,
                                          options=list(#iDisplayLength=8,                    # initial number of records
                                                       aLengthMenu=-1,                  # records/page options
                                                       bLengthChange=0,                       # show/hide records per page dropdown
                                                       bFilter=0,                                    # global search box on/off
                                                       bInfo=0,                                      # information on/off (how many records filtered, etc)
                                                       bAutoWidth=0,                            # automatic column width calculation, disable if passing column width via aoColumnDefs
                                                       aoColumnDefs = list(list(sWidth="100px", aTargets="_all")),    # custom column size
                                                       dom = 't', # this removes next/previous page buttons
                                                       columnDefs = list(list(className = 'dt-center', width = '50%', targets = "_all")),    # to centre columns
                                                       scrollX=TRUE,scrollY=400,
                                                       scrollCollapse=TRUE 
                                          ))
    
    output$LR_results<-DT::renderDataTable({results},colnames = c('Variable' = 1),caption = 'Table 2: Logistc Regression Results (* indicates significance at a 5% level)',width = NULL,rownames = TRUE,
                                          options=list(iDisplayLength=5,                    # initial number of records
                                              #aLengthMenu=-1,                  # records/page options
                                              bLengthChange=0,                       # show/hide records per page dropdown
                                              bFilter=1,                                    # global search box on/off
                                              bInfo=0,                                      # information on/off (how many records filtered, etc)
                                              bAutoWidth=0,                            # automatic column width calculation, disable if passing column width via aoColumnDefs
                                              pageLength = 100000,  # change this for all rows to be shown in the scroll
                                              #aoColumnDefs = list(list(sWidth="100px", aTargets="_all")),    # custom column size
                                              dom = 't', # this removes next/previous page buttons
                                              columnDefs = list(list(className = 'dt-center', targets = 1:4)),    # to centre columns
                                              scrollX=TRUE,scrollY=400,
                                              scrollCollapse=TRUE
                                              #drawCallback = JS(js)
                                          ))
    
    
    output$measures<-DT::renderDataTable({
    
      cm_info<- ConfusionMatrixInfo_train( data = trainSet, predict = "prediction", 
                                           actual = "Response to Campaign 6", cutoff = 0.6 )
      roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                           actual = "actual", cost.fp = input$cost_fp, cost.fn = input$cost_fn)
      
      probabs_train <- predict(logistic_trained, trainSet, type='response') #predictions on the test set made by the model
      probabs_test <- predict(logistic_trained, testSet, type='response') #predictions on the test set made by the model
      
      preds_test <- ifelse(probabs_test > roc_info$cutoff, "Yes","No") # classifying prob made on test set as 1 or 0 using 0.5 cuttoff point
      preds_train <- ifelse(probabs_train > roc_info$cutoff, "Yes","No") # classifying prob made on test set as 1 or 0 using 0.5 cuttoff point
    
    
     performance_train=confusionMatrix(factor(preds_train), factor(trainSet$`Response to Campaign 6`),positive = "Yes")
     performance_test=confusionMatrix(factor(preds_test), factor(testSet$`Response to Campaign 6`),positive = "Yes")
    
    
    Measures <- data.frame(Measure=c('Accuracy', 'Sensitivity', 'Specificity', 'Precision', 'F1'),
                           Training =c(round(performance_train[["overall"]][["Accuracy"]],3)*100,
                                       round(performance_train[["byClass"]][["Sensitivity"]],3)*100,
                                       round(performance_train[["byClass"]][["Specificity"]],3)*100,
                                       round(performance_train[["byClass"]][["Precision"]],3)*100,
                                       round(performance_train[["byClass"]][["F1"]],3)*100), 
                           Testing=c(round(performance_test[["overall"]][["Accuracy"]],3)*100,
                                     round(performance_test[["byClass"]][["Sensitivity"]],3)*100,
                                     round(performance_test[["byClass"]][["Specificity"]],3)*100,
                                     round(performance_test[["byClass"]][["Precision"]],3)*100,
                                     round(performance_test[["byClass"]][["F1"]],3)*100))
    
    Measures},filter = "none",caption = 'Table 3: Model performance measures (%)',width = NULL,rownames = FALSE,
    options=list(#iDisplayLength=8,                    # initial number of records
      aLengthMenu=-1,                  # records/page options
      bLengthChange=0,                       # show/hide records per page dropdown
      bFilter=0,                                    # global search box on/off
      bInfo=0,                                      # information on/off (how many records filtered, etc)
      bAutoWidth=0,                            # automatic column width calculation, disable if passing column width via aoColumnDefs
      aoColumnDefs = list(list(sWidth="100px", aTargets="_all")),    # custom column size
      dom = 't', # this removes next/previous page buttons
      columnDefs = list(list(className = 'dt-center', width = '20%', targets = 1:2)),    # to centre columns
      scrollX=TRUE,scrollY=300,
      scrollCollapse=TRUE)
   
    
    )
    
    
    output$prediction<-renderDataTable({Table},editable = list(target = "column", disable = list(columns = c(0))),filter = "none",width = NULL,rownames = FALSE,
                                       options=list(#iDisplayLength=8,                    # initial number of records
                                         aLengthMenu=-1,                  # records/page options
                                         bLengthChange=0,                       # show/hide records per page dropdown
                                         bFilter=0,                                    # global search box on/off
                                         bInfo=0,                                      # information on/off (how many records filtered, etc)
                                         bAutoWidth=0,                            # automatic column width calculation, disable if passing column width via aoColumnDefs
                                         pageLength = 26,
                                         aoColumnDefs = list(list(sWidth="100px", aTargets="_all")),    # custom column size
                                         dom = 't', # this removes next/previous page buttons
                                         columnDefs = list(list(className = 'dt-center', width = '10%', targets = 1),list(width = '20%', targets = 0)),    # to centre columns
                                         scrollX=TRUE,scrollY=400,
                                         scrollCollapse=TRUE ))
    
############################################## Model Prediction Setup ##########################################    
    
    # Input Data
    datasetInput <- reactive({
      
      cm_info<- ConfusionMatrixInfo_train( data = trainSet, predict = "prediction", 
                                           actual = "Response to Campaign 6", cutoff = 0.6 )
      roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                           actual = "actual", cost.fp = input$cost_fp, cost.fn = input$cost_fn)
     
      
      df <- data.frame(matrix(ncol = 13, nrow = 0))
      
      row_values=c(input$education,
                   input$marital,
                   input$country,
                   input$teenagers,
                   input$recency,
                   input$complaint,
                   input$spend,
                   input$discounts,
                   input$website,
                   input$catalogue,
                   input$stores,
                   input$visits,
                   input$previous)
      
      df=rbind(df,row_values)
      
      colnames(df)=colnames(predict_data)
      
      df$`Education Level`=factor(df$`Education Level`)
      df$`Marital Status`=factor(df$`Marital Status`)
      df$`Country`=factor(df$`Country`)
      df$`Customer made a complaint in the last 2 years`=factor(df$`Customer made a complaint in the last 2 years`)
      df$`Number of teenagers in customer's household`=as.numeric(df$`Number of teenagers in customer's household`)
      df$`Recency`=as.numeric(df$`Recency`)
      df$`Total Spent`=as.numeric(df$`Total Spent`)
      df$`Number of purchases made with discounts`=as.numeric(df$`Number of purchases made with discounts`)
      df$`Number of purchases made through website`=as.numeric(df$`Number of purchases made through website`)
      df$`Number of purchases made using a catalogue`=as.numeric(df$`Number of purchases made using a catalogue`)
      df$`Number of purchases made directly in stores`=as.numeric(df$`Number of purchases made directly in stores`)
      df$`Number of visits to company's website in the last month`=as.numeric(df$`Number of visits to company's website in the last month`)
      df$`Number of previous campaigns taken up the offer for`=as.numeric(df$`Number of previous campaigns taken up the offer for`)
      
      
      probability <- round(predict(logistic_trained, df, type='response'),2)
      
      prediction <- ifelse(probability > roc_info$cutoff, "Yes, the customer will take up the offer of the campaign","No, the customer will not take up the offer of the campaign")
      
      row_output=c(probability,prediction)
      
      Output <- data.frame(matrix(ncol = 2, nrow = 0))
      
      Output=rbind(Output,row_output)
      colnames(Output)=c("Predicted Probability", "Classification")
      
       print(Output)
      
     })
    
    # Status/Output Text Box
    output$contents <- renderText({
      if (input$submit_prediction>0) { 
        isolate("The prediction has been made:") 
      } else {
        return("Enter the customer's attributes to obtain the prediction below:")
      }
      })
    
    # Prediction results table
    output$tabledata <- renderTable({
      if (input$submit_prediction>0) { 
        isolate(datasetInput()) 
      } 
    })    

# ############################ Generating Report ################################
#     output$report <- downloadHandler(
#       # For PDF output, change this to "report.pdf"
#       filename = "report.pdf",
#       content = function(file) {
#         # Copy the report file to a temporary directory before processing it, in
#         # case we don't have write permissions to the current working dir (which
#         # can happen when deployed).
#         tempReport <- file.path(tempdir(), "report.pdf")
#         file.copy("report.pdf", tempReport, overwrite = TRUE)
#         
#         # Knit the document, passing in the `params` list, and eval it in a
#         # child of the global environment (this isolates the code in the document
#         # from the code in this app).
#         rmarkdown::render(tempReport, output: pdf_document,
#                           params = params,
#                           envir = new.env(parent = globalenv())
#         )
#       }
#     )
    
        
   
}

shinyApp(ui = ui, server = server)
