
library(shiny)
library(shinydashboard)
library(MASS)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(randomForest)
library(caTools)
library(ROCR)
library(e1071)
library(DT)
library(caret)
dataset<- read.csv("attrition.csv");
#View(dataset)
dataset[is.na(dataset)]=0
#View(dataset)

summary(dataset)
head(dataset)
#dataset <- sample(nrow(dataset), size=10000, replace = FALSE, prob = NULL)
#dataset.subset <- dataset[s, ]
#dataset.subset

#feature selection
dataset = dataset[4:14]
head(dataset)


dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

head(dataset)

corr<- dataset %>%
   sapply(., as.numeric) %>%
   as.data.table()
corr<- cor(corr, use = 'pairwise.complete.obs')
corr[upper.tri(corr)] <- NA
corr<- melt(corr, na.rm = T) %>% as.data.table() %>% setorder(-value)

corr$text<- ifelse(abs(corr$value) >= .8 &corr$value != 1, round(corr$value, 2), '')
forcorrplot<-data

ggplot(data = corr, aes(x = Var1, y = Var2, fill = value)) +
   geom_tile(color = 'white') +
   geom_text(aes(label = text)) +
   scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                        midpoint = 0, limit = c(-1, 1),
                        name = 'Pearson Correlation') +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(title = 'Correlation Matrix')

set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

classifier_rf_new= randomForest(x = training_set,
                                y = training_set$Exited,
                                ntree = 500)

r = predict(classifier_rf_new, newdata = test_set)


y_pred_rf_new = ifelse(r> 0.5, 1, 0)

# Making the Confusion Matrix
cm_rf_new = table(test_set$Exited, y_pred_rf_new)

#accuracy
n_rf_new = sum(cm_rf_new)
diag_rf_new = diag(cm_rf_new)
accuracy_rf_new = sum(diag_rf_new) / n_rf_new
accuracy_rf_new



pred_rf<- prediction(test_set$Exited, y_pred_rf_new)
perf_rf<- performance(pred_rf,"tpr","fpr")
plot(perf_rf,colorize=TRUE, main="AUC RF")

cm_rf=confusionMatrix(table(test_set$Exited,y_pred_rf_new))
cm_rf

classifier_glm = glm(formula = Exited ~ .,
                     family = binomial,
                     data = training_set)
summary(classifier_glm)
prob_pred = predict(classifier_glm, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred> 0.5, 1, 0)

summary(classifier_glm)

cm_glm_new = table(test_set[,9], y_pred)

d_glm=data.frame(test_set,Exited_Predicted=y_pred)
d_rf=data.frame(test_set,Exited_Predicted=y_pred_rf_new)


ui=shinyUI(dashboardPage(
   dashboardHeader(title = "PREDICTION"),
   
   #dashboardSidebar(width = 300),
   
   dashboardSidebar(
      
      sidebarMenu(
         selectInput("Model", "Choose  model:",c("Random forest"))
      )
      
   ),
   dashboardBody(
      
         tabPanel("Prediction result",
                  
      tabsetPanel(
                  column(6,box(height =400,width = 800, solidHeader = FALSE, status = "success",
                                DT::dataTableOutput("table1"))),
                  column(6,box(height =400,width = 800, solidHeader = FALSE, status = "success",
                               verbatimTextOutput("table2")))
                  
         )))))




server=shinyServer(function(input, output) {
   
   
   
   output$table1 <- DT::renderDataTable({
      if(input$Model=="Random forest"){
         d=d_rf
      }else{
         d=d_glm
      }
     d},options = list(scrollX = TRUE,scrollY=TRUE)
   )
   
   output$table2 <- renderPrint({
      if(input$Model=="Random forest"){
         cm=cm_rf_new
      }else{
         cm=cm_glm_new
      }
      cm
      
   })
   
})

shinyApp(ui = ui, server = server)
