data <- read.csv("C:/Users/Preethi/Documents/Data Mining Lab/DM_Lab_CAT/Ola/Ola_Datasets/Ola_3000.csv")
data <- data.frame(data)

head(data)
#check missing values
is.na(data)
#Text Cleaning

data_text <- data$Text
data_text <- tolower(data_text)
data_text <- gsub("rt", "", data_text)
data_text <- gsub("@\\w+", "", data_text)
data_text <- gsub("[[:punct:]]", "", data_text)
data_text <- gsub("http\\w+", "", data_text)
data_text <- gsub("[ |\t]{2,}", "", data_text)
data_text <- gsub("^ ", "", data_text)
data_text <- gsub(" $", "", data_text)

#generate wordcloud

library("wordcloud")
wordcloud(data_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

library("syuzhet")
mysentiment_data <-get_nrc_sentiment((data_text))
Sentimentscores_data<-data.frame(colSums(mysentiment_data[,]))

names(Sentimentscores_data)<-"Score"
Sentimentscores_data<-cbind("sentiment"=rownames(Sentimentscores_data),Sentimentscores_data)
rownames(Sentimentscores_data)<-NULL

library("tidyverse")
library("ggplot2")
ggplot(data=Sentimentscores_data,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on OLA")


sents.value <- get_sentiment(data_text)
most.positive <- data_text[sents.value==max(sents.value)]
most.positive

most.negative <- data_text[sents.value<=min(sents.value)]
most.negative

sents.value

positive.tweet <- data_text[sents.value>0]
positive.tweet

category_sent <- ifelse(sents.value>0,"Positive","Negative")
category_sent

df <- data.frame(Tweets=data_text,Category=category_sent)
df
str(df)
write.csv(df,"C:/Users/Preethi/Documents/Data Mining Lab/DM_Lab_CAT/Ola/Ola_Datasets/Ola_Category_3000.csv")
data <- read.csv("C:/Users/Preethi/Documents/Data Mining Lab/DM_Lab_CAT/Ola/Ola_Datasets/Ola_Category_3000.csv",stringsAsFactors = TRUE)
str(data)
library("caret")
intrain <- createDataPartition(y= data$Category,p=0.70,list = FALSE) 
training <-  data[intrain,]
testing <- data[-intrain,]
str(training)
str(testing)
dim(training)
dim(testing)

library("e1071")
svmfit <- svm(Category~.,data=training,kernel="linear",cost=1,scale = FALSE,type = "C-classification")
summary(svmfit)

#Radial SVM 

radialsvm <- svm(Category~.,data=training,kernel="radial",cost=0.5,scale = FALSE)
radialsvm

test_pred <- predict(radialsvm,testing)
test_pred

confusionMatrix(testing$Category,test_pred)


#Plotting

plot(training$X,training$Tweets,col=training$Category)

#Navie Bayers

x <- training[,-3]
y<- training$Category

library("klaR")
nb = train(x,y,'nb',trControl=trainControl(method='cv',number=5))
nb
Predict <- predict(nb,newdata = testing )
confusionMatrix(Predict,as.factor(testing$Category))

