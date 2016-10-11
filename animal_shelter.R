library(randomForest)
library(rpart)
library(lubridate)
library(dplyr)

cat("Retrieving data - ")
train_csv = read.csv("train.csv", stringsAsFactors=FALSE)
test_csv = read.csv("test.csv", stringsAsFactors=FALSE)

names(train_csv)[1] = 'ID'
test_csv$ID = as.character(test_csv$ID)

data_frame = bind_rows(train_csv, test_csv)

cat("Formating data - ")

data_frame$OutcomeType = as.factor(data_frame$OutcomeType)
data_frame$OutcomeSubtype = as.factor(data_frame$OutcomeSubtype)

# Name
data_frame$Name = as.character(data_frame$Name)
data_frame$hasName = data_frame$Name != ""
data_frame$NameMultiple = as.logical(grepl(" ", data_frame$Name))

data_frame$AnimalType = as.factor(data_frame$AnimalType)

### DateTime
data_frame$DateTimeHour = as.integer(hour(data_frame$DateTime))
data_frame$DateTimeYear = as.integer(year(data_frame$DateTime))
data_frame$DateTimeMonth = as.integer(month(data_frame$DateTime))
data_frame$DateTimeWeekDay = as.integer(wday(data_frame$DateTime))
data_frame$DateTime = as.Date(data_frame$DateTime)
data_frame$DateTimeSeason = "Winter"
data_frame$DateTimeSeason[which(data_frame$DateTimeMonth >= 4 & data_frame$DateTimeMonth < 7)] = "Spring"
data_frame$DateTimeSeason[which(data_frame$DateTimeMonth >= 7 & data_frame$DateTimeMonth < 10)] = "Summer"
data_frame$DateTimeSeason[which(data_frame$DateTimeMonth >= 10)] = "Autumn"
data_frame$DateTimeSeason = as.factor(data_frame$DateTimeSeason)

### Sex
data_frame$Sex = as.factor(ifelse(grepl('Male', data_frame$SexuponOutcome), 'Male', ifelse(grepl('Unknown', data_frame$Sex), 'Unknown', 'Female')))

### SexuponOutcome
data_frame$isIntact = as.logical(grepl("Intact", data_frame$SexuponOutcome))
data_frame$SexuponOutcome = as.character(data_frame$SexuponOutcome)
data_frame$SexuponOutcome[which(data_frame$SexuponOutcome == "")] = "Unknown"
data_frame$SexuponOutcome = as.factor(data_frame$SexuponOutcome)

### AgeuponOutcome
data_frame$AgeuponOutcome = as.character(data_frame$AgeuponOutcome)
data_frame$AgeuponOutcomeInDays = 0
for(i in 1:nrow(data_frame)){
  if(data_frame$AgeuponOutcome[i] == ""){
    data_frame$AgeuponOutcomeInDays[i] = 365 # median of train
  }else{
    elements = unlist(strsplit(data_frame$AgeuponOutcome[i], " "))
    multiplicator = 1
    if(grepl("week", elements[2])){
      multiplicator = 12
    }else if(grepl("month", elements[2])){
      multiplicator = 12
    }else if(grepl("year", elements[2])){
      multiplicator = 365
    }
    data_frame$AgeuponOutcomeInDays[i] = as.numeric(elements[1]) * multiplicator
  }
}

### Breed
data_frame$Breed = as.character(data_frame$Breed)
data_frame$isMultipleBreed = as.logical(grepl("/", data_frame$Breed))
data_frame$BreedMixLogical = as.logical(grepl("Mix", data_frame$Breed))
data_frame$BreedMix = as.logical(grepl("Mix", data_frame$Breed))

### Color
data_frame$Color = as.character(data_frame$Color)
data_frame$ColorNumber = 1
data_frame$ColorMain = "Other"
for(i in 1:nrow(data_frame)){
  splitted_color = unlist(strsplit(data_frame$Color[i], "/"))
  data_frame$ColorNumber[i] = length(splitted_color)
  mainColor = unlist(strsplit(splitted_color[1], " "))[1]
  
  # data_frame$ColorMain[i] = mainColor
  if(mainColor == "Black"){
    data_frame$ColorMain[i] = "Black"
  }else if(mainColor == "Brown"){
    data_frame$ColorMain[i] = "Brown"
  }else if(mainColor == "White"){
    data_frame$ColorMain[i] = "White"
  }else if(mainColor == "Blue"){
    data_frame$ColorMain[i] = "Blue"
  }else if(mainColor == "Tan"){
    data_frame$ColorMain[i] = "Tan"
  }else if(mainColor == "Orange"){
    data_frame$ColorMain[i] = "Orange"
  }else if(mainColor == "Red"){
    data_frame$ColorMain[i] = "Red"
  }else if(mainColor == "Tricolor"){
    data_frame$ColorMain[i] = "Tricolor"
  }else if(mainColor == "Tortie"){
    data_frame$ColorMain[i] = "Tortie"
  }else if(mainColor == "Calico"){
    data_frame$ColorMain[i] = "Calico"
  }else if(mainColor == "Cream"){
    data_frame$ColorMain[i] = "Cream"
  }else if(mainColor == "Chocolate"){
    data_frame$ColorMain[i] = "Chocolate"
  }
}
data_frame$ColorMain = as.factor(data_frame$ColorMain)
data_frame$ColorNumber = as.integer(data_frame$ColorNumber)



cat("Reassembling datasets - ")
train = data_frame[1:nrow(train_csv), ]
test = data_frame[(nrow(train_csv)+1):nrow(data_frame), ]

cat("Building model - ")
random_forest = randomForest(OutcomeType ~ AnimalType + hasName + NameMultiple + Sex + SexuponOutcome + isIntact + AgeuponOutcomeInDays + ColorMain + ColorNumber + BreedMix + isMultipleBreed + DateTimeMonth + DateTimeYear + DateTimeSeason + DateTimeHour + DateTimeWeekDay, data=train, importance=TRUE)
print(importance(random_forest))
print(random_forest)
plot(random_forest)
prediction = predict(random_forest, newdata=test, type="vote")

cat("Prediction - ")
test_final = data.frame(ID=test$ID, prediction)

cat("Writing File ")
write.csv(test_final, "result.csv", quote=FALSE, row.names=FALSE)
