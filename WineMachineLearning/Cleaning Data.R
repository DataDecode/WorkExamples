white <- read.csv('winequality-white.csv', header = TRUE, sep=";")
white$type <- "white"

red <- read.csv('winequality-red.csv', header = TRUE, sep=";")
red$type <- "red"

wine <- rbind(white, red)

#basic data tests
any(is.na(wine)) #indicates any missing data



#randomize data
wine_data <- wine[ sample(1:nrow(wine), nrow(wine)) , ]

#cut out test and training sets
test_rows <- sample(1:nrow(wine), floor(nrow(wine) / 5))
wine_test <- wine_data[ test_rows , ]
wine_training <- wine_data[ -test_rows , ]

#save reformed data
save(wine_test, wine_training, 
     file="wine_test_training.RData")

