#Selecting the dataset
pokemon <- read.csv(file.choose(), sep=",", header = TRUE, stringsAsFactors = FALSE)
pokemon

#Data Cleaning
# Install and load the libraries for Data cleaning purpose
#install.packages("cleaner")
library(cleaner)
#install.packages("janitor")
library(janitor)
#Converting Logical type to factors
pokemon$Legendary <- as.factor(pokemon$Legendary)

#Renaming column names to simpler names
colnames(pokemon)[colnames(pokemon) == 'Type.1'] = 'type'
colnames(pokemon)[colnames(pokemon) == 'Sp..Atk'] = 'Spl_Atk'
colnames(pokemon)[colnames(pokemon) == 'Sp..Def'] = 'Spl_Def'
pokemon <- clean_names(pokemon)    #from upper to lower case of column names

#Descriptive Analysis and Exploratory Data Analysis
library(psych)
str(pokemon)
describe(pokemon)

#Bar plot - Count the Number of Pokemons in Each Type
library(ggplot2)
pokemon %>% ggplot(aes(x = type,  fill = as.factor(type))) + geom_bar(color = 'sienna') + 
  scale_fill_discrete(name = 'Type') +
  xlab('Pokemon Type') + ylab('Count') + theme_bw() + coord_flip()

#SUBSET FOR REGRESSION 
library(Hmisc)
sub_pokemon <- subset(pokemon, select=total:speed)

#REGRESSION MODEL
model <- lm(total ~ ., data = sub_pokemon)
summary(model)

#Multi collinearity
library(car)
vif(model)

#Correlation Values
corr_pokemon <- round(cor(sub_pokemon), 2)

#Correlation Matrix
cor_matrix <- rcorr(as.matrix(corr_pokemon))
cor_matrix

#Coefficients
round(cor_matrix$r, 2)

#P-value
round(cor_matrix$P, 2)

#Correlation Plot 
library(corrplot)
corrplot(corr_pokemon, order = "hclust", method = "number",
         tl.col = "black", tl.srt = 45)

#Scatter plot for highest correlation with respect to Total
ggplot(sub_pokemon, aes(x=total, y=spl_atk)) +
  geom_point() + xlab("Total") + ylab("Special Attack")

#Scatter plot for lowest correlation with respect to Total
ggplot(sub_pokemon, aes(x=total, y=speed)) +
  geom_point() + xlab("Total") + ylab("Speed")


#-------------------------------------------------------------------------------------------------------------#
#CHI-SQUARE AND ANOVA ANALYSIS

#ANOVA MODEL

#Performing Anova Test
output_anova <- aov(total ~ ., data = sub_pokemon)

#Summary of Anova Test
summary(output_anova)


#Chi-squared Test
#Create a vector for each row
special_attack <- pokemon$spl_atk
special_defence <- pokemon$spl_def

#State the number of rows for the Matrix
row_matrix_special = 2

#Create a matrix from the rows
matrix_special <- matrix(c(special_attack, special_defence), 
                         nrow=row_matrix_special, byrow = TRUE)
#matrix_special

#Performing Chi-squared Test
output_special <- chisq.test(matrix_special)
output_special

#Calculate Critical Value
qchisq(p = 0.05, df = 3, lower.tail = FALSE)

#Extract Test Value
output_special$statistic

#Making a Decision
ifelse(output_special$p.value > 0.05,
       "fail to reject null hypothesis",
       "reject null hypothesis")


#-------------------------------------------------------------------------------------------------------------#
#GLM ANALYSIS

# Point 1 - Splitting the dataset into 2 sets - Train and Test sets
split_pokemon <- sort(sample(x = nrow(pokemon), size = nrow(pokemon) * 0.75))
train_pokemon <- pokemon[split_pokemon,]
test_pokemon <- pokemon[-split_pokemon,]

# Point 2 - Generalized Linear Model
#Model
train_model <- glm(as.factor(legendary) ~ attack + defense + speed,
                   data = train_pokemon, family = binomial(link = "logit"))
summary(train_model)

#Point 3 - Confusion Matrix
legend_data <- data.frame(attack = c(min(train_pokemon$attack), 
                                     mean(train_pokemon$attack), max(train_pokemon$attack)),
                          defense = c(min(train_pokemon$defense), 
                                      mean(train_pokemon$defense), max(train_pokemon$defense)),
                          speed = c(min(train_pokemon$speed), 
                                    mean(train_pokemon$speed), max(train_pokemon$speed)))
legend_data 

legend_data$probs <- predict(train_model, legend_data, type = "response")
legend_data

train_prob <- predict(train_model, newdata = train_pokemon, type = "response")
predicted_train_min <- as.factor(ifelse(train_prob >= 0.5, "TRUE", "FALSE"))

#install.packages("caret")
# Loading caret for Confusion Matrix
library(caret)
confusionMatrix(predicted_train_min, as.factor(train_pokemon$legendary), positive = 'TRUE')

#Point 4 - Identifying True Positive, True Negative, False Positive and False Negative
TP = 9
TN = 547
FP = 35
FN = 9

#Point 5 - Calculating accuracy, precision, recall, specificity
accuracy = (TN + TP)/(TN+FP+FN+TP)
accuracy
precision = TP/(FP+TP)
precision
recall = TP/(TP+FN)
recall
specificity = TN/(TN+FP)
specificity


#Point 6 - Confusion Matrix using Test Set
test_prob <- predict(train_model, newdata = test_pokemon, type = "response")
predicted_test_min <- as.factor(ifelse(test_prob >= 0.5, "TRUE", "FALSE"))
confusionMatrix(predicted_test_min, as.factor(test_pokemon$legendary), positive = 'TRUE')

#Point 7 - Plotting the Receiver Operating Characteristic 
library(pROC)
ROC = roc(test_pokemon$legendary, test_prob)
X <- plot(ROC, col = "royalblue", ylab = "Sensitivity = TP rate", xlab = 'specificity = FP rate')

#Point 8 - Calculating the Area Under the Curve
AUC = auc(ROC)
AUC

