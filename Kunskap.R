# Remove everything in columns 11 and onwards
data <- read.csv("C:/Users/leona/EC-Data science/R/Kunskapskontroll/ec_r-main/ec_r-main/Cars_data_feed_2.csv", header = TRUE, sep = ",")
feed <- data[,1:10]
# Print the feed
head(feed, 5)
tail(feed, 5)

# Get the number of empty cells in each column and print the result
empty_cells <- sapply(feed, function(x) sum(is.na(x)))
print(empty_cells)

# Get the type of each column and print the result
column_types <- sapply(feed, class)
print(column_types)


# For each of the columns 3, 6 - 9 of feed, remove the leading, middle, and trailing whitespaces
feed[,c(3,6:9)] <- sapply(feed[,c(3,6:9)], function(x) gsub("^\\s+|\\s+$", "", x))

# Convert all characters in the columns 3, 6 - 9 of feed to small letters
feed[,c(3,6:9)] <- sapply(feed[,c(3,6:9)], function(x) tolower(x))

# Remove the square brackets from the cells of the column 6 of feed
feed[,6] <- gsub("\\[|\\]", "", feed[,6])

# Convert the fourth column of feed to character
feed[,4] <- as.character(feed[,4])

# Remove all spaces in the cells of the last column of feed and convert them all to integers
feed[,10] <- as.integer(gsub(" ", "", feed[,10]))

# Print out each unique value and its frequency in the first column of feed
print(table(feed[,3]))

feed[,6][2]

x <- 0
### Binning av färger
for (färg in feed[,6]) {
  x <- x + 1
  feed[,6][x] <- switch (färg,
                         'ljusgrå' = 'grå',
                         'mörkblå' = 'blå',
                         'mörkgrå' = 'grå',
                         'ljusgrön' = 'grön',
                         'mörkgrön' = 'grön',
                         färg)
}

print(table(feed[,6]))


print(feed)

# Get the number of unique values in each column and print the result, each one in a new line
unique_values <- sapply(feed, function(x) length(unique(x)))
print(unique_values)

feed.data <- data.frame(feed)
print(feed.data)

# Set the seed for reproducibility
set.seed(123)

# Calculate the number of rows to select for the test set
n <- nrow(feed.data)
k <- round(0.20 * n)

# Randomly select 'k' rows for the test set
test_indices <- sample(1:n, size = k)
test_set <- feed.data[test_indices, ]

# The remaining data will be used for the training set
train_set <- feed.data[-test_indices, ]

# Print the train and test sets
print(table(train_set[,8]))
print(test_set)



# Boxplot för att se outliers, kolla normalitet, linjäritet, correlationer.
install.packages("data.table")
library(data.table)

plot(feed)
pairs(feed[,c(1,2,5,10)])
plot(feed$Modellår, feed$Hästkrafter, main="Hästkrafter över Modellår", xlab="Modellår", ylab="Hästkrafter")
plot(feed$Modellår, feed$Miltal, main="Miltal över Modellår", xlab="Modellår", ylab="Miltal")

#Modellår verkar ha en Kolliniaritet med mital och hästkrafter


## Eftersom vi har alldelles för många features så har jag valt att ta bort 
## Dessa för att inte det ska påverka modellen på ett dåligt sätt


rownames(train_set) <- NULL
train_set <- train_set[c(-271, -291, -9), ]
print(train_set[207,])


library('olsrr')
library('QuantPsyc')

fit <- lm(Pris..Y. ~ . - Län  - Modell - Modellår, data=train_set)
fit_transformed <- lm(sqrt(Pris..Y.) ~ .  - Län  - Modell - Modellår, data=train_set)
fit2 <- lm(Pris..Y. ~. - Län  - Modell - Modellår, data=train_set)
fit <- lm(sqrt(Pris..Y.) ~ Miltal + Hästkrafter + Märke + Biltyp, data=train_set)

plot(fit)
plot(fit_transformed)

ols_plot_resid_hist(fit)
ols_test_normality(fit)

summary(fit)
print(fit$coefficients)
par(mfrow=c(2,2))
plot(fit)
vif(fit)
cooks.distance(fit)
# Print the ordered leverage values
print(ordered_hats)
plot(cooks.distance(fit))

#Checking dimensionality p<<n
coef <- fit$coefficients
n_coef <- length(coef)     
n_obs <- nrow(train_set)
print(n_obs/n_coef)

# Best subset selection

Bestfit_p <- ols_step_best_subset(fit_transformed, penter=0.05)
Bestfit_p

plot(Bestfit_p)

#

par(mfrow=c(1,1))
plot(regfit.full, scale='Cp')
coef(regfit.full, which.min(reg.summary$cp))

library(caret)

vif_values <- vif(fit)

# Print VIF values
print(vif_values)



model <- train(sqrt(Pris..Y.) ~ Miltal + Hästkrafter + Märke + Biltyp, 
                    method='lm',
                    data=train_set,
                    trControl = trainControl(
                      method = 'cv',
                      number = 10,
                      verboseIter = TRUE
                    )
               )
print(model)


predictions <- predict(fit, test_set)
print(predictions)
R_sq <- R2(predictions, sqrt(test_set$Pris..Y.))
RMSE <- RMSE(predictions, sqrt(test_set$Pris..Y.))
MAE <- MAE(predictions, sqrt(test_set$Pris..Y.))
R_sq <- R2(predictions, test_set$Pris..Y.)
RMSE <- RMSE(predictions, test_set$Pris..Y.)
MAE <- MAE(predictions, test_set$Pris..Y.)

print(c(R_sq, RMSE, MAE))
mean((test_set$Pris..Y.))
print(sqrt(test_set$Pris..Y.))
pred_error_rate <- RMSE / mean((test_set$Pris..Y.))
pred_error_rate <- RMSE / mean(sqrt(test_set$Pris..Y.))
print(pred_error_rate)

#### SCB

library(pxweb)

data <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarA")

px_data_frame <- as.data.frame(data, column.name.type = "text", variable.value.type = "text")


px_data_frame

plot(px_data_frame)

# PXWEB query 
pxweb_query_list <- 
  list("Region"=c("00"),
       "Agarkategori"=c("060"),
       "ContentsCode"=c("TK1001AB"),
       "Tid"=c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarA",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments 
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as 
pxweb_cite(px_data)

px_data_frame

x <- px_data_frame$år
y <- px_data_frame$`Personbilar i trafik`

par(mfrow=c(1,1))
plot(x, y, type='l', main="Personbilar i trafik per År (i 1000)", xlab="År", ylab="Personbilar i Trafik")
