rmse <- function(actual, predicted) { sqrt(mean((actual - predicted)^2)) }

price_prediction_error <- function(data) {
    # Create a data frame with only the relevant columns
    house_info <- data.frame(
        price = data$price,
        bedrooms = data$bedrooms,
        bathrooms = data$bathrooms,    # Updated column name
        sqft_living = data$sqft_living,
        sqft_lot = data$sqft_lot,
        grade = data$grade,
        yr_built = data$yr_built
    )
    
    # Separate the data into training and testing sets
    rows <- nrow(house_info)
    f <- 0.6  # 60% training, 40% testing
    set.seed(123)  # For reproducibility
    permuted_house_info <- house_info[sample(rows), ]
    
    upper_bound <- floor(f * rows)
    train.dat <- permuted_house_info[1:upper_bound, ]
    test.dat <- permuted_house_info[(upper_bound + 1):rows, ]
    
    # Fit a linear regression model using specified predictors
    house.lm <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + grade + yr_built, data = train.dat)
    
    # Make predictions on the testing set
    predicted.dat <- predict(house.lm, newdata = test.dat)
    
    # Calculate RMSE
    rmse_val <- rmse(test.dat$price, predicted.dat)
    
    return(rmse_val)
 }


house_data <- read.csv("kc_house_data.csv")
# Check column names to confirm correctness


data_by_zipcode <- house_data %>%
group_by(zipcode) %>%
summarize(
count = n(),
med_price = median(price),
med_yr_built = median(yr_built),
error = price_prediction_error(cur_data()) )
 # Print RMSE results by zip code
 print(data_by_zipcode)








