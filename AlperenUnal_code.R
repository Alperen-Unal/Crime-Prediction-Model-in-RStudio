
# This coursework predicts per capita violent crimes in communities based on attributes 
# like urban population and police statistics. The goal is to build an interpretable model 
# to inform crime prevention strategies, while considering limitations such as omitted 
# communities and missing LEMAS data.

# My dataset's link is: https://archive.ics.uci.edu/dataset/183/communities+and+crime
# I imported my dataset manually and assigned it as 'df'

df = communities

# To ensure equality, I removed the last two rows.
df = df[1:(nrow(df) - 2), ]

set.seed(123)

# Using sample() for random indices for the dataset
indices = sample(1:nrow(df), replace = FALSE)

total_rows = nrow(df)
part_size = floor(total_rows / 3)

# Creating of training, validation, and test sets
training_set = df[indices[1:part_size], ]
validation_set = df[indices[(part_size + 1):(2 * part_size)], ]
test_set = df[indices[(2 * part_size + 1):total_rows], ]

View(training_set)

# Number of records
num_records = nrow(training_set)

# Number of variables
num_variables = ncol(training_set)

cat("Number of records:", num_records, "\n")
cat("Number of variables:", num_variables, "\n")


# Names of input attributes (excluding the dependent variable)
input_attributes = names(training_set)[-ncol(training_set)]

# Types of input attributes
input_types = sapply(training_set[, input_attributes], class)

# Print the results
cat("Input Attribute Names:", paste(input_attributes, collapse = ", "), "\n")
cat("Input Attribute Types:", paste(input_types, collapse = ", "), "\n")
 
#The column names are just for demonstration, I will show what they mean with their new names.

new_names = c("state", "county", "community", "communityname", "fold", "population", "householdsize",
              "racepctblack", "racePctWhite", "racePctAsian", "racePctHisp", "agePct12t21", "agePct12t29",
              "agePct16t24", "agePct65up", "numbUrban", "pctUrban", "medIncome", "pctWWage", "pctWFarmSelf",
              "pctWInvInc", "pctWSocSec", "pctWPubAsst", "pctWRetire", "medFamInc", "perCapInc", "whitePerCap",
              "blackPerCap", "indianPerCap", "AsianPerCap", "OtherPerCap", "HispPerCap", "NumUnderPov",
              "PctPopUnderPov", "PctLess9thGrade", "PctNotHSGrad", "PctBSorMore", "PctUnemployed", "PctEmploy", 
              "PctEmplManu", "PctEmplProfServ", "PctOccupManu", "PctOccupMgmtProf", "MalePctDivorce",
              "MalePctNevMarr", "FemalePctDiv", "TotalPctDiv", "PersPerFam", "PctFam2Par", "PctKids2Par",
              "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids", "PctWorkMom", "NumIlleg", "PctIlleg",
              "NumImmig", "PctImmigRecent", "PctImmigRec5", "PctImmigRec8", "PctImmigRec10", "PctRecentImmig",
              "PctRecImmig5", "PctRecImmig8", "PctRecImmig10", "PctSpeakEnglOnly", "PctNotSpeakEnglWell", 
              "PctLargHouseFam", "PctLargHouseOccup", "PersPerOccupHous", "PersPerOwnOccHous", "PersPerRentOccHous",
              "PctPersOwnOccup", "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant", "PctHousOccup",
              "PctHousOwnOcc", "PctVacantBoarded", "PctVacMore6Mos", "MedYrHousBuilt", "PctHousNoPhone", 
              "PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal", "OwnOccHiQuart", "RentLowQ", "RentMedian",
              "RentHighQ", "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc", "MedOwnCostPctIncNoMtg", "NumInShelters",
              "NumStreet", "PctForeignBorn", "PctBornSameState", "PctSameHouse85", "PctSameCity85", "PctSameState85",
              "LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps", "LemasSwFTFieldPerPop", "LemasTotalReq",
              "LemasTotReqPerPop", "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol", "PctPolicWhite", "PctPolicBlack",
              "PctPolicHisp", "PctPolicAsian", "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", "PolicAveOTWorked",
              "LandArea", "PopDens", "PctUsePubTrans", "PolicCars", "PolicOperBudg", "LemasPctPolicOnPatr", "LemasGangUnitDeploy",
              "LemasPctOfficDrugUn", "PolicBudgPerPop", "ViolentCrimesPerPop")
colnames(training_set) = new_names
training_set

# I will also change the names of our main data, validation, and test set's attributes.
colnames(df) = new_names
colnames(validation_set) = new_names
colnames(test_set) = new_names

# I detect missing values represented as "?". We should use "NA" instead.
# Replace '?' with NA in the entire dataset
df[df == "?"] = NA

# Replace '?' with NA in all three sets
training_set[training_set == "?"] = NA
validation_set[validation_set == "?"] = NA
test_set[test_set == "?"] = NA

# Check for missing values in each set
missing_values_df = sum(is.na(df))
missing_values_train = sum(is.na(training_set))
missing_values_val = sum(is.na(validation_set))
missing_values_test = sum(is.na(test_set))

cat("Missing Values in df:", missing_values_df, "\n")
cat("Missing Values in Training Set:", missing_values_train, "\n")
cat("Missing Values in Validation Set:", missing_values_val, "\n")
cat("Missing Values in Test Set:", missing_values_test, "\n")

# The percentage of missing values for each variable in the training set
missing_percentage_train = colMeans(is.na(training_set)) * 100
cat("Missing Value Percentages in Training Set:\n")
print(missing_percentage_train)

# Name of the output attribute (dependent variable)
output_attribute = names(training_set)[ncol(training_set)]

# Type of the output attribute
output_type = class(training_set[, output_attribute])
cat("Output Attribute Name:", output_attribute, "\n")
cat("Output Attribute Type:", output_type, "\n")

# Compute variable correlations
# The first five columns are non-predictive; for this reason, I don't include these columns for the correlation computation.
# To calculate variable correlations, ensuring all of the variables are numeric is critical.

columns_to_convert = names(training_set)[6:ncol(training_set)]
training_set[, columns_to_convert] = lapply(training_set[, columns_to_convert], as.numeric)
sapply(training_set, class)


columns_for_correlation = names(training_set)[6:ncol(training_set)]
correlation_matrix = cor(training_set[, columns_for_correlation])
print(correlation_matrix)


# To Compute variable correlations with the target value "ViolantCrimesPerPop",
# excluding the first five columns from the correlation analysis is critical.
correlations = cor(training_set[, -(1:5)])[, "ViolentCrimesPerPop"]
sorted_correlations = sort(correlations, decreasing = TRUE)
print(sorted_correlations)

# Due to the number of variables, the scatter plots can't be read clearly. 
# Therefore, we should pick variables that have the highest correlation coefficient with the target value. 

target_variable = "ViolentCrimesPerPop"

# Get the names of the top seven predictors
top_predictors = names(sort(abs(correlation_matrix[target_variable, ]), decreasing = TRUE)[1:7])

# Create scatter plot matrix
pairs(training_set[, top_predictors])

panelWithCurve <- function(x, y, ...) {
  points(x, y, ...)
  loess_model <- loess(y ~ x, span = 0.9) 
  loess_predicted <- predict(loess_model, data.frame(x = x))
  
  lines(x, loess_predicted, col = "red")
}

#To avoid non-fatal warnings,  suppressWarnings() is suggested to be used.
suppressWarnings({
# Let's create scatter plot matrices with target attribute and other variables to read the data more clear.
pairs(training_set[, c("population", "householdsize", "racepctblack", "racePctWhite", "racePctAsian", "racePctHisp", "ViolentCrimesPerPop")], panel = panelWithCurve )
pairs(training_set[, c("agePct12t21", "agePct12t29", "agePct16t24", "agePct65up", "numbUrban", "pctUrban", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("medIncome", "pctWWage", "pctWFarmSelf", "pctWInvInc", "pctWSocSec", "pctWPubAsst", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("pctWRetire", "medFamInc", "perCapInc", "whitePerCap", "blackPerCap", "indianPerCap", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("AsianPerCap", "OtherPerCap", "HispPerCap", "NumUnderPov", "PctPopUnderPov", "PctLess9thGrade", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PctNotHSGrad", "PctBSorMore", "PctUnemployed", "PctEmploy", "PctEmplManu", "PctEmplProfServ", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PctOccupManu", "PctOccupMgmtProf", "MalePctDivorce", "MalePctNevMarr", "FemalePctDiv", "TotalPctDiv", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PersPerFam", "PctFam2Par", "PctKids2Par", "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PctWorkMom", "NumIlleg", "PctIlleg", "NumImmig", "PctImmigRecent", "PctImmigRec5", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PctImmigRec8", "PctImmigRec10", "PctRecentImmig", "PctRecImmig5", "PctRecImmig8", "PctRecImmig10", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PctSpeakEnglOnly", "PctNotSpeakEnglWell", "PctLargHouseFam", "PctLargHouseOccup", "PersPerOccupHous", "PersPerOwnOccHous", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PersPerRentOccHous", "PctPersOwnOccup", "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PctHousOccup", "PctHousOwnOcc", "PctVacantBoarded", "PctVacMore6Mos", "MedYrHousBuilt", "PctHousNoPhone", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal", "OwnOccHiQuart", "RentLowQ", "RentMedian", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("RentHighQ", "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc", "MedOwnCostPctIncNoMtg", "NumInShelters", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("NumStreet", "PctForeignBorn", "PctBornSameState", "PctSameHouse85", "PctSameCity85", "PctSameState85", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps", "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol", "PctPolicWhite", "PctPolicBlack", "PctPolicHisp", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PctPolicAsian", "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", "PolicAveOTWorked", "LandArea", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("PopDens", "PctUsePubTrans", "PolicCars", "PolicOperBudg", "LemasPctPolicOnPatr", "LemasGangUnitDeploy", "ViolentCrimesPerPop")], panel = panelWithCurve)
pairs(training_set[, c("LemasPctOfficDrugUn", "PolicBudgPerPop", "ViolentCrimesPerPop")], panel = panelWithCurve)
})
# Looking for missing values in the 3 sets.
# Since the missing values were identified before, checking the outputs again is would be a good idea.

cat("Missing Value Percentages in Training Set:\n")
print(missing_percentage_train)

# The code of the percentage of missing values for the test and the validation sets will be used.

missing_percentage_validation = colMeans(is.na(validation_set)) * 100
missing_percentage_test = colMeans(is.na(test_set)) * 100

cat("Missing Value Percentages in Validation Set:\n")
print(missing_percentage_validation)

cat("Missing Value Percentages in Test Set:\n")
print(missing_percentage_test)

# 2 of the non-predictive attributes have a huge amount of missing value.
# Since these attributes don't affect the dependent variable, these two attributes can be discarded.

training_set = training_set[, -which(names(training_set) %in% c("county", "community"))]
validation_set = validation_set[, -which(names(validation_set) %in% c("county", "community"))]
test_set = test_set[, -which(names(test_set) %in% c("county", "community"))]

# The predictive attributes which have not any correlation with the target value can be discarded.
# These attributes need to be sorted to be detected

new_correlations = cor(training_set[, -(1:3)])[, "ViolentCrimesPerPop"]
na_correlation_variables = names(new_correlations[is.na(new_correlations)])
print(na_correlation_variables)

# Now these attributes can be removed from the training, validation and test sets.


training_set = training_set[, -which(names(training_set) %in% c("LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps",
                                                                "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", 
                                                                "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol",
                                                                "PctPolicWhite", "PctPolicBlack", "PctPolicHisp", "PctPolicAsian",
                                                                "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", 
                                                                "PolicAveOTWorked", "PolicCars", "PolicOperBudg", 
                                                                "LemasPctPolicOnPatr", "LemasGangUnitDeploy", "PolicBudgPerPop"))]

validation_set = validation_set[, -which(names(validation_set) %in% c("LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps",
                                                                      "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", 
                                                                      "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol",
                                                                      "PctPolicWhite", "PctPolicBlack", "PctPolicHisp", "PctPolicAsian",
                                                                      "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", 
                                                                      "PolicAveOTWorked", "PolicCars", "PolicOperBudg", 
                                                                      "LemasPctPolicOnPatr", "LemasGangUnitDeploy", "PolicBudgPerPop"))]

test_set = test_set[, -which(names(test_set) %in% c("LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps",
                                                    "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", 
                                                    "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol",
                                                    "PctPolicWhite", "PctPolicBlack", "PctPolicHisp", "PctPolicAsian",
                                                    "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", 
                                                    "PolicAveOTWorked", "PolicCars", "PolicOperBudg", 
                                                    "LemasPctPolicOnPatr", "LemasGangUnitDeploy", "PolicBudgPerPop"))]


# Let's create a new correlation matrix and examine it.

new_columns_for_correlation = names(training_set)[4:ncol(training_set)]
new_correlation_matrix = cor(training_set[, new_columns_for_correlation])
print(new_correlation_matrix)

# As can be seen, all the dependent attributes have a correlation with the target value now.
# Let's check the missing values again.

missing_values_train = sum(is.na(training_set))
missing_values_val = sum(is.na(validation_set))
missing_values_test = sum(is.na(test_set))

cat("Missing Values in Training Set:", missing_values_train, "\n")
cat("Missing Values in Validation Set:", missing_values_val, "\n")
cat("Missing Values in Test Set:", missing_values_test, "\n")

# There is one missing value in the test set. It needs to be found.

if (missing_values_test == 0) {
  cat("There aren't any missing values.\n")
} else {
  cat("Positions of missing values:\n")
  print(which(is.na(test_set), arr.ind = TRUE))
}

# Missing value is detected. 
# Let's calculate the mean of "OtherPerCap" and replace the mean instead of the missing value.

# But firstly, the type of column needs to be checked.
OtherPerCap_column_type = class(test_set$OtherPerCap)
cat("Type of 'OtherPerCap' column:", OtherPerCap_column_type, "\n")

# As can be seen the type of this column should be changed at first.
test_set$OtherPerCap = as.numeric(test_set$OtherPerCap)
any(is.na(test_set$OtherPerCap))
OtherPerCap_mean = mean(test_set$OtherPerCap, na.rm = TRUE)
print(OtherPerCap_mean)

# Replace missing values in 'OtherPerCap' column with 0.2825792
test_set$OtherPerCap = ifelse(is.na(test_set$OtherPerCap), 0.2825792, test_set$OtherPerCap)

# Let's check the missing values in the test set again:
missing_values_test = sum(is.na(test_set))
cat("Missing values in test set:", missing_values_test, "\n")

# The non-predictive variables should not be in the linear regression model.
# For this reason, these values from our model must be excluded.

non_predictive_columns = c("state", "communityname", "fold")
formula = as.formula(paste("ViolentCrimesPerPop ~ . -", paste(non_predictive_columns, collapse = " - ")))
lm_model = lm(formula, data = training_set)

# The variables which have the largest p-values will be removed.
summary_lm_model = summary(lm_model)
p_values = summary_lm_model$coefficients[, "Pr(>|t|)"]

# Identify variables with p-values greater than 0.8
variables_to_remove = names(p_values[p_values > 0.8])
variables_to_remove

# We will turn back to our training set and remove these values and then create a new model.
# Specify the columns to be removed
columns_to_remove = c(
  "racepctblack", "agePct12t29", "agePct65up", "pctUrban", 
  "pctWWage", "PctImmigRec8", "PctRecImmig5", "PctNotSpeakEnglWell", 
  "PctHousOwnOcc", "PctHousNoPhone", "PctWOFullPlumb", 
  "PctBornSameState", "PctSameHouse85", "PctSameCity85"
)

training_set = training_set[, setdiff(names(training_set), columns_to_remove)]

# The same 'formula' can be used.
formula = as.formula(paste("ViolentCrimesPerPop ~ . -", paste(non_predictive_columns, collapse = " - ")))

# Fit a linear regression model using the specified formula and the modified data.
new_lm_model = lm(formula, data = training_set)
summary(new_lm_model)

# As can be seen, the least statistically significant values are removed from the training data.
# But the set still has the least statistically significant values we will remove it manually again.
columns_to_remove_2 = c("NumUnderPov", "indianPerCap", "PctImmigRec10")
training_set = training_set[, setdiff(names(training_set), columns_to_remove_2)]

# Let's check the updated model's p-values
new_2_lm_model = lm(formula, data = training_set)
summary(new_2_lm_model)

# It seems the training set's attributes are cleaned. Now the other sets can be also cleaned.

columns_to_remove_for_all_sets = c("racepctblack", "agePct12t29", "agePct65up", "pctUrban", 
                                   "pctWWage", "PctImmigRec8", "PctRecImmig5", "PctNotSpeakEnglWell", 
                                   "PctHousOwnOcc", "PctHousNoPhone", "PctWOFullPlumb", 
                                   "PctBornSameState", "PctSameHouse85", "PctSameCity85",
                                   "NumUnderPov", "indianPerCap", "PctImmigRec10" )

validation_set = validation_set[, setdiff(names(validation_set), columns_to_remove_for_all_sets)]
test_set = test_set[, setdiff(names(test_set), columns_to_remove_for_all_sets)]
names(training_set)

# First 3  attributes are non-predictive. Due to this reason, the new training set without non-predictive attributes is created.
# So a linear regression model can be created without non_predictive valuables.

new_training_set = training_set[, 4:ncol(training_set)]

# This column was fitted with a numeric type in the model, but in the new data, it's being treated as a character type.
# Its type must be changed as numeric.

M1 = lm(ViolentCrimesPerPop ~ ., data = new_training_set)
summary(M1)

# Let's create a scatter plots with statistically significant values between the goal attribute:
# To Converge the most proper degree of the polynomial regression, loess curve will be used 
customPanelWithCurve <- function(x, y, ...) {
  points(x, y, ...)
  loess_model <- loess(y ~ x, span = 0.9) 
  loess_predicted <- predict(loess_model, data.frame(x = x))
  
# Using the loess curve
  lines(x, loess_predicted, col = "red")
}

# Apply the custom panel function to each pairs plot:
plot_with3stars = pairs(training_set[, c("racePctWhite", "PersPerOccupHous", "NumStreet", "ViolentCrimesPerPop")], panel = customPanelWithCurve)
plot_with2stars = pairs(training_set[, c("PctWorkMom", "NumIlleg", "PctIlleg", "HousVacant", "PctVacantBoarded", "ViolentCrimesPerPop")], panel = customPanelWithCurve)
plot_with1star = pairs(training_set[, c("agePct16t24", "pctWSocSec", "medFamInc", "MalePctNevMarr", "PctImmigRecent", "ViolentCrimesPerPop")], panel = customPanelWithCurve)
plot_with1star_2 = pairs(training_set[, c("PctPersOwnOccup", "PctHousLess3BR", "PctVacMore6Mos", "MedRent", "ViolentCrimesPerPop")], panel = customPanelWithCurve)
plot_with_dot = pairs(training_set[, c("numbUrban", "pctWInvInc", "MalePctDivorce", "PctWorkMomYoungKids", "NumImmig", "PctLargHouseOccup", "MedYrHousBuilt", "ViolentCrimesPerPop")], panel = customPanelWithCurve)

# Let's create polynomial regression models:

M2 = lm(ViolentCrimesPerPop ~ poly(medFamInc, 2) + poly(OwnOccHiQuart, 2) + poly(TotalPctDiv, 2), data = training_set)
M3 = lm(ViolentCrimesPerPop ~ poly(PctHousLess3BR, 2) + poly(PctPersOwnOccup, 2) + poly(MedRent, 2), data = training_set)
M4 = lm(ViolentCrimesPerPop ~ poly(pctWInvInc, 2) + poly(MalePctDivorce, 2) + poly(PctHousLess3BR, 2), data = training_set)

plot_for_M2 = pairs(training_set[, c("OwnOccHiQuart", "TotalPctDiv", "medFamInc", "ViolentCrimesPerPop")], panel = customPanelWithCurve)
plot_for_M3 = pairs(training_set[, c("PctHousLess3BR", "PctPersOwnOccup", "MedRent", "ViolentCrimesPerPop")], panel = customPanelWithCurve)
plot_for_M4 = pairs(training_set[, c("pctWInvInc", "MalePctDivorce", "PctHousLess3BR", "ViolentCrimesPerPop")], panel = customPanelWithCurve)


#KNN Models
# We start with fourth column because of the non-predictive attributes
X_train = training_set[, 4:ncol(training_set)]  
y_train = training_set$ViolentCrimesPerPop

X_valid = validation_set[, 4:ncol(validation_set)]  
y_valid = validation_set$ViolentCrimesPerPop
X_valid$OtherPerCap <- as.numeric(X_valid$OtherPerCap)

library(class)
set.seed(123)

# Build KNN models for k=1, 2, 3, 4
k_1 = knn(train = X_train, test = X_valid, cl = y_train, k = 1)
k_2 = knn(train = X_train, test = X_valid, cl = y_train, k = 2)
k_3 = knn(train = X_train, test = X_valid, cl = y_train, k = 3)
k_4 = knn(train = X_train, test = X_valid, cl = y_train, k = 4)

prediction_k_1 = mean(y_valid[k_1])
prediction_k_2 = mean(y_valid[k_2])
prediction_k_3 = mean(y_valid[k_3])
prediction_k_4 = mean(y_valid[k_4])

cat("Prediction for k = 1:", prediction_k_1, "\n")
cat("Prediction for k = 2:", prediction_k_2, "\n")
cat("Prediction for k = 3:", prediction_k_3, "\n")
cat("Prediction for k = 4:", prediction_k_4, "\n")

mse_k_1 = mean((y_valid - prediction_k_1)^2)
mse_k_2 = mean((y_valid - prediction_k_2)^2)
mse_k_3 = mean((y_valid - prediction_k_3)^2)
mse_k_4 = mean((y_valid - prediction_k_4)^2)

cat("MSE for k=1:", mse_k_1, "\n")
cat("MSE for k=2:", mse_k_2, "\n")
cat("MSE for k=3:", mse_k_3, "\n")
cat("MSE for k=4:", mse_k_4, "\n")

# Find the optimal k
optimal_k = which.min(c(mse_k_1, mse_k_2, mse_k_3, mse_k_4))
cat("Optimal k:", optimal_k, "\n")

# The optimal k value is 1.


M5 = knn(train = X_train, test = X_valid, cl = y_train, k = 1)



# Predictions from each linear regression model on the validation set:
prediction_M1 = predict(M1, newdata = X_valid)
prediction_M2 = predict(M2, newdata = X_valid)
prediction_M3 = predict(M3, newdata = X_valid)
prediction_M4 = predict(M4, newdata = X_valid)

# Predictions from the KNN model on the validation set:
k_1 = knn(train = X_train, test = X_valid, cl = y_train, k = 1)
prediction_M5 = mean(y_valid[k_1])

# Calculating MSE for each model:
mse_M1 = mean((y_valid - prediction_M1)^2)
mse_M2 = mean((y_valid - prediction_M2)^2)
mse_M3 = mean((y_valid - prediction_M3)^2)
mse_M4 = mean((y_valid - prediction_M4)^2)
mse_M5 = mean((y_valid - prediction_M5)^2)

cat("MSE for M1:", mse_M1, "\n")
cat("MSE for M2:", mse_M2, "\n")
cat("MSE for M3:", mse_M3, "\n")
cat("MSE for M4:", mse_M4, "\n")
cat("MSE for M5:", mse_M5, "\n")

# Finding the model with the minimum MSE
best_model = which.min(c(mse_M1, mse_M2, mse_M3, mse_M4, mse_M5))
cat("Best model:", best_model, "\n")

Mb = lm(ViolentCrimesPerPop ~ ., data = new_training_set)

# Assuming 'ViolentCrimesPerPop' is the target variable in both the training and test sets
# We don't want to encounter any errors here; due to this reason, we extract the non-predictive attributes
# from the test set as same as the training set for this model

new_test_set = test_set[, 4:ncol(test_set)] 
y_test = new_test_set$ViolentCrimesPerPop
X_test = new_test_set[, -which(names(test_set) == 'ViolentCrimesPerPop')]

predictions = predict(Mb, newdata = X_test)

mse = mean((y_test - predictions)^2)
print(paste("Mean Squared Error (MSE):", mse))

# Optionally, other metrics such as RMSE, MAE, and R-squared can be calculated.
rmse = sqrt(mse)
mae = mean(abs(y_test - predictions))
rsquared = 1 - (sum((y_test - predictions)^2) / sum((y_test - mean(y_test))^2))

print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("R-squared:", rsquared))

# In terms of the results Mb linear regression model seems to perform reasonably well on the test set.
# MSE, RMSE and MAE values suggest that the model's predictions are close to the actual values.
# Additionally, the R-squared value of 0.602 indicates a moderate level of explanatory power in predicting the target variable.



