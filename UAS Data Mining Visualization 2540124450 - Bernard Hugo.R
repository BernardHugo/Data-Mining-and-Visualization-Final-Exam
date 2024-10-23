# Nama: Bernard Hugo
# Kelas: LA05
# NIM: 2540124450

# UAS Data Mining & Visualization

# Nomor 1

library(readxl)

df1 <- read_excel("Insurance.xlsx")
df1

summary(df1)

# a. Define the most suitable regression model based on all predictor variables and response 
# variable. Explain your answer.

# Answer: Linear regression

# Sangat berguna dalam mencocokkan model 
# regresi yang menggambarkan hubungan antara 
# variabel-variabel prediktor / independen 
# dengan satu variabel respons / dependen numerik.

# Dari dataset insurance.xlsx, 
# response variable = expenses


# b. Construct the model regression you 
# choose based on answer (a).

model1 <- lm(expenses ~ ., data = df1)
model1

summary(model1)

# Sex, smoker, dan region dijadikan numeric

Smoker <- ifelse(df1$smoker == "yes", 1, 0)
Smoker <- as.numeric(Smoker)
Smoker

Sex <- as.numeric(as.factor(df1$sex))
Sex
# 1 = female, 2 = male 

Region <- as.numeric(as.factor(df1$region))
Region
# 1 = northeast, 2 = northwest, 
# 3 = southeast, 4 = southwest

# Membuat linear regression plot 

model_bmi <- lm(expenses ~ bmi, data = df1)
plot(expenses ~ bmi, data = df1) + title("Linear Regression Plot of Expenses and BMI")
abline(model_bmi)

model_child <- lm(expenses ~ children, data = df1)
plot(expenses ~ children, data = df1) + title("Linear Regression Plot of Expenses and Children")
abline(model_child)

model_age <- lm(expenses ~ age, data = df1)
plot(expenses ~ age, data = df1) + title("Linear Regression Plot of Expenses and Age")
abline(model_age)

model_reg <- lm(expenses ~ Region, data = df1)
plot(expenses ~ Region, data = df1) + title("Linear Regression Plot of Expenses and Region")
abline(model_reg)

model_smok <- lm(expenses ~ Smoker, data = df1)
plot(expenses ~ Smoker, data = df1) + title("Linear Regression Plot of Expenses and Smoker")
abline(model_smok)

model_sex <- lm(expenses ~ Sex, data = df1)
plot(expenses ~ Sex, data = df1) + title("Linear Regression Plot of Expenses and Sex")
abline(model_sex)

# c. Interpret your regression model. Give the details.

# Variable-variabel prediktor/independen yang 
# memiliki hubungan yang erat dengan expenses 
# adalah bmi, children, smoker, dan region. 
# Sedangkan age dan sex memiliki kaitan yang 
# tidak erat dengan expenses, karena bergantung
# pada gaya hidup masing-masing orang.


# d. Do the significance test for all parameters and 
# analyze the goodness of the model.

summary(model1)

# Significance test semua parameter dilihat pada 
# p - value.
# Kebagusan model dilihat pada hasil R-squared.


# Nomor 2

df2 <- read_excel("Breast Cancer Wisconsin (Diagnostic).xlsx")
df2

summary(df2)

# a. Define the most suitable classification method based on all predictor variables and response 
# variable. Explain your answer.

# Answer: Logistic regression

# Berguna untuk memprediksi hasil data biner variabel respons
# berdasarkan variable-variabel prediktor.
# Variable respons: diagnosis (“M” – Maligna dan “B” – Benign). 


# b. Construct the model regression you choose based on answer (a).

# Diagnosis dijadikan numerik supaya bisa dimodelkan atau lm
Diagnosis <- ifelse(df2$diagnosis == "M", 1, 0)
Diagnosis <- as.numeric(Diagnosis)
Diagnosis

model2 <- lm(Diagnosis ~ ., data = df2)
model2

summary(model2)

# Membuat plot

model_breast <- lm(Diagnosis ~ concavity_mean, data = df2)
plot(Diagnosis ~ concavity_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Concavity Mean")
abline(model_breast)

model_breast1 <- lm(Diagnosis ~ radius_mean, data = df2)
plot(Diagnosis ~ radius_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Radius Mean")
abline(model_breast1)

model_breast2 <- lm(Diagnosis ~ area_mean, data = df2)
plot(Diagnosis ~ area_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Area Mean")
abline(model_breast2)

model_breast3 <- lm(Diagnosis ~ fractal_dimension_mean, data = df2)
plot(Diagnosis ~ fractal_dimension_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Fractal Dimension Mean")
abline(model_breast3)

model_breast4 <- lm(Diagnosis ~ texture_mean, data = df2)
plot(Diagnosis ~ texture_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Texture Mean")
abline(model_breast4)

model_breast5 <- lm(Diagnosis ~ perimeter_mean, data = df2)
plot(Diagnosis ~ perimeter_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Perimeter Mean")
abline(model_breast5)

model_breast6 <- lm(Diagnosis ~ smoothness_mean, data = df2)
plot(Diagnosis ~ smoothness_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Smoothness Mean")
abline(model_breast6)

model_breast7 <- lm(Diagnosis ~ compactness_mean, data = df2)
plot(Diagnosis ~ compactness_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Compactness Mean")
abline(model_breast7)

model_breast8 <- lm(Diagnosis ~ `concave points_mean`, data = df2)
plot(Diagnosis ~ `concave points_mean`, data = df2) + title("Logistic Regression Plot of Diagnosis and Concave Point Mean")
abline(model_breast8)

model_breast9 <- lm(Diagnosis ~ symmetry_mean, data = df2)
plot(Diagnosis ~ symmetry_mean, data = df2) + title("Logistic Regression Plot of Diagnosis and Symmetry Mean")
abline(model_breast9)


# c. Interpret your regression model. Give the details.

summary(model2)

# Perempuan dengan diagnosis Maligna rata-rata 
# memiliki variable-variabel prediktor yang 
# jumlahnya lebih besar daripada perempuan 
# dengan kanker diagnosis Benign. Karena Maligna 
# merupakan tumor yang ganas dan mengakibatkan 
# kanker tinggi pada wanita.  


# d. From the dataset, use 70% of the data as training data and 30% of the data as a testing data 
# then construct the classification model and define the accuracy of your classification model

# Panggil dataset 2 dalam bentuk csv agar bisa 
# digunakan pada data training 
dataf2 <- read.csv("Breast Cancer Wisconsin (Diagnostic) (1).csv")
dataf2

library(caTools)

# Menggunakan 70% data untuk training
set.seed(1000)
split <- sample.split(dataf2, SplitRatio = 0.7)
dataf2_model <- dataf2[split,]
dataf2_test <- dataf2[!split,]

split
dataf2_model
dataf2_test

Diagnosis2 <- ifelse(dataf2_model$diagnosis == "M", 1, 0)
Diagnosis2 <- as.numeric(Diagnosis2)
Diagnosis2

model_logistic <- glm(Diagnosis2 ~ ., data = dataf2_model, family = 'binomial')
model_logistic

summary(model_logistic)

Diagnosis3 <- ifelse(dataf2_test$diagnosis == "M", 1, 0)
Diagnosis3 <- as.numeric(Diagnosis3)
Diagnosis3

prediction <- predict(model_logistic, dataf2_test, type="response")
# Cek probabilitas
prediction <- ifelse(prediction < 0.5, 0, 1)
prediction

# Construct classification model
model_pred <- prediction(prediction, Diagnosis3)
model_perform <- performance(model_pred, measure = "tpr", x.measure = "fpr")
auc <- performance(model_pred, measure = "auc")
auc <- auc@y.values[[1]]

plot(model_perform)
abline(a = 0, b = 1)
auc <- round(auc, 4)

# Menghitung akurasi
accuracy <- sum(Diagnosis3 == prediction)/length(Diagnosis3)
print(paste('Accuracy = ', accuracy))