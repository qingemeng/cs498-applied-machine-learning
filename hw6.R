library("MASS")

data <- read_table(file = "data/hw6/housing.data.txt", col_names = FALSE)
colnames(data)[14] = 'medv'
fit <- lm(medv ~ ., data)
summary(fit)
plot(fit, id.n = 10)

new_data = data[-c(365, 366, 369, 370, 371, 372, 373, 413), ]
fit_new = lm(medv ~ ., new_data)
plot(fit_new, id.n = 10)

new_data2 = new_data[-c( 366, 368 , 167, 162), ]
fit_new2 = lm(medv ~ ., new_data2)
plot(fit_new2, id.n = 5)

par(mfrow=c(1,1))

b <- boxcox(fit_new)
b <- data.frame(b)
lambda <- b$x[which.max(b$y)]

fit_transformed <- lm(((medv^lambda-1)/lambda) ~ ., new_data)
plot(x=fitted(fit_transformed) , y=stdres(fit_transformed), xlab = 'Fitted values', ylab = 'Standardized Residuals')
plot((y =(predict(fit_transformed) * lambda + 1) ^ (1/lambda)) , x= new_data$medv, xlab = 'True', ylab = 'Predicted')
