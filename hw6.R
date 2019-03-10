library("MASS")

data <- read_table(file = "data/hw6/housing.data.txt", col_names = FALSE)
colnames(data)[14] = 'medv'
fit <- lm(medv ~ ., data)
plot(fit, id.n = 10)

new_data = data[-c(187, 365, 366, 368, 369, 370, 371, 372, 373, 413), ]
fit_new = lm(medv ~ ., new_data)
plot(fit_new)

par(mfrow=c(1,1)) 
b <- boxcox(fit_new)
b <- data.frame(b)
lambda <- b$x[which.max(b$y)]

fit_transformed <- lm(((medv^lambda-1)/lambda) ~ ., new_data)
plot(fit_transformed)
plot(x=fitted(fit_transformed) , y=stdres(fit_transformed))
plot(((predict(fit_transformed) * lambda + 1) ^ (1/lambda)) , new_data$medv)
