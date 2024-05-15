library(dplyr)
xq1 <- quantile(train$GrLivArea2, 0.50)
yq2 <- quantile(Y, 0.75)

rowcount <- dim(train)[1]
upperxq1yq2 <- filter(train, train$SalePrice > yq2 & train$GrLivArea2 > xq1) %>% count()
upperyq2 <- filter(train, train$SalePrice > yq2) %>% count()

(upperxq1yq2/rowcount) / (upperyq2/rowcount)



xq1 <- quantile(train$BsmtFinSF12, 0.25)
yq2 <- quantile(train$SalePrice, 0.5)

rowcount <- dim(train)[1]
upperxq1yq2 <- filter(train, train$SalePrice > yq2 & train$BsmtFinSF12 > xq1) %>% count()
upperyq2 <- filter(train, train$SalePrice > yq2) %>% count()

(upperxq1yq2/rowcount) / (upperyq2/rowcount)

