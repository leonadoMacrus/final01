# Tải các gói cần thiết
install.packages("compositions")
install.packages("car")
install.packages("caret")

# Tải các gói cần thiết
library(compositions)
library(car)
library(caret)

# Đọc dữ liệu
df <- read.csv("acs2015_census_tract_data.csv")
df <- na.omit(df)

# Chọn các biến thành phần độc lập và biến giải thích
compositional_cols <- c('PrivateWork', 'PublicWork', 'SelfEmployed', 'FamilyWork')
compositional_data <- df[compositional_cols]
y <- df$IncomePerCap

# Thay thế giá trị 0 bằng một giá trị nhỏ
compositional_data[compositional_data == 0] <- 1e-6

# Áp dụng phép biến đổi clr
clr_data <- clr(compositi
+onal_data)

# Chia dữ liệu thành tập huấn luyện và tập kiểm tra
set.seed(42)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- clr_data[train_index, ]
X_test <- clr_data[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Xây dựng mô hình hồi quy tuyến tính
model_clr <- lm(y_train ~ ., data = as.data.frame(X_train))

# Dự đoán và đánh giá mô hình
y_pred_clr <- predict(model_clr, newdata = as.data.frame(X_test))
rmse_clr <- sqrt(mean((y_test - y_pred_clr)^2))
print(paste("RMSE của mô hình hồi quy tuyến tính kết hợp CoDA:", rmse_clr))

# Dữ liệu chưa biến đổi
X_raw <- df[compositional_cols]

# Chia dữ liệu thành tập huấn luyện và tập kiểm tra
X_train_raw <- X_raw[train_index, ]
X_test_raw <- X_raw[-train_index, ]

# Xây dựng mô hình hồi quy tuyến tính
model_raw <- lm(y_train ~ ., data = X_train_raw)

# Dự đoán và đánh giá mô hình
y_pred_raw <- predict(model_raw, newdata = X_test_raw)
rmse_raw <- sqrt(mean((y_test - y_pred_raw)^2))
print(paste("RMSE của mô hình hồi quy tuyến tính thông thường:", rmse_raw))
