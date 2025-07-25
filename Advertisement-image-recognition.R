## TIEN XU LY DU LIEU
library(dplyr)
add <- read.csv("C:/Users/Admin/Downloads/add.csv")

head(add,5 )
add <- add[, -1]
# Xóa các cột từ thứ 4 đến thứ 1558 (có thể là các cột không liên quan đến phân tích)
add <- add[, -c(4:1558)]
colnames(add)[1] <- "height"
colnames(add)[2] <- "width"
colnames(add)[3] <- "ratio"
colnames(add)[4] <- "target"
head(add)

# Thay thế giá trị của biến mục tiêu 'target' từ "ad." thành 1 và "nonad" thành 0
add$target <- ifelse(add$target == "ad.", 1, 0)
table(add$target)
head(add)
# Chuyển các biến 'height', 'width', 'ratio' về kiểu số học (numeric)
add$height <- as.numeric(add$height)
add$width <- as.numeric(add$width)
add$ratio <- as.numeric(add$ratio)
# Thay thế tất cả dấu '?' trong dữ liệu thành NA
add[add == "   ?"] <- NA
add[add == "     ?"] <- NA

# Thống kê số lượng giá trị NA theo từng cột
na_counts <- colSums(is.na(add))

# Tính tỷ lệ phần trăm NA theo cột
na_percentage <- colMeans(is.na(add)) * 100

na_summary <- data.frame(
  NA_Count = na_counts,
  NA_Percent = round(na_percentage, 2)
)
print(na_summary)





na_counts <- colSums(is.na(add))
na_percentage <- colMeans(is.na(add)) * 100

na_summary <- data.frame(
  NA_Count = na_counts,
  NA_Percent = round(na_percentage, 2)
)
print(na_summary)


add <- add[!(is.na(add$height) | is.na(add$width)), ]
na_counts <- colSums(is.na(add))
na_percentage <- colMeans(is.na(add)) * 100
na_summary <- data.frame(
  NA_Count = na_counts,
  NA_Percent = round(na_percentage, 2)
)
print(na_summary)


##OUTLIERS

# Xu ly outliers

# --- Get the number of rows before outlier removal ---
n_rows_before <- nrow(add)
print(n_rows_before)
# ---

replace_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}

# Sử dụng lapply cho từng cột cụ thể
add[c("height", "width","ratio")] <- lapply(add[c("height", "width","ratio")], replace_outliers)


na_counts <- colSums(is.na(add))
# print(na_counts) # You can keep or remove this print if you don't need it

add <- add[!(is.na(add$height) | is.na(add$width) | is.na(add$ratio)), ]

# --- Get the number of rows after outlier removal ---
n_rows_after <- nrow(add)
# ---

# --- Calculate and print the number of rows removed ---
rows_removed <- n_rows_before - n_rows_after
print(paste("Tổng số dòng đã xóa do outliers:", rows_removed))




## THỐNG KÊ MÔ TẢ

library(psych)
# Hàm tính các thống kê mô tả cho một vector
df1=add[, c("height", "width", "ratio")]
summary_stats <- function(x) {
  c(
    count = sum(!is.na(x)),               # Số lượng giá trị không bị thiếu
    mean  = mean(x, na.rm = TRUE),         # Trung bình
    std   = sd(x, na.rm = TRUE),           # Độ lệch chuẩn
    min   = min(x, na.rm = TRUE),          # Giá trị nhỏ nhất
    Q1 = quantile(x, 0.25, na.rm = TRUE),# Phân vị 25%
    median = median(x, na.rm = TRUE),       # Trung vị (Phân vị 50%)
    Q3 = quantile(x, 0.75, na.rm = TRUE),# Phân vị 75%
    max   = max(x, na.rm = TRUE)           # Giá trị lớn nhất
  )
}

# Áp dụng hàm summary_stats cho từng cột của data frame
stats <- sapply(df1, summary_stats)

# In kết quả ra màn hình
print(stats)




# Vẽ Histogram cho 'height'
hist(df1$height,
     main = "Histogram of Height",
     xlab = "Height",
     col = "pink",  # Màu sắc
     border = "white")  # Viền trắng

# Vẽ Histogram cho 'width'
hist(df1$width,
     main = "Histogram of Width",
     xlab = "Width",
     col = "purple",
     border = "white")

# Vẽ Histogram cho 'ratio'
hist(df1$ratio,
     main = "Histogram of Ratio",
     xlab = "Ratio",
     col = "lightgreen",
     border = "white")



#Boxplot
boxplot(height ~ target,
         data = add,
        main = "Boxplot of height by target",
         xlab = "Target",
         ylab = "Height",
         col = c("pink", "lightblue"),
         names = c("Non-Ad (0)", "Ad (1)")
)

boxplot(width ~ target,
        data = add,
        main = "Boxplot of width by target",
        xlab = "Target",
        ylab = "Height",
        col = c("red", "lightblue"),
        names = c("Non-Ad (0)", "Ad (1)")
)

boxplot(ratio ~ target,
        data = add,
        main = "Boxplot of ratio by target",
        xlab = "Target",
        ylab = "Height",
        col = c("green", "lightblue"),
        names = c("Non-Ad (0)", "Ad (1)")
)







library(tidymodels)
library(caTools) 
library(randomForest) 
library(glmnet)
## THỐNG KÊ SUY DIỄN:
# Thiết lập lại ngẫu nhiên với set.seed để tái lập kết quả
table(add$target)

set.seed(123)
add$target = as.factor(add$target)
# Chia bộ dữ liệu thành train (70%) và test (30%)
sample_id <- sample(1:nrow(df1),  size = 0.7 * nrow(df1))
train <- add[sample_id, ]
test <- add[-sample_id, ]

#Logistic Regression

model_lg <- glm(target ~ height + width, data = train, family = binomial)
summary(model_lg)



#RandomForest

rf_model <- randomForest(target ~ height + width, data = train ,  ntree = 500, mtry = 2, importance = TRUE)  
# In kết quả tóm tắt của mô hình
print(rf_model)



#Du doan 

# Dự đoán nhãn
pred_class <- predict(rf_model, newdata = test, type = "response") # hoặc "class" đều được

# Dự đoán xác suất
pred_proba <- predict(rf_model, newdata = test, type = "prob")

# Kết hợp kết quả
results <- test %>%
  dplyr::select(target) %>%
  dplyr::mutate(pred_class = pred_class) %>%
  dplyr::bind_cols(as.data.frame(pred_proba))

# Tính accuracy
accuracy <- mean(results$pred_class == results$target)
print(accuracy)
# Hiển thị tầm quan trọng của các biến
importance(rf_model)
varImpPlot(rf_model)





library(pROC)





##DECISION TREE
library(rpart)
library(rpart.plot)


sample_tree <- sample(1:nrow(df1),  size = 0.7 * nrow(df1))
train_tree <- df1[sample_tree, ]
test_tree <- df1[-sample_tree, ]
# Xây dựng mô hình cây quyết định trên tập train
tree_model <- rpart(target ~ height + width , data = train_tree, method = "class", cp = 0.01)
rpart.plot(tree_model)

# Dự đoán lớp và xác suất
pred_class <- predict(tree_model, newdata = test_tree, type = "class")
pred_proba <- predict(tree_model, newdata = test_tree, type = "prob")

# Nếu pred_proba có cột có tên ví dụ "0" và "1", có thể đổi tên:
pred_proba <- as.data.frame(pred_proba) %>%
  rename(.pred_0 = `0`, .pred_1 = `1`)

# Tạo bảng kết quả với tên tiêu chuẩn cho dự đoán lớp
test_result_tree <- test_tree %>%
  mutate(.pred_class = pred_class) %>%
  bind_cols(pred_proba)

# Tính accuracy
accuracy(test_result_tree, truth = target, estimate = .pred_class)




