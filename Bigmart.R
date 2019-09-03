# loading packages
  library(data.table) 
  library(dplyr)  
  library(ggplot2) 
  library(corrplot)
  library(xgboost)    
  library(cowplot)
  library(caret)
  library(glmnet)
  library(ranger)
  library(reshape2)
  
# Reading Data
  train <- fread("Train.csv")
  test <- fread("Test.csv")
  submission <- fread("SampleSubmission.csv")
  
# Structure of Data
  str(train)

# Combining Data
  test$Item_Outlet_Sales <- NA
  combine <- rbind(train, test)  
  
# Univariate Analysis
  
  ## Target Variable
     ggplot(train)+
       geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 200, fill = "darkgreen")+
       xlab("Item_Outlet_Sales")
     
  
     ## Numerical Variables
     p1 <- ggplot(combine)+
            geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")

     
     p2 <- ggplot(combine)+
            geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
     
     p3 <- ggplot(combine)+
            geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")
     
     plot_grid(p1, p2, p3, nrow = 1)
     
  ## Categorical Variables
     ggplot(combine %>% group_by(Item_Fat_Content) %>% summarise(count = n()))+
      geom_bar(aes(Item_Fat_Content, count), stat = "identity", fill = "orange")
     
     # "LF", "low fat", "Low Fat" --> "Low Fat"
     # "reg", "Regular" --> "Regular"
        combine$Item_Fat_Content[combine$Item_Fat_Content == "LF"] = "Low Fat"
        combine$Item_Fat_Content[combine$Item_Fat_Content == "low fat"] = "Low Fat"
        combine$Item_Fat_Content[combine$Item_Fat_Content == "reg"] = "Regular"   
        
     p4 <- ggplot(combine %>% group_by(Item_Type) %>% summarise(count = n()))+
            geom_bar(aes(Item_Type, count), stat = "identity", fill = "orange")+
            xlab("")+
            geom_label(aes(Item_Type, count, label = count), vjust = 0.5)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            ggtitle("Item_Type")
     
     p5 <- ggplot(combine %>% group_by(Outlet_Identifier) %>% summarise(count = n()))+
            geom_bar(aes(Outlet_Identifier, count), stat = "identity", fill = "orange")+
            geom_label(aes(Outlet_Identifier, count, label = count), vjust = 0.5)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     p6 <- ggplot(combine %>% group_by(Outlet_Size) %>% summarise(count = n()))+
            geom_bar(aes(Outlet_Size, count), stat = "identity", fill = "orange")+
            geom_label(aes(Outlet_Size, count, label = count), vjust = 0.5)+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     sec_row <- plot_grid(p5, p6, nrow = 1)
     plot_grid(p4, sec_row, ncol = 1)
     
     p7 <- ggplot(combine %>% group_by(Outlet_Establishment_Year) %>% summarise(count = n()))+
            geom_bar(aes(factor(Outlet_Establishment_Year), count), stat = "identity", fill = "orange")+
            geom_label(aes(factor(Outlet_Establishment_Year), count, label = count), vjust = 0.5)+
            theme(axis.text.x = element_text(size = 8.5))
     
     p8 <- ggplot(combine %>% group_by(Outlet_Type) %>% summarise(count = n()))+
            geom_bar(aes(factor(Outlet_Type), count), stat = "identity", fill = "orange")+
            geom_label(aes(factor(Outlet_Type), count, label = count), vjust = 0.5)+
            theme(axis.text.x = element_text(size = 8.5))

     plot_grid(p7, p8, nrow = 1) 
     
# Bivariate Analysis
  train <- combine[1:nrow(train)]
     
     ## Target vs Independent Numerical variables
     p9 <- ggplot(train)+
            geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.5)+
            theme(axis.title = element_text(size = 8.5))
     
     p10 <- ggplot(train)+
             geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.5)+
             theme(axis.title = element_text(size = 8.5))

     p11 <- ggplot(train)+
             geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.5)+
             theme(axis.title = element_text(size = 8.5))

     sec_row2 <- plot_grid(p10, p11, ncol = 2)              
     plot_grid(p9, sec_row2, nrow = 2)    
     
     ## Target vs Independent Categorical variable
     p12 <- ggplot(train)+
             geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta")+
             theme(axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.text = element_text(size = 8),
                   axis.title = element_text(size = 8.5))
     
     p13 <- ggplot(train)+
             geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta")+
             theme(axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.text = element_text(size = 8),
                   axis.title = element_text(size = 8.5))
     
     p14 <- ggplot(train)+
             geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta")+
             theme(axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.text = element_text(size = 8),
                   axis.title = element_text(size = 8.5))

     sec_row3 <- plot_grid(p13, p14, ncol = 2)     
     plot_grid(p12, sec_row3, ncol = 1)     
     
     ggplot(train)+
       geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")
     
     p15 <- ggplot(train)+
             geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")
     
     p16 <- ggplot(train)+
             geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

     plot_grid(p15, p16, ncol = 1)     

# Missing Value Treatment 
  sum(is.na(combine$Item_Weight))           # 2439
     
  miss_index <- which(is.na(combine$Item_Weight))
  for(i in miss_index)
  {
    item = combine$Item_Identifier[i]
    combine$Item_Weight[i] = mean(combine$Item_Weight[combine$Item_Identifier == item], na.rm = TRUE)
  }
     
  miss_index1 <- which(combine$Outlet_Size == "")
  for(i in miss_index1)
  {
    combine$Outlet_Size[i] = "Small"
  }
     
# Replacing 0's in Item_Visibility variable
  p2 <- ggplot(combine)+
         geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
     
  zero_index <- which(combine$Item_Visibility == 0)
  for(i in zero_index)
  {
   item = combine$Item_Identifier[i]
   combine$Item_Visibility[i] = mean(combine$Item_Visibility[combine$Item_Identifier == item], na.rm = TRUE)
  }
     
  ggplot(combine)+
  geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
     
     
# Feature Engineering
     # Item_Type_new
       perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
       non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
       
       combine$Item_Type_new <- ifelse(combine$Item_Type %in% perishable, "perishable",
                                ifelse(combine$Item_Type %in% non_perishable, "non_perishable", "not_sure"))

     # Item_Category
       table(combine$Item_Type, substr(combine$Item_Identifier,1,2))
       
       combine$Item_Category <- substr(combine$Item_Identifier,1,2)
       
       combine$Item_Fat_Content[combine$Item_Category == "NC"] = "Non-Edible"

     # Outlet_Years
       combine$Outlet_years <- 2019 - combine$Outlet_Establishment_Year
       combine$Outlet_Establishment_Year = as.factor(combine$Outlet_Establishment_Year) 

     # Price per unit Weight
       combine$Price_per_unit_wt <- combine$Item_MRP/combine$Item_Weight

     # Item_MRP_Clusters
       combine$Item_MRP_Clusters <- ifelse(combine$Item_MRP < 69, "1st",
                                    ifelse(combine$Item_MRP >= 69 & combine$Item_MRP < 136, "2nd",
                                    ifelse(combine$Item_MRP >= 136 & combine$Item_MRP < 203, "3rd", "4th")))
    
# Encoding Categorical Variables
       ## Ordinal Variables 
          combine$Outlet_size_num <- ifelse(combine$Outlet_Size == "Small",0,
                                     ifelse(combine$Outlet_Size == "Medium",1,2))

          combine$Outlet_Location_Type_num <- ifelse(combine$Outlet_Location_Type == "Tier 3", 0,                                          
                                              ifelse(combine$Outlet_Location_Type == "Tier 2", 1, 2))        
          
          combine$Outlet_Size <- NULL
          combine$Outlet_Location_Type <- NULL  
          
       ## One Hot Encoding
          ohe <- dummyVars("~.", data = combine[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
          ohe_df = data.table(predict(ohe, combine[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
          combine <- cbind(combine[,"Item_Identifier"], ohe_df)

# Removing Skewness
  combine$Item_Visibility <- log(combine$Item_Visibility + 1)
  combine$Price_per_unit_wt <- log(combine$Price_per_unit_wt + 1)   
      
# Scaling Numerical Predictors
  num_vars <- which(sapply(combine, is.numeric))
  num_vars_names <- names(num_vars)
  combine_numeric <- combine[, setdiff(num_vars_names, "Item_Output_Scales"), with = F]      
  prep_num = preProcess(combine_numeric, method=c("center", "scale"))
  combine_numeric_norm = predict(prep_num, combine_numeric)

# Split again
  train <- combine[1:nrow(train)]
  test <- combine[(nrow(train) + 1): nrow(combine)]
  test$Item_Outlet_Sales <- NULL
  
# Correlation
  cor_train = cor(train[,-c("Item_Identifier")]) 
  corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.2)
  

################# MODELS ###################

# Linear Regression
  lin_reg_model <- lm(Item_Outlet_Sales ~ ., data = train[, -c("Item_Identifier")])
  lin_reg_pred <- predict(lin_reg_model, test[, -c("Item_Identifier")])  
  submission$Item_Outlet_Sales <- lin_reg_pred
  write.csv(submission, "Lin_reg.csv", row.names = F)

# Regularized Regression Models
  
  ## Ridge 
     set.seed(2702); my_control = trainControl(method="cv", number=5) 
     grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002)) 
     ridge_lin_reg_model = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], 
                                 y = train$Item_Outlet_Sales, method='glmnet', 
                                 trControl= my_control, 
                                 tuneGrid = grid)
     ridge_lin_reg_pred <- predict(ridge_lin_reg_model, test[, -c("Item_Identifier")])
     submission$Item_Outlet_Sales <- ridge_lin_reg_pred
     write.csv(submission, "Ridge_Lin_reg.csv", row.names = F)
     
     
  
  ## Lasso 
     set.seed(2702); my_control = trainControl(method="cv", number=5) 
     grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002)) 
     lasso_lin_reg_model = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], 
                                 y = train$Item_Outlet_Sales, method='glmnet', 
                                 trControl= my_control, 
                                 tuneGrid = grid)
     lasso_lin_reg_pred <- predict(lasso_lin_reg_model, test[, -c("Item_Identifier")])  
     submission$Item_Outlet_Sales <- lasso_lin_reg_pred
     write.csv(submission, "Lasso_Lin_reg.csv", row.names = F)     
  
  ## Random Forest
     set.seed(2702); my_control <- trainControl(method="cv", number=5)
     tgrid <- expand.grid(.mtry = c(3:10), .splitrule = "variance", .min.node.size = c(10, 15, 20))
     rf_model <- train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
                       y = train$Item_Outlet_Sales,
                       method = "ranger",
                       trControl = my_control,
                       tuneGrid = tgrid,
                       num.trees = 400,
                       importance = "permutation")
     rf_pred <- predict(rf_model, test[, -c("Item_Identifier")])  
     submission$Item_Outlet_Sales <- rf_pred
     write.csv(submission, "Rand_forest.csv", row.names = F)
     
     ### Visualization
         plot(rf_model)
         plot(varImp(rf_model))
         
  ## XGBoost
     param_list = list(objective = "reg:linear", eta = 0.01, gamma = 1, max_depth = 6, subsample = 0.8,
                       colsample_bytree = 0.5)
     dtrain = xgb.DMatrix(data = as.matrix(train[, -c("Item_Identifier", "Item_Outlet_Sales")]),
                          label = train$Item_Outlet_Sales)
     dtest = xgb.DMatrix(data = as.matrix(test[, -c("Item_Identifier")]))
        
     set.seed(2702); xgbcv = xgb.cv(params = param_list, data = dtrain, nrounds = 1000, nfold = 10,
                                    print_every_n = 10, early_stopping_rounds = 30, maximize = F)
     xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 428)
     xgb_pred <- predict(xgb_model, dtest)  
     submission$Item_Outlet_Sales <- xgb_pred
     write.csv(submission, "xgb.csv", row.names = F)
     
     ### Visualization
     var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")),                          
                              model = xgb_model) 
     xgb.plot.importance(var_imp)

# Results
     
  result = data.frame(Algorithm = c("Ridge Regression", "Lasso Regression", "Random Forest","XGBoost"),
                         Validation_Score = c(1134.794, 1129.781, 1095.602, 1089.084),
                         Leaderboard_Score = c(1206.299, 1202.344, 1155.958, 1154.636))
  result$Algorithm <- factor(result$Algorithm, levels = c("Ridge Regression", "Lasso Regression", "Random Forest","XGBoost"))

  result.long<-melt(result)
  ggplot(result.long,aes(Algorithm, value, fill=variable))+
    geom_bar(stat="identity",position="dodge")+
    geom_text(aes(label=value),position = position_dodge2(width = 1, preserve = "single"), 
                 vjust= -0.4, hjust= 0.5,colour = "black")+
       theme(panel.background = element_rect(fill = "light gray",
                                             colour = "black",
                                             size = 2.9, linetype = "solid"))+
       labs(x = "Algorithm", y = "Performance (RMSE)", fill = "Type")
     
     