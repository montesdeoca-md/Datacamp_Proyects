library(tidyverse)
library(randomForest)
library(tidymodels)
library(naniar)
library(class)
library(caret)
library(patchwork)
library(ggprism)
library(skimr)
library(janitor)
library(car)
library(ggpubr)
library(pROC)
library(MLmetrics)
library(gbm)

traffic_site <- read_csv("Datacamp_DS_Recipe_Site_Traffic/recipe_site_traffic_2212.csv")

glimpse(traffic_site)

summary(traffic_site)

#Filtrado de NA values========================================================

before_removal <- traffic_site %>%
  select(calories, carbohydrate, sugar, protein) %>%
  vis_miss() +
  labs(
    title = "Missing values before cleaning \n proces"
  ) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.background = element_rect(color = "grey70", fill = "white", size = 1)
  )
# About 5 percent is missing. 

data <- traffic_site %>% 
  filter(
    !is.na(calories) & 
      !is.na(carbohydrate) & 
      !is.na(sugar) &
      !is.na(protein)
  )

# Confirmation of removal NA rows
after_removal <- data %>%
  select(calories, carbohydrate, sugar, protein) %>%
  vis_miss() +
  labs(
    title = "Confirmation of removal \n missing values"
  ) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.background = element_rect(color = "grey70", fill = "white", size = 1)
  )

before_removal + after_removal + 
  plot_layout(widths = c(2, 2), heights = 4) 

# Data Cleaning y Validation======================================================
cat_before_filter <- traffic_site %>% 
  mutate(
    category = ifelse(category == "Chicken Breast", "Chicken", category), 
  ) %>%
  mutate(
    category = factor(category)
  ) %>%
  group_by(category) %>%
  summarize(
    n = n()
  ) %>%
  arrange(desc(n))

prueba <- data %>% 
  mutate(
    recipe = as.numeric(recipe), 
    calories = as.numeric(calories), 
    carbohydrate = as.numeric(carbohydrate), 
    sugar = as.numeric(sugar), 
    protein = as.numeric(protein), 
    category = ifelse(category == "Chicken Breast", "Chicken", category), 
    servings = str_remove(servings, " as a snack"), 
    high_traffic = ifelse(is.na(high_traffic), as.numeric(0), as.numeric(1))
  ) %>%
  mutate(
    recipe = as.integer(recipe), 
    category = factor(category), 
    servings = as.integer(servings), 
    high_traffic = factor(high_traffic)
  )

cat_after_filter <- prueba %>%
  group_by(category) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

paste0("The number of diferent categories are ", length(levels(prueba$category)))

cat_before_filter %>%
  left_join(cat_after_filter, by = "category", suffix = c("_before", "_after")) %>%
  select(category, n_before, n_after) %>%
  rename("Number of recipes before filtering" = n_before, 
         "Number of recipes after filtering" = n_after) %>%
  pivot_longer(2:3, names_to = "Event", values_to = "Count") %>%
  mutate(
    Event = factor(Event, levels = c("Number of recipes before filtering", "Number of recipes after filtering"))
  ) %>%
  ggplot(aes(x = category, y = Count, fill = Event)) +
  geom_col(position = position_dodge(), color = "black") + 
  labs(
    title = "Comparison of changes filtering missing values", 
    x = "Categories", y = "Frequency"
  ) +
  scale_fill_manual(values = c("#EE6A50", "#79CDCD")) +
  theme_prism() +
  theme(
    axis.text.x = element_text(angle = 90), 
    legend.position = "top"
  )


# Exploratory Data Analysis=====================================================

cols <- c("calories", "carbohydrate", "sugar", "protein")
outlier_counts <- data.frame(Group = character(), negatives = integer(), positivos = integer())

for (col in cols) {
  x <- prueba[[col]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lim_inf <- q1 - 1.5 * iqr
  lim_sup <- q3 + 1.5 * iqr
  negatives <- sum(x < lim_inf)
  positives <- sum(x > lim_sup)
  outlier_counts <- rbind(outlier_counts, data.frame(Group = str_to_title(col), negatives, positives))
}

outlier_counts

prueba %>% 
  select(calories, carbohydrate, sugar, protein) %>% 
  pivot_longer(1:4, names_to = "Group", values_to = "Values") %>% 
  mutate(
    Group = str_to_title(Group)
  ) %>%
  group_by(Group) %>% 
  ggplot(aes(y = Values, fill = Group)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Distribution of Nutritional Features", 
    x = "Group", 
    y = "Values" 
  ) +
  theme_prism() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  facet_wrap(~ Group, nrow = 1, scales = "free")

table <- skim(prueba[2:5])

# Statistical Analysis ============================================================
# Barplot Proportion of High Traffic Sites=====================================

prop <- prueba %>% 
  group_by(high_traffic) %>% 
  summarize(
    frequency = n()
  ) %>% 
  mutate(high_traffic = dplyr::recode(high_traffic, "0" = "Low", "1" = "High")) %>%
  ggplot(aes(x = high_traffic, y = frequency, fill = high_traffic)) +
  geom_col(color = "black", show.legend = FALSE) +
  labs(
    title = "Distribution of Traffic Site Recipes", 
    x = "Traffic Site", 
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("#EE6A50", "#79CDCD"), 
                    labels = c("High", "Low")) +
  theme_prism()

# Pie Chart Proportion of High Traffic Sites
pie <- prueba %>% 
  group_by(high_traffic) %>% 
  summarise(
    Frequency = n()) %>% 
  mutate(prop = Frequency / sum(Frequency)) %>%
  mutate(high_traffic = factor(high_traffic, levels = c("Low" = "0", "High" = "1"), ordered = TRUE)) %>%
  ggplot(aes(x = "", y = prop, fill = high_traffic)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Proportion of Traffic Site Recipes", 
    fill = "High Traffic"
  ) +
  scale_fill_manual(values = c("#EE6A50", "#79CDCD"), 
                    labels = c("Low", "High")) +
  theme_prism() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_blank())

prop | pie

# High Traffic ~ Category (Chi-Squared Test)=====================================

HT_cat <- prueba %>%
  group_by(category, high_traffic) %>%
  summarise(
    frequency = n()
  ) %>%
  mutate(
    category = str_to_title(category), 
    high_traffic = dplyr::recode(high_traffic, "0" = "Low", "1" = "High")
  ) %>%
  ggplot(aes(x = category, y = frequency, fill = high_traffic)) +
  geom_col(position = position_dodge(), color = "black") +
  labs(
    title = "Traffic Site Recipes by Category", 
    x = "Category", 
    y = "Frequency", 
    fill = "High Traffic"
  ) +
  geom_text(
    aes(label = frequency), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 3, 
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#EE6A50", "#79CDCD")) +
  theme_prism() +
  theme(
    axis.text.x = element_text(angle = 90), 
    legend.position = "top"
  )

HT_cat_prop <- prueba %>%
  group_by(category, high_traffic) %>%
  summarise(frequency = n()) %>%
  group_by(category) %>%
  mutate(prop = frequency / sum(frequency)) %>%
  mutate(
    category = str_to_title(category), 
    high_traffic = dplyr::recode(high_traffic, "0" = "Low", "1" = "High")
  ) %>%
  ggplot(aes(x = category, y = prop, fill = high_traffic)) +
  geom_col(color = "black") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, fontface = "bold") +
  labs(
    title = "Proportion of Traffic Site Recipes by Category", 
    x = "Category", 
    y = "Proportion (%)", 
    fill = "High Traffic"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#EE6A50", "#79CDCD")) +
  theme_prism() +
  theme(
    axis.text.x = element_text(angle = 90), 
    legend.position = "top"
  )

HT_cat | HT_cat_prop

prueba %>%
  mutate(
    category = str_to_title(category), 
    high_traffic = dplyr::recode(high_traffic, "0" = "Low", "1" = "High")
  ) %>% 
  tabyl(category, high_traffic) %>%
  chisq.test()

# High Traffic ~ Servings==========================================================================

HT_ser <- prueba %>% 
  group_by(servings, high_traffic) %>% 
  mutate(
    high_traffic = dplyr::recode(high_traffic, "0" = "Low", "1" = "High"), 
    servings = factor(servings)
  ) %>% 
  summarize(
    n = n()
  ) %>%
  ggplot(aes(x = servings, y = n, fill = high_traffic)) +
  geom_col(position = position_dodge(), color = "black") +
  labs(
    title = "High Traffic vs Servings", 
    x = "Servings", 
    y = "Frequency", 
    fill = "High Traffic"
  ) +
  geom_text(
    aes(label = n), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 5, 
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#EE6A50", "#79CDCD")) +
  theme_prism()

HT_ser_prop <-   prueba %>%
  group_by(servings, high_traffic) %>%
  summarise(frequency = n()) %>%
  group_by(servings) %>%
  mutate(prop = frequency / sum(frequency)) %>%
  mutate(
    high_traffic = dplyr::recode(high_traffic, "0" = "Low", "1" = "High"),
    servings = factor(servings)
  ) %>%
  ggplot(aes(x = servings, y = prop, fill = high_traffic)) +
  geom_col(color = "black") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3, fontface = "bold") +
  labs(
    title = "Proportion of Traffic Site Recipes by Servings", 
    x = "Servings", 
    y = "Proportion (%)", 
    fill = "High Traffic"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#EE6A50", "#79CDCD")) +
  theme_prism() +
  theme(
    axis.text.x = element_text(angle = 90), 
    legend.position = "top"
  )

HT_ser | HT_ser_prop + plot_layout(guides = "collect") 

prueba %>%
  mutate(
    high_traffic = dplyr::recode(high_traffic, "0" = "Low", "1" = "High")
  ) %>% 
  tabyl(servings, high_traffic) %>%
  chisq.test()

# Normality Assasment =====================================================

norm <- prueba %>% 
  select(calories, carbohydrate, sugar, protein) %>% 
  pivot_longer(1:4, names_to = "Group", values_to = "Values") %>% 
  mutate(
    Group = str_to_title(Group)
  ) %>%
  group_by(Group) %>% 
  ggplot(aes(sample = Values, color = Group)) +
  stat_qq(show.legend = FALSE) +
  stat_qq_line(show.legend = FALSE) +
  facet_wrap(~ Group, nrow = 1, scales = "free") +
  labs(
    title = "Q-Q Plots for Nutritional Features",
    x = "Theorical Quantiles", 
    y = "Ordered Values"
  ) +
  theme_prism()

hist_p <- prueba %>% 
  select(calories, carbohydrate, sugar, protein) %>% 
  pivot_longer(cols = everything(), names_to = "Group", values_to = "Values") %>% 
  mutate(Group = str_to_title(Group)) %>%
  ggplot(aes(x = Values, fill = Group)) +
  geom_histogram(show.legend = FALSE, color = "black") +
  geom_vline(
    data = . %>% group_by(Group) %>% summarise(mean_val = mean(Values)),
    aes(xintercept = mean_val),
    color = "black", linetype = "dashed", size = 1
  ) +
  geom_text(
    label = "Mean",
    aes(x = -Inf, y = Inf, vjust = 2, hjust = -1),
    color = "black", size = 5
  ) +
  geom_vline(
    data = . %>% group_by(Group) %>% summarise(median_val = median(Values)),
    aes(xintercept = median_val),
    color = "blue", linetype = "dashed", size = 1
  ) +
  geom_text(
    label = "Median",
    aes(x = -Inf, y = Inf, vjust = 3.5, hjust = -0.75),
    color = "blue", size = 5
  ) +
  labs(
    title = "Distribution of Nutritional Features", 
    y = "Frequency"
  ) +
  facet_wrap(~ Group, nrow = 1, scales = "free") +
  theme_prism()

hist_p / norm

# Box-Cox Transformation =====================================================

prueba_long <- prueba %>% 
  select(calories, carbohydrate, sugar, protein) %>% 
  mutate((across(everything(), ~ ifelse(. == 0, 1e-8, .))))%>% 
  pivot_longer(cols = everything(), names_to = "Group", values_to = "Values") %>% 
  mutate(Group = str_to_title(Group))

# Aplicar Box-Cox por grupo
boxcox_trans <- prueba_long %>%
  group_by(Group) %>%
  group_split() %>%
  map_df(function(df) {
    trans <- BoxCoxTrans(df$Values, fudge = 0.02)
    df %>% mutate(Transformed = predict(trans, df$Values), 
                  lambda = trans$lambda)
  })

histBC <- boxcox_trans %>%
  ggplot(aes(x = Transformed, fill = Group)) +
  geom_histogram(show.legend = FALSE, bins = 30, color = "black") +
  geom_vline(
    data = boxcox_trans %>% group_by(Group) %>% summarise(mean_val = mean(Transformed)),
    aes(xintercept = mean_val),
    color = "black", linetype = "dashed", size = 1
  ) +
  geom_vline(
    data = boxcox_trans %>% group_by(Group) %>% summarise(median_val = median(Transformed)),
    aes(xintercept = median_val),
    color = "blue", linetype = "dashed", size = 1
  ) +
  labs(
    title = "Box-Cox Transformed Distributions", 
    y = "Frequency"
  ) +
  facet_wrap(~ Group, nrow = 1, scales = "free") +
  theme_prism()

normBC <- boxcox_trans %>%
  ggplot(aes(sample = Transformed, color = Group)) +
  stat_qq(show.legend = FALSE) +
  stat_qq_line(show.legend = FALSE) +
  facet_wrap(~ Group, nrow = 1, scales = "free") +
  labs(title = "Q-Q Plots After Box-Cox Transformation",
       x ="Theorical Quantiles", 
       y = "Ordered Values") +
  theme_prism()

before_norm <- hist_p / norm
after_norm <- histBC / normBC

before_norm | after_norm  +
  plot_layout(guides = "collect") & 
  theme(legend.position = "top")

# Confirmation of Normalization ================================================
boxcox_trans %>% 
  group_by(Group) %>%
  summarize(
    Shapiro_Wilk_p = shapiro.test(Values)$p.value, 
    Shapiro_Wilk_BC = shapiro.test(Transformed)$p.value, 
    lambda = first(lambda)
  )

# Mann_Whitney U Test ============================================================

prueba %>% 
  select(calories, carbohydrate, sugar, protein, high_traffic) %>%
  pivot_longer(cols = c(calories, carbohydrate, sugar, protein), 
               names_to = "Group", values_to = "Values") %>%
  mutate(
    Group = str_to_title(Group)
  ) %>%
  group_by(Group) %>%
  summarize(
    Median_Low_Traffic = median(Values[high_traffic == 0]), 
    Median_High_Traffic = median(Values[high_traffic == 1]),
    Mann_Whitney_p = wilcox.test(Values ~ high_traffic)$p.value
  )

prueba %>% 
  select(calories, carbohydrate, sugar, protein, high_traffic) %>%
  pivot_longer(cols = c(calories, carbohydrate, sugar, protein), 
               names_to = "Group", values_to = "Values") %>%
  mutate(
    Group = str_to_title(Group), 
    high_traffic = dplyr::recode(high_traffic, "0" = "Low", "1" = "High")
  ) %>%
  ggplot(aes(x = high_traffic, y = Values, fill = high_traffic)) +
  geom_boxplot() +
  labs(
    title = "Nutritional Features by Traffic Site", 
    x = "High Traffic", 
    y = "Values", 
    fill = "High Traffic"
  ) +
  geom_signif(
    comparisons = list(c("Low", "High")),
    test = "wilcox.test",
    map_signif_level = TRUE,
    tip_length = 0.01,
    textsize = 4,
    vjust = 0.5
  ) +
  scale_fill_manual(values = c("#EE6A50", "#79CDCD"), 
                    labels = c("Low", "High")) +
  theme_prism() +
  theme(
    axis.text.x = element_text(face = "bold"), 
    legend.position = "none"
  ) +
  facet_wrap(~ Group, nrow = 1, scales = "free")

# Correlation and Heatmap =====================================================

vars <- prueba %>%
  select(calories, carbohydrate, sugar, protein) %>% 
  rename(Calories = "calories") %>% 
  rename(Carbohydrate = carbohydrate) %>% 
  rename(Sugar = sugar) %>% 
  rename(Protein = protein)

cor_matrix <- cor(vars, use = "complete.obs")

cor_long <- as.data.frame(as.table(cor_matrix))

ggplot(cor_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 3)), size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#B22222", high = "#21618C", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  labs(title = "Correlation of Nutritional Variables (r)",
       x = "", y = "") +
  theme_prism() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none")


# Preparation =====================================================================

# Regresar a formato wide
prueba_trans <- boxcox_trans %>%
  select(Group, Transformed) %>%
  group_by(Group) %>%
  mutate(row_id = row_number()) %>%   # identificador de fila
  ungroup() %>%
  pivot_wider(names_from = Group, values_from = Transformed) %>%
  select(-row_id)

# Combinar con el dataset original (manteniendo otras variables)
prueba_final <- prueba %>%
  bind_cols(prueba_trans) %>% 
  rename(
    calories_BC = Calories, 
    carbohydrate_BC = Carbohydrate, 
    protein_BC = Protein,
    sugar_BC = Sugar
  ) %>% 
  mutate(
    calories_per_serving = calories/servings, 
    carbohydrate_per_serving = carbohydrate/servings,
    protein_per_serving = protein/servings, 
    sugar_per_serving = sugar/servings
  )  

prueba_long_per_serving <- prueba_final %>%
  dplyr::select(calories_per_serving, carbohydrate_per_serving, sugar_per_serving, protein_per_serving) %>%
  mutate(across(everything(), ~ ifelse(. == 0, 1e-8, .))) %>%
  pivot_longer(cols = everything(), names_to = "Group", values_to = "Values") %>%
  mutate(Group = str_c(Group, "_BC")) # opcional, para nombre con mayúscula inicial

boxcox_trans_per_serving <- prueba_long_per_serving %>%
  group_by(Group) %>%
  group_split() %>%
  map_df(function(df) {
    trans <- BoxCoxTrans(df$Values, fudge = 0.01)
    df %>% mutate(Transformed = predict(trans, df$Values),
                  lambda = trans$lambda)
  })

prueba_trans_per_serving_wide <- boxcox_trans_per_serving %>%
  select(Group, Transformed) %>%
  group_by(Group) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = Group, values_from = Transformed) %>%
  select(-row)

prueba_final <- bind_cols(prueba_final, prueba_trans_per_serving_wide)

glimpse(prueba_final)

# Step 1: Calcula el promedio de high_traffic por cada categoría
category_encoding <- prueba_final %>%
  group_by(category) %>%
  summarize(category_te = mean(as.numeric(as.character(high_traffic))), .groups="drop")

# Step 2: Une el encoding al dataframe original
prueba_final <- prueba_final %>%
  left_join(category_encoding, by="category")

glimpse(prueba_final)

# Modeling =============================================================
# Crear columna std
prueba_final <- prueba_final %>%
  mutate(std = apply(select(., calories, carbohydrate, sugar, protein), 1, sd, na.rm = TRUE))

# Target como factor binario (para Logistic y RF)
prueba_final_final <- prueba_final %>%
  mutate(high_traffic = factor(high_traffic, levels = c("0","1"), labels = c("Low","High")))

# Recipe de preprocesamiento
rec <- recipe(high_traffic ~ calories_BC + 
                carbohydrate_BC + sugar_BC + 
                protein_BC + std + protein_per_serving_BC + 
                category_te + sugar_per_serving_BC + carbohydrate_per_serving_BC +
                calories_per_serving_BC + servings,
              data = prueba_final_final) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

# Split train/test
set.seed(10)
trainIndex <- createDataPartition(prueba_final_final$high_traffic, p = 0.8, list = FALSE)
train <- prueba_final_final[trainIndex, ]
test  <- prueba_final_final[-trainIndex, ]

# Preprocesar
prep_rec <- prep(rec, training = train)
train_processed <- bake(prep_rec, new_data = train)
test_processed  <- bake(prep_rec, new_data = test)


# 2. Logistic Regression

log_model <- glm(high_traffic ~ ., data = train_processed, family = binomial)

log_prob <- predict(log_model, newdata = test_processed, type = "response")
log_class <- ifelse(log_prob > 0.60, "High", "Low")

# --- AÑADIDO: Calcular Accuracy para Regresión Logística ---
cm_log <- confusionMatrix(factor(log_class, levels = c("Low","High")), test_processed$high_traffic)
accuracy_log <- cm_log$overall['Accuracy']

roc_log <- roc(test_processed$high_traffic, log_prob)
auc_log <- auc(roc_log)

precision_log <- Precision(test_processed$high_traffic, factor(log_class, levels = c("Low","High")), positive = "High")
recall_log <- Recall(test_processed$high_traffic, factor(log_class, levels = c("Low","High")), positive = "High")
f1_log <- F1_Score(test_processed$high_traffic, factor(log_class, levels = c("Low","High")), positive = "High")


# 3. Random Forest

# Asegurar que no hay NAs para randomForest
train_processed_rf <- na.omit(train_processed)
rf_model <- randomForest(high_traffic ~ ., data = train_processed_rf, ntree = 500, mtry = 3)

rf_prob <- predict(rf_model, newdata = test_processed, type = "prob")[, "High"]
rf_class <- ifelse(rf_prob > 0.6, "High", "Low")

# --- AÑADIDO: Calcular Accuracy para Random Forest ---
cm_rf <- confusionMatrix(factor(rf_class, levels = c("Low", "High")), test_processed$high_traffic)
accuracy_rf <- cm_rf$overall['Accuracy']

roc_rf <- roc(test_processed$high_traffic, rf_prob)
auc_rf <- auc(roc_rf)

precision_rf <- Precision(test_processed$high_traffic, factor(rf_class, levels = c("Low","High")), positive = "High")
recall_rf <- Recall(test_processed$high_traffic, factor(rf_class, levels = c("Low","High")), positive = "High")
f1_rf <- F1_Score(test_processed$high_traffic, factor(rf_class, levels = c("Low","High")), positive = "High")


# 4. Gradient Boosting (GBM)

# Convertir target a 0/1 explícitamente para gbm
train_gbm <- train_processed %>%
  mutate(high_traffic = ifelse(high_traffic == "High", 1, 0))
test_gbm <- test_processed %>%
  mutate(high_traffic = ifelse(high_traffic == "High", 1, 0))

set.seed(10)
gbm_model <- gbm(
  formula = high_traffic ~ .,
  distribution = "bernoulli",
  data = na.omit(train_gbm), # Usar na.omit para asegurar que no haya NAs
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  verbose = FALSE
)

gbm_prob <- predict(gbm_model, newdata = test_gbm, n.trees = 1000, type = "response")
gbm_class <- ifelse(gbm_prob > 0.6, 1, 0)

# --- AÑADIDO: Calcular Accuracy para GBM ---
# Convertir a factores con los mismos niveles para usar confusionMatrix
actual_gbm_factor <- factor(test_gbm$high_traffic, levels = c(0, 1), labels = c("Low", "High"))
pred_gbm_factor <- factor(gbm_class, levels = c(0, 1), labels = c("Low", "High"))
cm_gbm <- confusionMatrix(pred_gbm_factor, actual_gbm_factor)
accuracy_gbm <- cm_gbm$overall['Accuracy']

roc_gbm <- roc(test_gbm$high_traffic, gbm_prob)
auc_gbm <- auc(roc_gbm)

precision_gbm <- Precision(test_gbm$high_traffic, gbm_class, positive = 1)
recall_gbm <- Recall(test_gbm$high_traffic, gbm_class, positive = 1)
f1_gbm <- F1_Score(test_gbm$high_traffic, gbm_class, positive = 1)


# 5. Comparación ROC Curves

plot(roc_log, col = "blue", main = "ROC Curve Logistic vs Random Forest vs GBM")
plot(roc_rf, col = "red", add = TRUE)
plot(roc_gbm, col = "green", add = TRUE)
legend("bottomright", legend = c("Logistic Regression", "Random Forest", "GBM"),
       col = c("blue", "red", "green"), lwd = 2)


# 6. Resultados comparativos

# El dataframe ahora se llenará correctamente
results <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "Gradient Boosting"),
  Accuracy = c(accuracy_log, accuracy_rf, accuracy_gbm),
  Precision = c(precision_log, precision_rf, precision_gbm),
  Recall = c(recall_log, recall_rf, recall_gbm),
  F1 = c(f1_log, f1_rf, f1_gbm),
  AUC = c(auc_log, auc_rf, auc_gbm)
)

print(results)


# 7. AÑADIDO: Gráficos de Importancia de Variables


# --- 7.1 Importancia para Regresión Logística ---
importance_log <- varImp(log_model, scale = FALSE)
importance_log_df <- data.frame(
  Variable = rownames(importance_log),
  Importance = importance_log$Overall
) %>%
  arrange(desc(Importance))

plot_log <- ggplot(importance_log_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Logistic Regression", y = "Importancia (Abs. t-statistic)", x = "") +
  theme_minimal()

# --- 7.2 Importancia para Random Forest ---
importance_rf <- importance(rf_model)
importance_rf_df <- data.frame(
  Variable = rownames(importance_rf),
  Importance = importance_rf[, "MeanDecreaseGini"]
) %>%
  arrange(desc(Importance))

plot_rf <- ggplot(importance_rf_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Random Forest", y = "Importancia (Mean Decrease Gini)", x = "") +
  theme_minimal()

# --- 7.3 Importancia para Gradient Boosting (GBM) ---
importance_gbm <- summary(gbm_model, plotit = FALSE)
importance_gbm_df <- data.frame(
  Variable = importance_gbm$var,
  Importance = importance_gbm$rel.inf
) %>%
  arrange(desc(Importance))

plot_gbm <- ggplot(importance_gbm_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Gradient Boosting", y = "Importancia Relativa", x = "") +
  theme_minimal()

# --- 7.4 Combinar y mostrar los gráficos ---
combined_plot <- plot_log + plot_rf + plot_gbm +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(title = "Comparación de Importancia de Variables por Modelo")

print(combined_plot)