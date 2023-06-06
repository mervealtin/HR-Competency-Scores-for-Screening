#-----------------------------------------------Kütüphaneler-----------------------------------------

library(naniar)
library(ggplot2)
library(gmodels)
library(pROC)
library(caret)
library(readxl)
library(reshape2)
library(corrplot)
install.packages("ROCR")
library(ROCR)

#-------------------------------------------Veri setini yükleme--------------------------------------

hr_data<-read_excel(file.choose())
#-------------------------------------------Veri setininin incelenmesi--------------------------------------

head(hr_data)
str(hr_data)
class(hr_data)

hr_data$call_for_interview <- as.factor(hr_data$call_for_interview)
hr_data$years_factor <- as.factor(hr_data$years_factor)
hr_data$functional_factor <- as.factor(hr_data$functional_factor)
hr_data$behavior_factor <- as.factor(hr_data$behavior_factor)
str(hr_data)

gg_miss_var(hr_data) #N/A değerlerinin varlığı


#----------------------------------------Ön Analizler----------------------------------

# hr_data'dan ilgili değişkenler
num_data <- hr_data[, c("years_of_experience","functional_competency_score", 
                        "top1_skills_score", "top2_skills_score", "top3_skills_score", 
                        "behavior_competency_score","top1_behavior_skill_score", 
                        "top2_behavior_skill_score", "top3_behavior_skill_score")]

# değişkenler arasındaki ilişki için korelasyon matrisi:

cor_matrix <- cor(num_data)
print(cor_matrix)

## matrisimizi heatmap ile görselleştirelim:

cor_matrix_melted <- melt(cor_matrix)

heatmap_plot <- ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Correlation Heatmap")
# x etiketlerini yan yazdırın
heatmap_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#--------------------------------------Değişkenlerin Görselleştirmeleri--------------------------------

### histogram yardımı ile değişkenlermizi görselleştirelim:
for (var in c("functional_competency_score", "top1_skills_score", "top2_skills_score", "top3_skills_score", "behavior_competency_score", "top1_behavior_skill_score", "top2_behavior_skill_score", "top3_behavior_skill_score")) {
  p <- ggplot(num_data, aes_string(y = var, fill = var)) +
    geom_boxplot() +
    labs(title = paste("Box Plot of", var), x = "Variable", y = var)
  
  print(p)
}

  
ggplot(hr_data, aes(x = call_for_interview, fill = call_for_interview)) +
  geom_bar() +
  xlab("call_for_interview") +
  ylab("Count") +
  ggtitle("Call for Interview Count") +
  scale_fill_discrete(name = "call_for_interview")
  
for (var in c("years_factor","functional_factor", "behavior_factor" )) {
 a <- ggplot(hr_data,  aes_string(y = var, fill = var)) +
  geom_bar() +
  labs(title = paste("Box Plot of", var), x = "Variable", y = var) +
  scale_fill_discrete(name = var) +
   coord_flip()
 print(a)
}
table(hr_data$call_for_interview)


#------------------------------------------bağısızlık analizi-------------------------------------------------------------
###Ki-kare yöntemi#####

# Years of experince ile Call interview olumsallık tablosu
table_year <- table(hr_data$years_factor, hr_data$call_for_interview)
rownames(table_year) <- c("Experience_year_0", "Experience_year_1" )
colnames(table_year) <- c("Call_Interview_0", "Call_Interview_1" )
print(table_year)

# Function comp. score ile Call interview olumsallık tablosu
table_funcscore <- table(hr_data$functional_factor, hr_data$call_for_interview)
rownames(table_funcscore) <- c("Func_score_0", "Func_score_1" )
colnames(table_funcscore) <- c("Call_Interview_0", "Call_Interview_1" )
print(table_funcscore)


# Behavior comp. score ile Call interview olumsallık tablosu
table_behaviorscore <- table(hr_data$behavior_factor, hr_data$call_for_interview)
rownames(table_behaviorscore) <- c("Behavior_score_0", "Behavior_score_1" )
colnames(table_behaviorscore) <- c("Call_Interview_0", "Call_Interview_1" )
print(table_behaviorscore)

chi_square_test_year <- chisq.test(table_year)
print(chi_square_test_year)

chi_square_test_behav <- chisq.test(table_behaviorscore)
print(chi_square_test_behav)

chi_square_test_funct <- chisq.test(table_funcscore)
print(chi_square_test_funct)

# P-değerine baktığımızda değerimizin çok küçük olduğunu görürüz. Bu durum,
#değişkenleri arasında bir ilişki olduğunu gösterir.

#-------------------------------------------Train Test olarak Ayırma-----------------------------------


# Ayrıştırmayı tekrarlanabilirlik esasına dayalı yaptıgımız için seed ekliyoruz.

set.seed(123)

## Veri setini %70 e %30 olarak ayırıyoruz
# Eğitim indeksimiz için bir değişken atayalım, sonrasında bu değişken train-test aryımı için kullanılacaktır.

train_idx <- createDataPartition(hr_data$call_for_interview, p = 0.7, list = FALSE)

# Veri setimizin, Train ve Test olarak ayıralım

train_data <- hr_data[train_idx, ]
test_data <- hr_data[-train_idx, ]

#-------------------------------------------Lojistik Regresyon Modeli-----------------------------------

### Lojistik Regresyon Modelini:
## Train-test modellerimizi oluşturalım:
Train_model <- glm(call_for_interview ~ years_of_experience+ functional_competency_score+ 
                     top1_skills_score + top2_skills_score + top3_skills_score +
                     behavior_competency_score+ top1_behavior_skill_score +
                     top2_behavior_skill_score + top3_behavior_skill_score,
                     data = train_data, family = binomial)

Test_model <- glm(call_for_interview ~ years_of_experience+ functional_competency_score+ 
                    top1_skills_score + top2_skills_score + top3_skills_score +
                    behavior_competency_score + top1_behavior_skill_score + 
                    top2_behavior_skill_score + top3_behavior_skill_score,
                    data = test_data, family = binomial)

summary(Train_model)

### Model Katsayılarını görselleştirelim:

Train_model_coefficients <- coef(Train_model)
print(Train_model_coefficients)

### OR Değerleri:
odds_ratios <- exp(Train_model_coefficients)

# Model Katsayıları ve OR Değerlerini aynı tabloda gösterelim:

results <- data.frame(Coefficient = Train_model_coefficients, Odds_Ratio = odds_ratios)
results

#---------------------------------------------ROC analizi yapma -----------------------------------------

###ROC analizi:

## Modelin tahminlerini alalalım:
predictions <- predict(Train_model, type = "response")
predy = ifelse(predictions > 0.5, 1, 0)


## ROC eğrisini çizelim:
roc <- roc(train_data$call_for_interview, predictions)


ROCRpred <- ROCR::prediction(predictions, train_data$call_for_interview)
ROCRperf <- ROCR::performance(ROCRpred, "tpr", "fpr")

# Çizdirdiğimiz ROC eğrisini görselleştirelim:
# Daha küçük grafik kenar boşluklarını ayarla
par(mar = c(5, 4, 4, 2) + 0.1)

# Grafiği oluştur
plot(ROCRperf, colorize = TRUE)

# Diyagonal referans çizgisini ekle
abline(0, 1, lty = 5)

## ROC eğrisinin preformans değerini ölçelim:

auc_ROCR <- ROCR::performance(ROCRpred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR


## tahminleme tablosuna bakalım:

predy=ifelse(predictions> 0.20,1,0)
S <- table(train_data$call_for_interview, predy)
rownames(S) <- c("Call_Interview_0", "Call_Interview_1" )
colnames(S) <- c("Call_Interview_0", "Call_Interview_1" )
print(S)

#---------------------------------------Kesim noktası elde etme---------------------------------------

### Kesim noktasını elde edelim: 

cutoff <- coords(roc, "best")

## Kesim noktasını yazdıralım:
print(cutoff)

#--------------------------------Bir deney birimi için olasılık tahmini----------------------------


# deney birimini herhangi bir satır olarak seçebiliriz:

deneybirimi2 <- hr_data["115", ]  # 115. deney birimini seçtik 


# Tahmin yapalım;

predictionD <- predict(Train_model, newdata = deneybirimi2, type = "response")

# Elde ettiğimiz olasılık tahminini yazdıralım:

print(predictionD)

# Elde ettiğimiz çıktıya göre, belirttiğimiz deney birimi için model tarafından tahmin edilen 
#olasılık 0.998'dir. Bu olasılık, deney biriminin olumlu bir sonuçla sınıflandırılma olasılığını
#temsil etmektedir.
#Tahmin edilen olasılık oldukça yüksektir, 
#yani deney biriminin olumlu bir sonuçla sınıflandırılma olasılığı yüksektir



