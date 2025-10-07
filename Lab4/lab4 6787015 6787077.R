#lab4
#Chanasorn Chirapongsathon 6787015
#Saksit Jittasopee 6787077

#Set working directory
setwd("C:/Users/Acer/Desktop/DST - Coding/Statistics/Lab4")

# Load necessary libraries
library(dplyr) # for plots
library(ggplot2) # for data manipulation
library(car) # for Levene’s test (variance check)

data <- read.csv("my_data.csv") # Ensure file is in your working directory

summary_stats <- data %>% #data
  group_by(Group, Gender) %>% #แบ่งกลุ่ม, เพศ
  summarise(
    Mean = mean(Score_Count), #ค่าเฉลี่ย
    Median = median(Score_Count), #มัธยฐาน
    Variance = var(Score_Count), #แปรปรวน
    SD = sd(Score_Count), #เบี่ยงเบน
    .groups = 'drop'
  )
summary_stats # Print summary stats แสดงผลแบบ summarize

## 2.1 Visual Inspection (Histogram and Q-Q Plot)

# Histogram of all scores แสดง histogram
hist(data$Score_Count, main = "Histogram of Score_Count", col = "skyblue", xlab = "Score")

# Q-Q Plot for one group (e.g., Group A)
qqnorm(subset(data, Group == "Group A")$Score_Count, main = "Q-Q Plot: Group A")
qqline(subset(data, Group == "Group A")$Score_Count, col = "red")

# Q-Q Plot for one group (e.g., Group B)
qqnorm(subset(data, Group == "Group B")$Score_Count, main = "Q-Q Plot: Group B")
qqline(subset(data, Group == "Group B")$Score_Count, col = "blue")

# Q-Q Plot for one group (e.g., Group C)
qqnorm(subset(data, Group == "Group C")$Score_Count, main = "Q-Q Plot: Group C")
qqline(subset(data, Group == "Group C")$Score_Count, col = "green")

## 2.2 Shapiro-Wilk Test (for normality)
# H0: Data are normally distributed, H0 = Normal Distribution
# H1: Data are NOT normally distributed, H1 = Not Normal Distribution

# Run the test for each group เช็คว่าเป็น Normal Distribution หรือไม่
shapiro_A <- shapiro.test(subset(data, Group == "Group A")$Score_Count)
shapiro_B <- shapiro.test(subset(data, Group == "Group B")$Score_Count)
shapiro_C <- shapiro.test(subset(data, Group == "Group C")$Score_Count)
shapiro_A #Normal ไม่ปฏิเสธ H0
shapiro_B #Normal ไม่ปฏิเสธ H0
shapiro_C #Normal ไม่ปฏิเสธ H0
# If p-value > 0.05 → do not reject H0 → assume normality
# If p-value ≤ 0.05 → reject H0 → data not normally distributed
#ถ้ามากกว่า 0.05 (Normal Distribution) จะทำ t-test ได้

#Non-Parametric Test (if not normal) ทำได้ทีละคู่
wilcox_result_ab <- wilcox.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group B")))
wilcox_result_ac <- wilcox.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group C")))
wilcox_result_bc <- wilcox.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group B", "Group C")))
wilcox_result_ab
wilcox_result_ac
wilcox_result_bc

#เช็ค t-test เทียบ score กลุ่มสองกลุ่ม ใน group
t_ab <- t.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group B")))
t_ac <- t.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group C")))
t_bc <- t.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group B", "Group C")))
t_ab #H1
t_ac #H1
t_bc #H1
# Interpretation:
# - H0: Mean(Score_A) = Mean(Score_B)
# - H1: Mean(Score_A) ≠ Mean(Score_B)
# - If p-value < 0.05 → reject H0 → significant difference
# - If CI does not include 0 → supports significant difference

#เทียบแค่กลุ่มเดียวว่ามีผลต่อค่าไหม
t_gender_a <- t.test(Score_Count ~ Gender, data = subset(data, Group == "Group A"))
t_gender_b <- t.test(Score_Count ~ Gender, data = subset(data, Group == "Group B"))
t_gender_c <- t.test(Score_Count ~ Gender, data = subset(data, Group == "Group C"))
t_gender_a #p-value = 0.5397 มากกว่า 0.05 แสดงว่าไม่ reject H0 ไม่มีการกระจายอย่างมีนัยสำคัญ ไม่แตกต่าง (H0 Normal)
t_gender_b #p-value = 0.02772 น้อยกว่า 0.05 แสดงว่า reject H0 มีการกระจายอย่างมีนัยสำคัญ แตกต่าง (H1)
t_gender_c #p-value = 0.2094 มากกว่า 0.05 แสดงว่าไม่ reject H0 ไม่มีการกระจายอย่างมีนัยสำคัญ ไม่แตกต่าง (H0 Normal)
# Interpretation:
# - H0: Mean(Score_A) = Mean(Score_B)
# - H1: Mean(Score_A) ≠ Mean(Score_B)
# - If p-value < 0.05 → reject H0 → significant difference
# - If CI does not include 0 → supports significant difference (Confidential Interval ไม่มี 0 น้อยกว่า 0.05)

# Levene’s Test (recommended) หาค่าแปรปรวนว่าเท่ากันหรือไหม
levene_result_ab <- leveneTest(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group B")))
levene_result_ac <- leveneTest(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group C")))
levene_result_bc <- leveneTest(Score_Count ~ Group, data = subset(data, Group %in% c("Group B", "Group C")))
levene_result_ab
levene_result_ac
levene_result_bc

var_result_ab <- var.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group B")))
var_result_ac <- var.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group C")))
var_result_bc <- var.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group B", "Group C")))
var_result_ab
var_result_ac
var_result_bc

# 📌 Interpretation:
# If p-value > 0.05 → assume equal variances → use var.equal = TRUE
# If p-value ≤ 0.05 → variances are unequal → use default Welch's test
t.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group B")), var.equal = TRUE)
t.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group A", "Group C")), var.equal = TRUE)
t.test(Score_Count ~ Group, data = subset(data, Group %in% c("Group B", "Group C")), var.equal = TRUE)
