#Assignment3
#Chanasorn Chirapongsathon 6787015
#Saksit Jittasopee 6787077

setwd("C:/Users/Acer/Desktop/DST - Coding/Statistics/Lab3_material")

library(dplyr)
library(ggplot2)

#Task1 : Answer what is your null hypothesis and alternative hypothesis 
#Null Hypothesis คือ ไม่มีความสัมพันธ์ระหว่างสถานะการสูบบุหรี่และการดื่มชา
#Alternative Hypothesis คือ มีความสัมพันธ์ระหว่างสถานะการสูบบุหรี่และการดื่มชา

#Task 2: Load the Dataset 
dataset <- read.csv("NutIntake.csv") #dataset อ่านไฟล์ NutIntake.csv
head(dataset) #head dataset
str(dataset) #tail dataset

# Check the data
glimpse(dataset)

#Task 3: Make a barplot
dataset_summary <- dataset %>% group_by(smoke, tea) %>% summarise(total.number = n(), .groups = "drop") #จัดกลุ่มตาม smoke, tea และนับจำนวนกลุ่มและรวมตัวเลข

ggplot(dataset_summary, aes(x = smoke, y = total.number, fill = tea)) + #สร้างกราฟแท่ง แกน x คือสถานะการสูบบุหรี่ (smoke) และ y คือจำนวนคน (total.number) แถบคือ tea
  geom_bar(stat = "identity", position = "dodge") + #วาดแถบแท่งแบบจัดเรียงแยก เพื่อเปรียบเทียบจำนวน
  labs(title = "Tea Consumption by Smoking Statistics", x = "Smoking Status", y = "Total Count") + #ตั้งชื่อหัวข้อและแกน x, y
  scale_fill_manual(values = c("red","blue")) #ใส่สีแดงและน้ำเงินในแผนภูมิแท่ง

#Task 4: Conduct Chi-Square Test
Nut.mat <- xtabs(~ smoke + tea, data = dataset) #Nut.mat = สร้างตารางแจกแจงความถี่จาก smoke และ tea
chisq_result <- chisq.test(Nut.mat) #ทำ chi-square เพื่อตรวจสอบว่ามีความสัมพันธ์ระหว่าง smoke และ tea
print(chisq_result) #แสดงผล chi-square

#Task 6: Answer that your Chi-square result is rejected or accept your null hypothesis
#ถ้าค่า p-value ของการทดสอบ chi-square น้อยกว่า 0.05 จะปฏิเสธ Null Hypothesis และกล่าวได้ว่า "มีความสัมพันธ์ระหว่างการสูบบุหรี่และการดื่มชา" 
#แต่ถ้าค่า p-value มากกว่า 0.05 ก็ไม่สามารถปฏิเสธ Null Hypothesis ได้