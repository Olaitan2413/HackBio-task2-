# Load libraries
library(ggplot2)     # For data visualization
library(dplyr)       # For data manipulation
library(readr)       # For reading the dataset
library(tidyverse)
install.packages("reshape2")
library(reshape2)
install.packages("pheatmap")
install.packages("ggrepel")
library(ggrepel)

# Load dataset
url <- "https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/WHO_AMR_PRODUCTS_DATA.tsv"
data <- read_tsv(url)
# View data
View(data)

# 1 Display the pathogen to products in stack bar chat
print(data)
ggplot(data, aes(x = `Product name`, y = `Pathogen name`, fill = `R&D phase`)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Phase I" = "lightblue", "Preregistration" = "orange", "Phase II" = "green","Phase III" = "yellow")) +
  theme_minimal() +
  labs(title = "WHO AMR Pipeline Analysis", x = "Product name", y = "Pathogen")

# 2 Drug type Analysis
data_chart <- data.frame(
  Product_Type = c("Antibiotics", "Non-traditional"),
  Count = c(383, 286)
)
data_chart$Percentage <- round((data_chart$Count / sum(data_chart$Count)) * 100, 1)
ggplot(data_chart, aes(x = "", y = Count, fill = Product_Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +  # Removes x and y axes
  labs(title = "Pie Chart of Product Types") +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values = c("Antibiotics" = "blue", "Non-traditional" = "red"))

# 3 The non traditional drug categories analysis
Traditionals_categories <- unique(data$`Non-traditionals categories`)
Traditionals_categories_count <- table(data$`Non-traditionals categories`)
Traditionals_categories_count_df <- as.data.frame(Traditionals_categories_count)
ggplot(Traditionals_categories_count_df, aes(x = Freq, y = Var1)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Frequency of Non-Traditionals categories", x = "Frequency", y = "Traditionals_categories") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 4 Analysis of drug indications
Indications <- unique(data$Indications)
Indications_count <- table(data$Indications)
Indications_count_df <- as.data.frame(Indications_count)
view(Indications_count_df)
colnames(Indications_count_df) <- c("Indications_count_df", "Count")
ggplot(Indications_count_df, aes(x = Indications, y = Count)) +
  geom_segment(aes(x = Indications, xend = Indications, y = 0, yend = Count), color = "yellow") + 
  geom_point(aes(y = Count), size = 2, color = "green") +  
  theme_minimal() +  
  labs(title = "Indications", x = "Indications", y = "Count") +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



# 5 Drugs route of Administration Analysis
 Route_administration_chat  <- data.frame(
    Route_administration = c("Colonoscopy", "Inhalation", "IV", "Enema", "IV & oral", "IV, Oral"),
    Count = c(1, 65, 326, 1, 13, 65))
  Route_administration_chat$Percentage <- round((Route_administration_chat$Count / sum(Route_administration_chat$Count)) * 100, 1)
 ggplot(Route_administration_chat, aes(x = "", y = Count, fill = Route_administration)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    theme_void() +  # Removes x and y axes
    labs(title = "Pie Chart of Route of Administration") +
    geom_text_repel(aes(label = paste0(Percentage, "%")),
                    nudge_x = 1.5,  # Nudging text outside the pie
                    show.legend = FALSE) +
    scale_fill_manual(values = c("Colonoscopy" = "blue",  "Inhalation" = "yellow", "IV" = "violet", "Enema" = "red", "IV & oral" = "purple", "IV, Oral" = "orange"))
  

# 6 Research and Development Analysis
RD_phase <- unique(data$`R&D phase`)
RD_phase_count <- table(data$`R&D phase`)
RD_phase_count_df <- as.data.frame(RD_phase_count)
ggplot(RD_phase_count_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "violet") +
  labs(title = "Frequency of R&D phase", x = "R&D_phase", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 7 Clinical trials analysis
Clinical_trials <- unique(data$`Clinical trials`)
Clinical_trials_count <- table(data$`Clinical trials`)
Clinical_trials_count_df <- as.data.frame(Clinical_trials_count)
view(Clinical_trials_count_df)
colnames(Clinical_trials_count_df) <- c("Clinical_trials_count_df", "Count")
ggplot(Clinical_trials_count_df, aes(x = Clinical_trials, y = Count)) +
  geom_segment(aes(x = Clinical_trials, xend = Clinical_trials, y = 0, yend = Count), color = "yellow") + 
  geom_point(aes(y = Count), size = 2, color = "green") +  
  theme_minimal() + 
  labs(title = "Clinical_trials", x = "Clinical_trials", y = "Count") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# 8 Antibacterial class analysis
Antibacterial_Class_count_df <- as.data.frame(Antibacterial_Class_count)
colnames(Antibacterial_Class_count_df) <- c("Antibacterial_Class", "Count")
ggplot(Antibacterial_Class_count_df, aes(x = Count, y = Antibacterial_Class)) +
  geom_segment(aes(y = Antibacterial_Class, yend = Antibacterial_Class, x = 0, xend = Count), color = "yellow") +  
  geom_point(aes(x = Count), size = 2, color = "green") +  
  theme_minimal() + 
  labs(title = "Lollipop Plot for Antibacterial Classes", y = "Antibacterial Class", x = "Count") +  
  theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 9 ))  


the report of the data is 
