phishing-models-analysis
# 🛡️ Phishing Website Detection Using Machine Learning

This project uses machine learning techniques to detect phishing websites based on URL features. The system was built in **RStudio** and includes a simple, user-friendly dashboard developed with **Shiny** to allow real-time predictions without relying on external security tools or web content access.

---

## 📌 Project Overview

Phishing attacks are a growing cybersecurity threat, often tricking users into providing sensitive information through fake websites. Traditional methods like blacklists and heuristics struggle to keep up with new, rapidly changing phishing techniques.

This project proposes a **machine learning-based solution** that predicts whether a URL is phishing or legitimate using structured data extracted from URLs. No third-party tools or full website content is needed—just the URL.

---

## ⚙️ Technologies Used

- **RStudio** – Development environment  
- **Tidyverse / dplyr** – Data cleaning and manipulation  
- **MICE** – Missing value imputation  
- **scale()** – Feature normalization  
- **Machine Learning Models**:
  - Naïve Bayes
  - Decision Tree
  - Random Forest
  - Support Vector Machine (SVM)
  - K-Nearest Neighbors (KNN)
- **Shiny** – Dashboard development for real-time predictions  

---

## 🧪 Machine Learning Approach

- **Dataset**: Includes phishing and legitimate URLs with features like domain length, use of HTTPS, IP address, and special characters.
- **Preprocessing**: 
  - Missing values handled using MICE
  - Outliers removed using IQR
  - Features normalized
- **Training & Testing**: Data split into 80% training and 20% testing using stratification to maintain class balance.
- **Evaluation Metrics**: Accuracy, precision, recall, and F1-score.

---

## 💻 Features

- 🔍 URL input field to test against trained models  
- ✅ Real-time prediction of whether a URL is phishing or legitimate  
- 🔒 Runs locally – No third-party API calls or internet required  
- 📊 Lightweight and easy to use interface built with Shiny  

---

## 📁 File Structure

