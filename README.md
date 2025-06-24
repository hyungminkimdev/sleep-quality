# Bayesian Network Analysis of Sleep Quality

A comprehensive study using Bayesian Networks to model and predict sleep quality based on various lifestyle, demographic, and physiological factors.

📄 **[Research Paper (PDF)](./CS5590_Counter_Fuzzing_Research_Project_Paper.pdf)**  
📊 **[Bayesian Network Graph Screenshot](./images/bn8_graphviz.png)**  
🖼️ **Figures** (see `/images` folder for all evaluation graphs)

## 📋 Project Overview

This project develops a predictive model using Bayesian Networks to analyze the complex relationships between multiple factors that influence sleep quality. By leveraging machine learning techniques and probabilistic modeling, the system provides insights into how various lifestyle choices, demographic factors, and physiological markers affect sleep quality ratings.

### Key Features
- **Multi-dataset Integration**: Combines health statistics with social media usage patterns to create a comprehensive sleep quality model
- **Melatonin Level Inference**: Novel approach to predict melatonin levels based on available lifestyle data
- **Probabilistic Modeling**: Uses Bayesian Networks to capture complex dependencies between variables
- **Evidence-based Predictions**: Incorporates academic research to establish meaningful relationships between factors

## 🎯 Objectives

1. **Predictive Modeling**: Develop a Bayesian Network capable of predicting sleep quality ratings based on input conditions
2. **Factor Analysis**: Identify and quantify the most influential factors affecting sleep quality
3. **Relationship Discovery**: Uncover complex interdependencies between lifestyle, demographic, and physiological variables
4. **Practical Application**: Provide a tool for individuals to understand how their habits might affect their sleep quality

## 📊 Datasets

### Primary Dataset: Health and Sleep Statistics
**Source**: [Kaggle - Health and Sleep Statistics](https://www.kaggle.com/datasets/hanaksoy/health-and-sleep-statistics)

**Variables Include**:
- Demographics: Age, Gender
- Sleep Patterns: Sleep Quality Rating (1-10), Bedtime, Wake-up Time
- Physical Activity: Daily Steps, Calories Burned, Physical Activity Level
- Lifestyle: Dietary Habits, Medication Usage
- Health: Sleep Disorders

### Secondary Dataset: Social Media Usage & Sleep Data
**Source**: [Kaggle - SocialMediaUsage SleepData SG](https://www.kaggle.com/datasets/globalmediadata/socialmediausage-sleepdata-sg)

**Key Variables Used**:
- Physiological Markers: Melatonin Level, Cortisol Level
- Sleep Metrics: Sleep Latency, Total Sleep Time, Sleep Efficiency
- Environmental Factors: Blue Light Exposure, Social Media Usage Patterns
- Psychological Factors: Stress Level Rating

## 🔬 Methodology

### 1. Data Preprocessing
- **Missing Value Handling**: Comprehensive check and treatment of missing data
- **Feature Encoding**: Conversion of categorical and continuous variables using appropriate encoding methods
- **Data Integration**: Merging multiple datasets while maintaining data integrity

### 2. Bayesian Network Construction
- **Structure Learning**: Hill Climbing algorithm for optimal network structure discovery
- **Variable Integration**: Addition of inferred melatonin levels as a key predictor
- **Domain Knowledge Integration**: Application of whitelist/blacklist constraints based on academic research

### 3. Model Optimization
- **Complexity Reduction**: Reduced independent parameters from 337M+ to 2,474 (99.9% reduction)
- **Mutual Information Analysis**: Identification of strongest variable relationships
- **Sensitivity Analysis**: Evaluation of model robustness across different configurations

### 4. Inference Methods
- **Likelihood Weighting**: Monte Carlo sampling for efficient probabilistic inference
- **Conditional Probability Queries**: Real-time prediction capabilities for sleep quality assessment

## 📈 Results

### Model Performance
- **Overall Accuracy**: 81.82% on test dataset
- **Parameter Efficiency**: 52% reduction in model complexity through optimization
- **Strong Correlations Identified**:
  - Bedtime ↔ Wake-up Time (MI: 2.535)
  - Sleep Duration ↔ Age Group (MI: 2.023)
  - Sleep Quality ↔ Melatonin Level (MI: 0.995)

### Key Findings
1. **Melatonin Impact**: Strong correlation between melatonin levels and sleep quality ratings
2. **Lifestyle Factors**: Dietary habits and physical activity significantly influence sleep patterns
3. **Demographic Patterns**: Age-related variations in sleep duration and quality
4. **Interconnected Systems**: Complex web of relationships between physiological and lifestyle factors

<-- Image 1: Initial Bayesian Network Structure -->
<-- Image 2: Final Optimized Network with Melatonin Integration -->
<-- Image 3: Mutual Information Heatmap -->
<-- Image 4: Model Performance Comparison -->

## 🛠️ Technical Implementation

### Technologies Used
- **R Programming**: Primary development environment
- **bnlearn Package**: Bayesian Network structure learning and inference
- **Data Processing**: Advanced preprocessing and feature engineering techniques
- **Visualization**: Network structure and relationship visualization tools

### Key Algorithms
- **Hill Climbing**: Structure learning optimization
- **Hartemink Discretization**: Continuous variable processing
- **Likelihood Weighting**: Probabilistic inference
- **kNN Imputation**: Missing value treatment

## 🎯 Usage Example

```r
# Example: Predict sleep quality for a specific profile
predicted_quality <- cpquery(
  bn_model, 
  event = (Sleep.Quality.Rating == "4"),
  evidence = (
    Age.group == "20-29" &
    Gender == "m" &
    Dietary.Habits == "unhealthy" &
    Bedtime == "01:15" &
    Sleep.Disorders == "yes"
  )
)
# Result: 3.23% probability of Sleep Quality Rating = 4
```

## 📚 Research Foundation

This project incorporates findings from multiple academic sources covering:
- Circadian rhythm regulation and melatonin secretion
- Gender differences in sleep disorders
- Impact of dietary habits and physical activity on sleep quality
- Age-related changes in sleep patterns and hormone levels

## 📖 Full Research Paper

For detailed methodology, mathematical formulations, and comprehensive analysis, please refer to the complete research paper: [Link to project paper - to be added]

## 🚀 Future Enhancements

- **Extended Dataset Integration**: Incorporation of additional physiological markers (adenosine levels, light exposure conditions)
- **Real-time Monitoring**: Development of continuous sleep quality monitoring system
- **Personalized Recommendations**: AI-driven suggestions for sleep quality improvement
- **Mobile Application**: User-friendly interface for personal sleep quality assessment

## 📄 License

This project is available under the MIT License. See LICENSE file for details.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit pull requests, report issues, or suggest improvements.

## 📧 Contact

**Author**: Hyungmin Kim  
**Supervisor**: Jinhee Cho  
**Project Date**: October 2024

---

*This project demonstrates the power of probabilistic modeling in understanding complex health relationships and provides a foundation for evidence-based sleep quality improvement strategies.*
