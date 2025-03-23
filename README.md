# Bone Data Analysis  

## Project Overview  
This project analyzes bone mineral content to determine if there is a significant difference between the dominant and non-dominant sides. Using statistical tests like Hotelling’s two-sample T² test and confidence interval analysis, we assess whether bone mineral strength varies significantly. Additionally, a separate analysis is conducted on triathlon performance data across different age categories.

## Data  
The project utilizes two datasets:  
1. **Bone Data** – Compares bone mineral content between dominant and non-dominant sides.  
2. **Triathlon Data** – Examines performance differences across age categories in three sports: Swimming, Biking, and Running.

## Methodology  
### Bone Data Analysis  
- **Test Used:** Hotelling’s two-sample T² test  
- **Sample Size:** 25  
- **Results:** No significant difference in bone mineral strength between dominant and non-dominant bones.  

### Triathlon Data Analysis  
- **Test Used:** Hotelling’s two-sample T² test and Wilks’ Lambda test  
- **Sample Size:** 20 (Analysis B1), 60 (Analysis B2)  
- **Results:**  
  - Significant differences in performance between age categories.  
  - Swimming and biking performance varies significantly, but running remains consistent across categories.

## Statistical Tools Used  
- **Multivariate Normality Tests**  
  - Royston’s Test  
  - Henze-Zirkler’s Test  
- **Confidence Intervals**  
  - Bonferroni Correction  
- **Multivariate Analysis**  
  - Hotelling’s T² test  
  - Wilks’ Lambda test  
  - MANOVA  

## Visualization  
- Chisquare QQ plots  
- Bivariate scatter plots  
- 3D scatter plots  

## Requirements  
The project requires R and the following libraries:  
```r
install.packages(c("readxl", "MVN", "heplots", "scatterplot3d", "DescTools", "car", "biotools"))
