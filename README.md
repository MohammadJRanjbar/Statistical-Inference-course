# Statistical-Inference-course <img src="University_of_Tehran_logo.svg.png" alt="Machine Learning" width="50">


Welcome to the Statistical Inference course repository offered at the University of Tehran. This repository contains code for assignments and projects completed during the course. The course by:

- [Dr. Behnam Bahrak](https://scholar.google.com/citations?user=1IdcoLMAAAAJ&hl=en)

## Course Description


This course provides an overview of fundamental concepts in statistics and probability. It begins with Probability Theory, which delves into the foundations of probability and its significance in statistical inference. Next, Sampling Techniques are explored, with a focus on various sampling methods and their implications in data analysis. Estimation techniques follow, covering how to estimate population parameters using sample data, including point and interval estimates. The concept of Hypothesis Testing is then introduced, detailing procedures for making decisions based on sample data. Regression Analysis comes next, highlighting the use of linear regression models to predict outcomes. Analysis of Variance (ANOVA) is discussed as a means of comparing means across multiple groups. Additionally, Non-parametric Methods are introduced, offering alternatives to traditional statistical techniques for data analysis. Finally, Bayesian Inference is presented as an introduction to Bayesian statistics and its differences from classical (frequentist) approaches. Overall, this summary provides a comprehensive glimpse into key statistical concepts and their applications.

## Table of Contents

Please find below a brief overview of the contents of this repository:
1. `HW1/`: This directory contains code and asnwers for Assignment 1, which focuses on confounding variables, sampling strategies, calculating data statistics, and different methods of data visualization.
2. `HW2/`: This directory contains code and answers for Assignment 2, which focused on conditional probability, binomial probability, geometric random variables, Poisson random variables, and using ggplot2 for data visualization.
3. `HW3/`: This directory contains code and answers for Assignment 3, which focuses on Hypothesis testing, Confidence Intervals, error types I and II, calculating power, and calculating confidence intervals for the Galton dataset.
4. `HW4/`: This directory contains code and answers for Assignment 4, which focuses on ANOVA, t-distribution, comparing two means, Multiple Comparisons, and Bootstrapping (Support Vector Machine).
5. `HW5/`: This directory contains code for Assignment 5, which focuses on feature selection methods, including LDA (Linear Discriminant Analysis), PCA (Principal Component Analysis), Sequential Forward Selection (SFS), and Recursive Feature Elimination(RFE). The assignment includes code for implementing SFS and RFE on the Wine dataset. Additionally, PCA has been implemented for the Faces dataset. In Q7, LDA has been implemented for the Fashion-MNIST dataset.
7. `Final Project/`: Here, you can find code related to the projects completed as part of the course requirements.

## Final Project
This particular project focuses on the classification of Dastgah in Persian music. Dastgah refers to the modal system in Persian music, which organizes melodic types (gūšas) and arranges them around a dominant mode (māya). Each dastgāh derives its name from this dominant mode, which is played during the introductory parts. Our objective in this project is to classify music based on their respective dastgahs. To accomplish this, we follow these key steps:
1. **Data Collection**: We have collected a comprehensive dataset, accessible through this [link](https://docs.google.com/spreadsheets/d/1QmJ2MomwjbD2N-9TZks4IhPzGdarQnYb9HgU-G0T3Cc/edit#gid=0).

2. **Data Preprocessing, Analysis, and Visualization**: To build a robust model, we need to clear our dataset by deleting incorrect files. Also, we need to find features that have the most correlation with our labels and help us in the classification and clustering tasks.

3. **Feature Extraction**: Based on the information acquired in the first part, we need to extract features and prepare our dataset for the next part. We use two sets of features for this project, the first set is as follows:

   - Zero Crossing Rate: The rate at which the signal changes from positive to negative or back.
   - STFT (Short Time Fourier Transforms): The STFT represents a signal in the time-frequency domain by computing discrete Fourier transforms (DFT) over short overlapping windows.
   - Harmonics and Perceptual: Harmonics are characteristics that human ears can't distinguish (representing the sound color). Perceptual understanding of shock waves represents the sound rhythm and emotion.
   - Tempo BMP (beats per minute): Dynamic programming beat tracker.
   - Spectral Centroid: Indicates where the "centre of mass" for a sound is located and is calculated as the weighted mean of the frequencies present in the sound.
   - Spectral Rolloff: It is a measure of the shape of the signal. It represents the frequency below which a specified percentage of the total spectral energy, e.g., 85%, lies.
   - Spectral Bandwidth: Spectral Bandwidth is the spectral range of interest around the centroid, that is, the variance from the spectral centroid.
   - Mel-Frequency Cepstral Coefficients: The Mel frequency cepstral coefficients (MFCCs) of a signal are a small set of features (usually about 10–20) which concisely describe the overall shape of a spectral envelope. It models the characteristics of the human voice.
   - Chroma Frequencies: Chroma features are an interesting and powerful representation for music audio in which the entire spectrum is projected onto 12 bins representing the 12 distinct semitones (or chroma) of the musical octave.

4. **Classification**: In this part, based on the features extracted from the model and their correlation with our classes, we need to design the best model that does the task of classifying. In this project, we try models such as Multilayer Perceptron, SVM, etc.

5. **Clustering**: Clustering music dastgah is a task of grouping music based on the similarities in their audio characteristics.


## Disclaimer

This repository is for archival and reference purposes only. The code here might not be updated or maintained. Use it at your own discretion.
