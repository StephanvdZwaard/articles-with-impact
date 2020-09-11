# Articles with impact

 <hr>
 
 <h2> Publication </h2>
 
  <hr>
  
### Abstract 

Worldwide scientific output is growing faster and faster. Academics should not only publish much and fast, but also publish research with impact. The aim of this study is to use machine learning to investigate characteristics of articles that were published in the Journal of Applied Physiology between 2009 and 2018, and characterize high-impact articles. Article impact was assessed for 4,531 publications by three common impact metrics: the Altmetric Attention Scores, downloads and citations. Additionally, a broad collection of (more than 200) characteristics was collected from the article’s title, abstract, authors, keywords, publication, and article engagement. We constructed random forest (RF) regression models to predict article impact and articles with the highest impact (top-25% and top-10% for each impact metric), which were compared to a naïve baseline method. RF models outperformed the baseline models when predicting the impact of unseen articles (p<0.001 for each impact metric). Also, RF models predicted top-25% and top-10% high-impact articles with a high accuracy. Moreover, RF models revealed important article characteristics. Higher impact was observed for articles about exercise, training, performance and V̇O2max, reviews, human studies, articles from large collaborations, longer articles with many references and high engagement by scientists, practitioners and public or via news outlets and videos. Lower impact was shown for articles about respiratory physiology or sleep apnea, editorials, animal studies, and titles with a question mark or a reference to places or individuals. In summary, research impact can be predicted and better understood using a combination of article characteristics and machine learning.

### Keywords 

Scientometrics, Bibliometrics, Altmetrics, Machine learning, Natural language processing

### Full article 

Can be accessed here: [10.1152/japplphysiol.00489.2020](https://journals.physiology.org/doi/abs/10.1152/japplphysiol.00489.2020)   

 <hr>
 
 <h2> Project documentation </h2>
 
<hr>

### 

The aim of this project is to use machine learning to investigate the characteristics of scientific articles that were published in the [Journal of Applied Physiology](https://journals.physiology.org/journal/jappl) from 2009 to 2018, and characterize articles with the highest impact. Article impact was assessed by the Altmetric Attention Score, the number of downloads and citations, and these three impact metrics were predicted by a broad collection of article characteristics derived from the article’s title, abstract, authors, publication, keywords, and article engagement.

Article impact was assessed for 4,531 publications by three common impact metrics: the [Altmetric Attention Scores](https://www.altmetric.com/about-our-data/), downloads and [citations](https://www.dimensions.ai/).

This project is a collaboration between the [Leiden Institute of Advanced Computer Science](https://www.universiteitleiden.nl/en/science/computer-science) of the Leiden University, the [Department of Human Movement Sciences](https://www.fgb.vu.nl) of the Vrije Universiteit Amsterdam and [Department of Internal Medicine, Endocrinology and Metabolism](https://medicine.uiowa.edu/internalmedicine/) of the University of Iowa, USA.


 <hr>
 
 <h2> Archive </h2>
 
 <hr>
 

<h3> Main script </h3>

The main script is used to run the entire analysis used to obtain the results in this study. This script calls other subfunctions and scripts that are located within the `scripts` subdirectory.

 <hr>
 
 **Description**:    Retrieve article impact from citations, downloads and attention scores and perform a machine-learning analysis to predict article impact based on article characteristics              
 **Authors**:        Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      
 **Date:**         03-08-2020                                                                                                 
 **Version:**      1.0                                                                                                        
 **R.version:**    3.5.1 (2018-07-02)                                                                                         
                                                                                                                          
 **Publication:**  vd Zwaard et al. 2020. Articles with impact: insights into 10 years of research with machine learning      
 doi:          [10.1152/japplphysiol.00489.2020](https://journals.physiology.org/doi/abs/10.1152/japplphysiol.00489.2020)               
 <hr>      
 
<h3> RStudio project </h3>

The RStudio project that is associated with the analysis of this publication.
                                                                                                                          
<h3> data/ </h3>

The `data` subdirectory contains the processed data (after feature engineering), trained `Random forest models` and data related to author affiliation and most frequently mentioned topics.

<h3> scripts/ </h3>

The `scripts` subdirectory contains all the necessary code for performing the analysis. These are related to the data collection, preprocessing, feature engineering, machine learning and models and data visualization, results and statistics.

<h3> results/ </h3>

The `results` subdirectory contains all figures and tables associated with the publication. This folder also contains the two supplemental tables.

<hr>
