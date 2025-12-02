## Reproducibility Archive 

This archive provides the code to reproduce all results reported in Dablander, F.<sup>&#11089;</sup>, Wimmer, S.<sup>&#11089;</sup>, & Haslbeck, J.M.B.<sup>&#11089;</sup> ([2025](https://osf.io/preprints/socarxiv/mv2q6_v1)). Mapping Climate Change Coverage: Causes, Consequences, and Solutions in German News Media, 2010–2024

### Prompting the AI

The file `Prompting/prompting.py` provides a function `make_prompt`
to create LLM input for a single article,
consisting of a title, content, and synopsis.
If this file is run directly, it will print the prompt template:
```python3 Prompting/prompting.py```

### Analyzing AI Responses

The folder `/Data` includes all files necessary to reproduce all analyses and results shown in the paper. It takes the AI-responses that we stored in an online database as input and outputs all figures and results shown in the paper. The folder contains the following folders and files:

- `/Files` this folder contains files that pass the data in different preprocessing stages from R-file to R-file
- `/Figures` we plot all Figures into this folder; note that not all figures plotted into this folder are shown in the main text / appendix of the paper
- `Helpers_Jonas.R` contains helper files used throughout the R-scripts, predominantly for plotting
- `1_Process.R`  The processing steps before that were obtaining the relevant data from our SQL database, computing a wordcount for each article, and deleting the full text data. We are not able to share the latter openly, but are happy to provide them for non-commercial and research-related projects at request. These preprocessing steps, which are not shared here so we do not share the full text data, lead to the file `RawData_With_WordCount7.0.2-Mistral-Large-.RDS` file containing the a data frame with the URL, newspaper, word count, and all AI-responses for each of the $N = 50,509$ news articles. This is the input to `1_Process.R`, which parses the JSON responses of the AI and binarizes the responses for each of the questions answered with the four answer categories "None", "weakly implied", "strongly implied", and "mentioned". It outputs the file `Df_binarized_rdy_7.0.2-Mistral-Large-.RDS`.
- `2_Filters_and_Subsetting.R` takes the later file as input and applies the four data quality filters and one of three filters on how narrowly articles need to be about climate change. It creates three versions, one with the filter "mainly about climate change", one with "broadly about climate change", and one with neither. For details, see the paper. The file returns an `.RDS` file for each of the three "narrowness filters", for example `Df_binarized_Filtered_mainly_7.0.2-Mistral-Large-.RDS` for the "mainly"-filter, which we use in the analysis in the main text
- `3_Plotting_Meta.R` contains labels for questions, dimensions, and newspapers, and color schemes that are used throughout the figures
- `4_Analysis_Topic_Newspaper.R` takes `Df_binarized_Filtered_mainly_7.0.2-Mistral-Large-.RDS` (or the equivalent file for different filters) as input and creates the main results figure. It also includes the code to get all the (conditional) percentages reported in the paper.
- `5_Analysis_Time.R` takes the same input and does all time-related analyses.
- `6_Analysis_Correlational.R` takes the same input and does all correlational / multivariate related analyses.
- `7_Analysis_Wordcount.R` takes the same input and creates the figure and summary statistics about word count shown in the appendix
- `8_Analysis_Misc.R` takes the same input and performs additional analysis (computing the worst-case standard error for the percentages in Figure 1, and doing the variance decomposition shown in the appendix)
- `9_RegressionModeling.R` contains the code for the analysis that replicates the main results figure after controlling for article length and article type
- `10_Analysis_Validation.R` takes both the AI- and human-consensus responses for the main 28 questions as inputs and creates the results of the validation analysis





