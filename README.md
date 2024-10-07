# Wasted food policy impact analysis

This GitHub repository provides the pipeline for data extraction, analysis, and visualization used to conduct a policy impact analysis of state-level wasted food policies in the United States (US).

## Description

The US Food Loss and Waste Reduction Goal seeks to reduce national food waste by 50% by 2030, down to 74 kg/capita (164 lb/capita). However, there is limited knowledge on the effectiveness of policies implemented by state governments to achieve that goal. To address this gap, we propose a method to quantitatively estimate the food waste diversion potential of state policies and project food waste generation levels at the state and federal level.

To do so, we developed a policy scoring matrix across four policy categories and applied it to wasted food solutions listed in the non-profit ReFED’s Insights Engine database.

The pipeline is as follows:
1.	Quantification of total food waste generation (at the federal and state level):
  * _1_fw_total_generation.R_: R script
  * _1_food_surplus_summary_2022.csv_: input file (raw food waste data across the entire supply chain)
  * _1_policy_scores_2022.csv_: input file (population and policy scoring data)
  * _1_total_fw_tons.csv_: output file
2.  Solution extraction and quantification of applicable food waste diversion (at the federal and state level):
  * _2_fw_solution_extraction.R_: R script
  * _2_fw_solutions_baseline_2022.csv_: input file (raw diversion potential data for 9 selected wasted food solutions included in the baseline scenario)
  * _2_fw_solutions_alternative_2022.csv_: input file (raw diversion potential data for 14 selected wasted food solutions included in the alternative scenario)
  * _2_applicable_fw_tons.csv_: output file
3. Quantification of likely food waste diversion
  * _3a_fw_likely_diversion.R_: R script (data in tons)
  * _3b_fw_likely_diversion.R_: R script (data in kg/capita)
  * _3_likely_federal_diversion_kg_per_capita.csv_: output file (federal level)
  * _3_likely_diversion_kg_per_capita.csv_: output file (state level)
4. Data visualization
  * _4a_fw_analysis_visualization.R_: R script (federal level)
  * _4b_fw_analysis_visualization.R_: R script (state level)
  * _4c_fw_analysis_visualization.R_: R script (split by policy category)

## Getting Started

### Dependencies

* This script was developed in R via RStudio v2024.04.2+764 on a MacBook Air 2014 (Big Sur macOS 11).
* The following libraries are required to run the R scripts:
	* tidyverse (data wrangling)
	* dplyr (data wrangling)
	* readr (data wrangling)
	* ggplot2 (data visualization)
	* hrbrthemes (data visualization)
	* ggthemes (data visualization)
	* ggh4x (data visualization)
	* viridis (data visualization)
	* ggrepel (data visualization)
	* cowplot (data visualization)

### Running R Code

* Download R scripts and supporting CSV files and save them in the same folder.
* Run the R scripts (ending in “.R”) in ascending order. 
* Remember to set the folder as working directory:
```
setwd("~/your-folder-name/you-potential-subfolder-name/wasted-policy-project")
```

## Help & Author Information

For any code-related issue, feel free to contact the author (Sarah Kakadellis).

Keen to learn more or access the latest data used in this analysis?

* For food generation and wasted food solutions in the US, visit ReFED's Insights Engine: https://insights.refed.org
* For relevant food policies in the US, visit ReFED's Policy Finder: https://policyfinder.refed.org
* For demographic data in the US, see the US Census Bureau: https://www.census.gov/en.html

Study contributors: Selena Mao (ReFED), Asch Harwood (ReFED), and Ned S. Spang (UC Davis).

## Version History

* 0.2 (October 7, 2024)
    * README and LICENSE files added
    * See [commit change]() or See [release history]()
* 0.1 (July 3, 2024)
    * Initial release

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

Inspiration, code snippets, _etc_.
* [README101](https://www.makeareadme.com)
* [DomPizzie](https://gist.github.com/DomPizzie/7a5ff55ffa9081f2de27c315f5018afc)
