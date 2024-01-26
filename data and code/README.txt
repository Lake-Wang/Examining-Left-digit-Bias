README file for the data and code files for "An Empirical Bargaining Model with Left-digit Bias: A Study on Auto Loan Monthly Payments"

The materials to replicate the results in the paper include:
1. SYNTHETIC data: Jiang2020_Bunching_synthetic_data.rda. The actual data is protected under NDA, so I have prepared a synthetic data that tries to minic the key features of the actual data. 
One will be able to run the code and get results for all the tables and figures. But the numbers will be different from those in the paper (because the data is synthetic).

2. replication code for all the results in the paper: Jiang2020-MS-replication-code.R

3. estimation code: Jiang2020_Bunching_Estimation.R. This file is referenced by the overall replication code

4. code for counterfactual: Jiang2020_Bunching_CF.R. This file is referenced by the overall replication code

5. Jiang2020_Bunching_functions.R. This file contains functions that are referenced by the overall replication code.


Data key for Jiang2020_Bunching_synthetic_data.rda
	column 1: payment, the monthly payment number
	column 2: min_p, the (calculated) finance manager reservation price
	column 3: max_p, the (calculated) consumer reservation price
	column 4: vantage, the credit score at the time of loan origination
	column 5: highcredit, the loan amount (in $1000)
	column 6: terms, the loan length (in years)
	column 7: black, the percentage of black at a zip-code level
	column 8: hispanic, the percentage of hispanic at a zip-code level
	column 9: int, the loan interest rate

The main code file is the second file: Jiang2020-MS-replication-code.R. One only needs to follow this file, which will call and reference the other three code files.