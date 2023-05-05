# cpsc680_spellchecking
Repository for the project "Racial Bias in Spellchecking" by Florian Caro in Yale's CPSC 680 (Spring 23)

## Steps for Replication

1. Prepare the Python environment using the provided "requirements.txt" file
2. Run the R script "1_generate_synthetic_data.R": This will prepare the synthetic examples for the experiments. You may need to adjust the path to the csv files with the raw data on names.
3. Run the Python script "CPSC680_spellchecking_bias_local.py": This will run the experiments for each spellchecker and save the output in the local directory. You may need to adjust the path to the input files generated in step 2.
4. Run the R script "2_process_model_performance.R": This will generate the results presented in the paper using the output from the experiments generated in step 3. You need to make sure that there is a folder named "Tables" in you local directory. The tables generated in this step will be saved there. You may need to adjust the path to the output files generated in step 3.

## Contribution
All work on this project was performed by Florian Caro.
