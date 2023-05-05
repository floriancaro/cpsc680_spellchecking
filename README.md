# cpsc680_spellchecking
Repository for the project "Racial Bias in Spellchecking" by Florian Caro in Yale's CPSC 680 (Spring 23)

## Steps for Replication

The instructions provided here have been tested for a MacBook Pro M1 on Ventura 13.1.

1. Prepare the Python environment to run the experiments by
  1. creating a conda enviroment
  2. running "pip install -r requirements.txt" (make sure that you are the "requirements.txt" file is in your directory)
  3. running "python -m spacy download en"
  4. running "conda install -c conda-forge brotlipy"
2. Run the R script "1_generate_synthetic_data.R": This will prepare the synthetic examples for the experiments. You may need to adjust the path to the csv files with the raw data on names.
3. Run the Python script "CPSC680_spellchecking_bias_local.py": This will run the experiments for each spellchecker and save the output in the local directory. You may need to adjust the path to the input files generated in step 2.
4. Run the R script "2_process_model_performance.R": This will generate the results presented in the paper using the output from the experiments generated in step 3. You need to make sure that there is a folder named "Tables" in you local directory. The tables generated in this step will be saved there. You may need to adjust the path to the output files generated in step 3.

## Contribution
All work on this project was performed by Florian Caro.
