# import packages
import numpy as np
import pandas as pd
import re
import spacy
from spacy import displacy
from spacy import tokenizer
nlp = spacy.load('en_core_web_sm')
from transformers import pipeline
import jamspell
from symspellpy import SymSpell
import pkg_resources
from itertools import islice
print("Loading packages done.")

# load transfomer spellchecker
corrector = pipeline("text2text-generation", model="oliverguhr/spelling-correction-english-base") # , device=0
# corrector = pipeline(
#               'text2text-generation',
#               'pszemraj/flan-t5-large-grammar-synthesis',
#               # 'pszemraj/grammar-synthesis-small',
#               )
print("Loading spellchecker done.")

# define replacement functions
num_to_txt = {
    0: "FIRST",
    1: "SECOND",
    2: "THIRD",
    3: "FOURTH",
    4: "FIFTH",
    5: "SIXTH",
    6: "SEVENTH",
    7: "EIGHTH",
    8: "NINTH",
    9: "TENTH"
}


def replace_names(text, placeholder_pattern = "PLACEHOLDER"):
  placeholders = {}
  i = 0
  ents = [(e.text, e.start_char, e.end_char, e.label_) for e in nlp(text).ents]
  if len(ents) > 0:
    for name, start_char, end_char, label in ents:
      # placeholder = str(placeholder_pattern + str(i))
      placeholder = str(num_to_txt[i] + " " + placeholder_pattern)
      text = str(text[:start_char] + placeholder + text[end_char:])
      placeholders[placeholder] = name
      i = i + 1
  return(text, placeholders)


def recover_names(text, placeholders):
  # Inputs:
  # - text: string potentially containing placeholders for names to be restored
  # - placeholders: list of dictionaries with placeholder strings as keys and originl names as values, e.g.
  # [{'PLACEHOLDER0': 'Florian'}]
  import re
  # pattern = re.compile("PLACEHOLDER0000000NN[0-9]+")
  for placeholder, name in placeholders.items():
    text = re.sub(placeholder, name, text)
  return(text)


# load data
synth_data_comb = pd.read_csv("data/model_performance/synth_data_comb_non_DL_results.csv")
print("Loading data done.")
# synth_data_comb = synth_data_comb.loc[synth_data_comb['name_type'] == "firstname"]
synth_data_comb = synth_data_comb.loc[synth_data_comb['experiment'].isin([
   "name_perturbed",
   "name_perturbed_20",
   "sentence_name_perturbed_1",
   "sentence_name_perturbed_2",
   "sentence_name_perturbed_3",
   "sentence_name_perturbed_20_1",
   "sentence_name_perturbed_20_2",
   "sentence_name_perturbed_20_3",
])]
synth_data_comb.reset_index(drop=True, inplace=True)


# SYMSPELL
sym_spell = SymSpell(max_dictionary_edit_distance=2, prefix_length=7)
dictionary_path = pkg_resources.resource_filename(
    "symspellpy", "frequency_dictionary_en_82_765.txt"
)
bigram_path = pkg_resources.resource_filename(
    "symspellpy", "frequency_bigramdictionary_en_243_342.txt"
)
sym_spell.load_dictionary(dictionary_path, term_index=0, count_index=1)
sym_spell.load_bigram_dictionary(bigram_path, term_index=0, count_index=2)
for col in ["perturbed_version", "unperturbed_version"]:
  print(col)
  symspell_output = []
  symspell_output_replacement = []
  for row in range(synth_data_comb.shape[0]):
    if (row % 5000) == 0:
      print(row)
    if synth_data_comb[col][row] == 0:
      symspell_output.append("") 
      symspell_output_replacement.append("")
      continue
    # clean spellcheck
    suggestions = sym_spell.lookup_compound(synth_data_comb[col][row], max_edit_distance=2, transfer_casing=True)
    symspell_output.append(str(suggestions[0].term + ".")) # append best candidate
    # replacement spellcheck
    tmp_input, tmp_replacements = replace_names(synth_data_comb[col][row])
    tmp_output = sym_spell.lookup_compound(tmp_input, max_edit_distance=2, transfer_casing=True)
    tmp_output = recover_names(str(tmp_output[0].term + "."), tmp_replacements)
    symspell_output_replacement.append(tmp_output) # append best candidate
  synth_data_comb[str("symspell_" + col)] = symspell_output
  synth_data_comb[str("symspell_replacement_" + col)] = symspell_output_replacement


# JAMSPELL
corrector_js = jamspell.TSpellCorrector()
corrector_js.LoadLangModel('en.bin')
for col in ["perturbed_version", "unperturbed_version"]:
  print(col)
  jamspell_output = []
  jamspell_output_replacement = []
  for row in range(synth_data_comb.shape[0]):
    if (row % 5000) == 0:
      print(row)
    if synth_data_comb[col][row] == 0:
      jamspell_output.append("") 
      jamspell_output_replacement.append("")
      continue
    # clean spellcheck
    jamspell_output.append(corrector_js.FixFragment(synth_data_comb[col][row]))
    # replacement spellcheck
    tmp_input, tmp_replacements = replace_names(synth_data_comb[col][row])
    tmp_output = corrector_js.FixFragment(tmp_input)
    tmp_output = recover_names(tmp_output, tmp_replacements)
    jamspell_output_replacement.append(tmp_output)
  synth_data_comb[str("jamspell_" + col)] = jamspell_output
  synth_data_comb[str("jamspell_replacement_" + col)] = jamspell_output_replacement


# save non-DL results
synth_data_comb.to_csv("data/model_performance/synth_data_comb_non_DL_results.csv", encoding='utf-8')


# TRANSFORMERS
# perform spellcheck without mitigation mechanism
for col in ["perturbed_version", "unperturbed_version"]:
  print(col)
  x = corrector(synth_data_comb[col].astype(str).values.tolist())
  transformers_output = [a['generated_text'] for a in x]
  synth_data_comb[str("transformer_" + col)] = transformers_output
print("Spellchecking without mitigation mechanism done.")

# save file
synth_data_comb.to_csv("data/model_performance/synth_data_comb_incl_intermediate_DL_results1.csv", sep=',', encoding='utf-8')
print("Intermediate file saved.")


# perform spellcheck with mitigation mechanism
for col in ["perturbed_version", "unperturbed_version"]:
  print(col)
  transformer_output_replacement = []
  for row in range(synth_data_comb.shape[0]):
    if row % 500 == 0:
      print(row)
    if (synth_data_comb[col][row] == 0) or (synth_data_comb[col][row] == "0"):
      transformer_output_replacement.append("")
      continue
    # replacement spellcheck
    tmp_input, tmp_replacements = replace_names(synth_data_comb[col][row])
    tmp_output = corrector(tmp_input)
    tmp_output = recover_names(str(tmp_output[0]['generated_text']), tmp_replacements)
    transformer_output_replacement.append(tmp_output) # append best candidate
  synth_data_comb[str("transformer_replacement_" + col)] = transformer_output_replacement
print("Spellchecking with mitigation mechanism done.")


# save file
synth_data_comb.to_csv("data/model_performance/synth_data_comb_incl_intermediate_DL_results2.csv", sep=',', encoding='utf-8')
print("File saved.")


# save file
synth_data_comb.to_csv("data/model_performance/synth_data_comb_incl_intermediate_DL_results.csv", sep=',', encoding='utf-8')
print("File saved.")