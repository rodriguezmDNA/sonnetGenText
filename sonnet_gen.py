#!/usr/bin/env python3
import os
import json
import pandas as pd
import numpy as np
import re
import sys

# Build absolute paths relative to this script's directory.
current_dir = os.path.dirname(os.path.abspath(__file__))
emission_matrix_path = os.path.join(current_dir, 'Python/EmissionMatrix.tsv')
single_word_probs_path = os.path.join(current_dir, 'Python/SingleWordProbs.tsv')
transition_matrix_path = os.path.join(current_dir, 'Python/TransitionMatrix.tsv')
list_of_words_path = os.path.join(current_dir, 'Python/ListOfWords.json')

# Read TSV files using the absolute paths.
EmissionMatrix = pd.read_csv(emission_matrix_path, sep="\t")
SingleWordProbs = pd.read_csv(single_word_probs_path, sep="\t")
TransitionMatrix = pd.read_csv(transition_matrix_path, sep="\t")

with open(list_of_words_path) as f:
    ListOfWords = json.load(f)

def clean_list_of_words(list_of_words):
    """
    Remove trailing .<number> from each word key and aggregate probabilities.
    Then normalize the probabilities for each state.
    """
    cleaned = {}
    for state, words_dict in list_of_words.items():
        new_words = {}
        for word, prob in words_dict.items():
            # Remove only at the end (e.g., "sweet.1" becomes "sweet")
            base_word = re.sub(r'\.\d+$', '', word)
            new_words[base_word] = new_words.get(base_word, 0) + prob
        # Normalize probabilities for this state.
        total = sum(new_words.values())
        for word in new_words:
            new_words[word] /= total
        cleaned[state] = new_words
    return cleaned

# Clean the ListOfWords dictionary.
ListOfWords = clean_list_of_words(ListOfWords)

def make_sonnet(debugging=False):
    write = True
    state = "StateFirstProb"
    mark_quote = []
    
    if debugging:
        print(f"Initial state: {state}")
        print("ListOfWords for initial state:", ListOfWords[state])
    
    # Sample the first word using probabilities from ListOfWords[state].
    words = list(ListOfWords[state].keys())
    probs = np.array(list(ListOfWords[state].values()), dtype=float)
    probs /= probs.sum()
    first_word = np.random.choice(words, p=probs)
    if debugging:
        print(f"First word (raw): {first_word}")
    mark_quote.append(first_word)
    
    # Set the state for subsequent words.
    state = "StateBody"
    if debugging:
        print(f"State changed to: {state}")
    
    while write:
        if debugging:
            print("\nCurrent state:", state)
            print("Current mark_quote:", mark_quote)
        
        # Get the previous word and clean it.
        previous_word = mark_quote[-1]
        cleaned_prev = re.sub(r"\.\d+", "", previous_word)
        if debugging and previous_word != cleaned_prev:
            print(f"Cleaned previous word: '{previous_word}' -> '{cleaned_prev}'")
        previous_word = cleaned_prev
        
        # Get transition probabilities for the previous word.
        if previous_word in TransitionMatrix.index:
            probabilities_series = TransitionMatrix.loc[previous_word]
            if debugging:
                print(f"Transition probabilities for '{previous_word}':")
                print(probabilities_series[probabilities_series != 0])
        else:
            probabilities_series = pd.Series(np.ones(len(TransitionMatrix.columns)), index=TransitionMatrix.columns)
            if debugging:
                print(f"'{previous_word}' not found in TransitionMatrix. Using uniform probabilities.")
        
        # Determine valid words based on the current state's list and nonzero transition probabilities.
        current_state_words = list(ListOfWords[state].keys())
        valid_words = probabilities_series[probabilities_series != 0].index.tolist()
        words_overlap = [w for w in current_state_words if w in valid_words]
        if debugging:
            print("Valid words from TransitionMatrix:", valid_words)
            print("Words in current state:", current_state_words)
            print("Overlap words:", words_overlap)
        
        if words_overlap:
            probs = probabilities_series[words_overlap].values.astype(float)
            if probs.sum() > 0:
                probs /= probs.sum()
            else:
                probs = np.ones(len(words_overlap)) / len(words_overlap)
            next_word = np.random.choice(words_overlap, p=probs)
            cleaned_next = re.sub(r"\.\d+", "", next_word)
            if debugging and next_word != cleaned_next:
                print(f"Cleaned next word: '{next_word}' -> '{cleaned_next}'")
            next_word = cleaned_next
            mark_quote.append(next_word)
        else:
            # Fallback: sample ignoring transition probabilities.
            all_words = list(ListOfWords[state].keys())
            probs_all = np.array(list(ListOfWords[state].values()), dtype=float)
            probs_all /= probs_all.sum()
            next_word = np.random.choice(all_words, p=probs_all)
            if debugging:
                print("Fallback next word (no overlap):", next_word)
            mark_quote.append(next_word)
        
        # Append punctuation if current state indicates sentence end.
        if state == "StateSentenceEnd":
            mark_quote[-1] = mark_quote[-1] + ". \n"
            if debugging:
                print("Appended sentence-ending punctuation.")
        
        # Select new state based on the EmissionMatrix.
        state_names = list(EmissionMatrix.columns)
        state_probs = EmissionMatrix.loc[state].values.astype(float)
        if state_probs.sum() > 0:
            state_probs /= state_probs.sum()
        else:
            state_probs = np.ones(len(state_names)) / len(state_names)
        new_state = np.random.choice(state_names, p=state_probs)
        if debugging:
            print(f"State transition: {state} -> {new_state}")
        state = new_state
        
        # Stop if new state is "STOP".
        if state == "STOP":
            if debugging:
                print("State is STOP. Ending sonnet generation.")
            write = False
            
    return mark_quote

def capitalize_quote(text):
    if not text:
        return text
    return text[0].upper() + text[1:]

def get_sonnet(debugging=False):
    tot = 0
    count = 0
    out_quote = []
    # Generate until total character length is between 100 and 110 or max 100 attempts.
    while (tot < 100 or tot > 110) and count < 100:
        if debugging:
            print(f"\nAttempt {count+1}:")
        out_quote = make_sonnet(debugging)
        tot = sum(len(word) for word in out_quote)
        if debugging:
            print(f"Total characters in sonnet: {tot}")
        count += 1
    if out_quote:
        out_quote[0] = capitalize_quote(out_quote[0])
    mark_quote = " ".join(out_quote)
    return mark_quote

if __name__ == '__main__':
    # Toggle debugging here: set debugging=True for detailed output.
    sonnet = get_sonnet(debugging=False)
    print(sonnet)
