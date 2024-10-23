#!/bin/bash

dossier="./Samples"

log_file="test_log.txt"

if [ ! -d "$dossier" ]; then
  echo "Le dossier '$dossier' n'existe pas." | tee -a "$log_file"
  exit 1
fi

for file in "$dossier"/*; do
  if [ -f "$file" ]; then

    echo "Test du fichier : $file" | tee -a "$log_file"
    
    echo "Vérification du typage :" | tee -a "$log_file"
    ./prologTerm "$file" | swipl -s type_checker.pl -g main_stdin 2>&1 | tee -a "$log_file"
    
    echo "Vérification de l'évaluation :" | tee -a "$log_file"
    ./eval "$file" 2>&1 | tee -a "$log_file"
    
    echo "------------------------------" | tee -a "$log_file"
  fi
done
