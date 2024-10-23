# Guide d'utilisation du projet APS

Ce projet permet de compiler, vérifier le type et exécuter des programmes écrits en APS. Suivez les instructions ci-dessous pour installer et utiliser les divers outils et scripts fournis.

## Prérequis

- Make
- SWI-Prolog

## Installation

1. Clonez le dépôt ou téléchargez les sources du projet.
2. Naviguez dans le répertoire du projet.
3. Compilez le projet en utilisant la commande `make` pour générer les exécutables nécessaires.

```bash
 make
```
## Structure du projet

- '/Samples' : Dossier contenant des fichiers de test .aps fournis par Yanis et Salim.
- Scripts exécutables :

    - ./prologTerm : Génère un terme Prolog depuis un fichier APS ciblé.
    - ./typrog : Vérifie le type d’un fichier APS ciblé.
    - ./exeprog : Évalue un fichier APS ciblé.
    - ./test.sh : Vérifie et évalue tous les fichiers du répertoire '/Samples'
    
## Utilisation
### Génération du terme Prolog d'un fichier APS

Pour générer un terme Prolog à partir d'un fichier APS :

```bash
./prologTerm ./Samples/nom_du_fichier.aps
```
### Vérification du typage d'un fichier APS

Pour éxécuter le vérificateur de types sur un fichier APS :
```bash
./typrog ./Samples/nom_du_fichier.aps
```
### Evaluation d'un fichier APS

Pour lancer l'évaluation d'un programme APS :

```bash
./exeprog ./Samples/add.aps
```

### Lancer tous les tests

Pour vérifier le typage et l'évaluation de tous les fichiers dans '/Samples' :
```bash
./test.sh
```# APS
