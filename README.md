# Theme 14: Tableaux de bord avec R shiny


---
output: 
  html_document:
    toc: true
    toc_depth: 2
    number_sections: true
    theme: flatly
    highlight: tango
---

# 📊 Projet Shiny — Conception de Tableau de Bord

## 📚 Contexte

Ce projet a été réalisé dans le cadre de la matière **Projet Statistique avec R**.  
Le thème du projet est : **"Conception d’un tableau de bord statistique avec Shiny"**.

Les objectifs sont :
- Concevoir un **tableau de bord interactif** en R permettant à la fois :
  - l’**analyse spatiale** via la cartographie dynamique,
  - l’**analyse et traitement de bases de données socio-économiques** (comme l’EHCVM),
  - la **génération automatique de graphiques** et **de tableaux statistiques**.
- Structurer l’application en **modules Shiny** pour une meilleure maintenabilité.

## 🛠️ Technologies utilisées

- **R** et **Shiny** pour les applications interactives.
- **shinydashboard** pour la structure du tableau de bord.
- **leaflet**, **ggplot2**, **plotly**, **sf** pour la visualisation de données spatiales.
- **DT**, **gt**, **gtsummary** pour les tableaux dynamiques.
- **readxl**, **haven** pour l'import de bases Excel et Stata.

## 🧩 Détail des principaux fichiers

### `app.R`
Application principale qui structure l’interface et appelle les différents modules.

### `modules/mod_map_page.R`
Module dédié à l'affichage d’une carte interactive avec Leaflet.

### `modules/CartoAnalyse.R`
Module pour la cartographie thématique du Mali avec `ggplot2` et `viridis`.

### `Base/mali_map.rds`
Données spatiales utilisées pour les cartes.

### `www/custom.css`
Fichier CSS personnalisé pour styliser l’interface.

## 🚀 Instructions d'exécution

**Prérequis :**
- R (>= 4.0.0)
- RStudio
- Packages requis :

\`\`\`r
install.packages(c(
  "shiny", "shinydashboard", "leaflet", "plotly", "ggplot2", 
  "viridis", "RColorBrewer", "sf", "DT", "gt", "gtsummary", 
  "readxl", "haven"
))
\`\`\`

**Lancement de l'application :**
1. Ouvrir `app.R` dans RStudio.
2. Lancer avec le bouton **Run App**.

## 📈 Fonctionnalités

- **Cartographie dynamique** avec Leaflet.
- **Carte thématique du Mali** avec sélection de variables.
- **Chargement, traitement et fusion** de données EHCVM (ménage et individu).
- **Graphiques univariés et bivariés interactifs.**
- **Tableaux statistiques** générés automatiquement avec `gtsummary`.

## 👨‍💻 Membres du projet

- *Ange Rayan Emilson RAHERINASOLO*
- *Jean Luc BATABATI*
- *Tamsir NDONG*

## 📄 Licence

Projet académique réalisé dans le cadre pédagogique de l'ENSAE Dakar (2025).
Utilisation libre pour des objectifs éducatifs.
"""
