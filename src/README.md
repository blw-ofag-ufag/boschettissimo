# Über den Code Workflow

In diesem Ordner finden Sie die verschiedenen Skripte, mit denen der identifizierte Workflow umgesetzt werden kann.

1. **r/001_Initialization.R**  
   Legt Code-Bestandteile fest (insbesondere Pfade), die in den verschiedenen Skripten wiederverwendet werden.

2. **r/002_SegmentTrees_ComputeEcolVal_CH.R**  
   Basierend auf eine 1kmx1km Grid über der ganzer Schweiz werden die Bäume Zelle pro Zelle (immer gebuffered) segmentiert und ökologischen Merkmale beschrieben.

3. **r/003_CombineProcessedCells.bat**  
   Kombiniert die Resultate von jeder Zelle in einem eindeutigen geopackage.