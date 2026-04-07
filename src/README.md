# Über den Code Workflow

In diesem Ordner finden Sie die verschiedenen Skripte, mit denen der identifizierte Workflow umgesetzt werden kann.

1. **001_Initialization.R**  
   Legt Code-Bestandteile fest (insbesondere Pfade), die in den verschiedenen Skripten wiederverwendet werden.

2. **002_DownloadData.R**  
   Lädt die Eingabedatensätze herunter, vorzugsweise über die Spatial Temporal Asset Catalog (STAC) API.

3. **003_StudyAreaData.R**  
   Lädt die definierten Untersuchungsgebiete, für die der Workflow angewendet wird. Ruft _003b_CropStudyAreaData.bat_.

  **003b_CropStudyAreaData.bat**  
   Erstellt für jedes Untersuchungsgebiet eine gepufferte Bounding Box (＋100 m) und schneidet die Eingabedaten entsprechend zu.

