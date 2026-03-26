##Metabolomics Dashboard

The **Metabolomics Dashboard** is a static HTML dashboard designed to present an analytical summary of a sample. It provides a clean and interactive format for visualizing proximate composition, carbohydrate-related compounds, mineral micronutrients, and nutritional context.

## Features

### 1. **Overview**
- **Comprehensive Dashboard**: The dashboard provides a visual synthesis of chickpea's nutritional profile, starting from broad composition to detailed nutrient classes.
- **Interactive Figures**: Users can click on figures to enlarge them for better readability.

### 2. **Sections**
The dashboard is divided into the following sections:
- **Overview**: Broad composition and macronutrient structure.
- **Protein and Amino Acids**: Amino-acid pattern and protein character.
- **Carbohydrates**: Structure and distribution of carbohydrate fractions.
- **Fat and Lipid Classes**: Lipid composition and class distribution.
- **Minerals**: Mineral pattern within the broader fingerprint.
- **Whole-Food Signature**: Integrated profile across the full nutrient space.
- **Ranking**: Chickpea's relative position across the main composition measures.
- **Comparison**: Chickpea's position among other legumes.
- **Takeaways**: Summary of the chickpea profile.

### 3. **Figures**
- **Interactive Figures**: Each figure can be clicked to open a larger view.
- **Missing Image Handling**: If an image is missing, a placeholder message is displayed.

### 4. **Lightbox Feature**
- Clicking on any figure opens it in a lightbox for a larger view.
- Includes captions for better context.
---

## How to Use

1. **Open the Dashboard**:
   - Open `index.html` in any modern web browser.

2. **Navigate the Sections**:
   - Use the navigation bar at the top to jump to specific sections (e.g., Protein, Carbohydrates, Fat, etc.).

3. **View Figures**:
   - Click on any figure to enlarge it in a lightbox.
   - Use the "Close" button or press `Esc` to exit the lightbox.

4. **Understand the Data**:
   - Each section includes detailed descriptions and interpretations of the figures.

---

## Required Files

Ensure the following files are present in the `plots/` directory for the dashboard to function correctly:
- `proximate_composition.png`
- `macro_donut.png`
- `fingerprint_heatmap.png`
- `amino_acid_profile.png`
- `carbohydrate_profile.png`
- `lipid_class_summary.png`
- `chickpea_macro_spotlight.png`
- (Other figures as referenced in the `index.html` file)

If any image is missing, a placeholder message will be displayed.

---