# UMAP Dashboard from TSV

This project is an interactive **UMAP Dashboard** built using HTML, CSS, and JavaScript. It allows users to load, filter, and visualize data from TSV (Tab-Separated Values) files. The dashboard provides a scatter plot visualization of UMAP (Uniform Manifold Approximation and Projection) embeddings, along with filtering and customization options.

---

## Features

### 1. Data Input
- **Upload TSV File**: Users can upload a `.tsv` file containing the dataset.
- **Paste TSV Text**: Users can paste TSV data directly into the provided text area.

### 2. Visualization
- **UMAP Scatter Plot**:
  - Displays UMAP1 and UMAP2 embeddings as a scatter plot.
  - Points are interactive, showing detailed information on hover.
  - Supports zooming and panning for better exploration.
- **Customizable Display**:
  - Color points by a specific column (e.g., `subcategory`).
  - Change point symbols based on a column (e.g., `food`).
  - Adjust point size and opacity.

### 3. Filtering
- **Search**: Filter rows by searching for specific text across multiple columns.
- **Concentration Range**: Set minimum and maximum concentration values.
- **Checkbox Filters**:
  - Filter by `food`, `subcategory`, and `status` using checkboxes.
- **Reset Filters**: Reset all filters to show the full dataset.

### 4. Statistics
- Displays key statistics about the dataset:
  - Total rows.
  - Plottable rows (rows with valid UMAP1 and UMAP2 values).
  - Unique `food` and `subcategory` values.

### 5. Filtered Data Preview
- A table displays the first 250 rows of the filtered dataset.
- Columns include `compound`, `food`, `subcategory`, `status`, `concentration`, `UMAP1`, and `UMAP2`.

---

## File Structure

### HTML
- **Title**: UMAP Dashboard from TSV.
- **Main Sections**:
  1. **Titlebar**: Displays the dashboard title and a brief description.
  2. **Controls Panel**: Contains input fields, filters, and display settings.
  3. **Statistics Panel**: Shows dataset statistics.
  4. **Scatter Plot**: Displays the UMAP visualization using Plotly.
  5. **Filtered Data Table**: Shows a preview of the filtered dataset.

### CSS
- **Styling**:
  - Uses CSS variables for consistent theming (e.g., `--bg`, `--text`, `--accent`).
  - Responsive design for different screen sizes.
  - Custom styles for tables, buttons, and checkboxes.

### JavaScript
- **Libraries**:
  - [Plotly.js](https://plotly.com/javascript/): Used for rendering the UMAP scatter plot.
- **Core Functions**:
  - `parseTSV`: Parses TSV data into rows and columns.
  - `applyFilters`: Filters the dataset based on user input.
  - `renderPlot`: Generates the UMAP scatter plot.
  - `renderTable`: Displays the filtered data in a table.

---

## How to Use

### Load Data
1. Upload a `.tsv` file or paste TSV data into the text area.
2. Click the **Load Data** button to parse and display the data.

### Customize Display
1. Use the dropdowns to change the color, symbol, and size of points.
2. Adjust point size, opacity, and scaling.

### Filter Data
1. Use the search bar to filter rows by text.
2. Set minimum and maximum concentration values.
3. Use checkboxes to filter by `food`, `subcategory`, or `status`.

### Explore the Plot
1. Hover over points to see detailed information.
2. Zoom and pan to explore specific areas of the plot.

### Preview Filtered Data
- View the first 250 rows of the filtered dataset in the table.

---

## Required Columns

The TSV file must include the following columns:

| Column         | Description                                   |
|----------------|-----------------------------------------------|
| `compound`     | Name of the compound.                        |
| `structure`    | SMILES or other structural representation.   |
| `subcategory`  | Subcategory of the compound.                 |
| `status`       | Status of the compound (e.g., active).       |
| `food`         | Food source of the compound.                 |
| `concentration`| Concentration value of the compound.         |
| `UMAP1`        | UMAP embedding dimension 1.                  |
| `UMAP2`        | UMAP embedding dimension 2.                  |

Rows missing `UMAP1` or `UMAP2` values will be excluded from the scatter plot but retained in the dataset.

---

## Example TSV Format

```tsv
compound    structure    subcategory    status    food    concentration    UMAP1    UMAP2
Compound1   CCO          Category1      Active    Food1   0.5              1.23     4.56
Compound2   CCN          Category2      Inactive  Food2   1.2              2.34     5.67