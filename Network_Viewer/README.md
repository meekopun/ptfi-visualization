# Food Network Viewer

The **Food Network Viewer** is a standalone HTML application for visualizing food data as an interactive network graph. It allows users to paste tab-separated values (TSV) data, configure visualization options, and render an offline network graph directly in the browser.

## Features

- **Paste TSV Data**: Users can paste tab-separated data, including a header row and food sample rows.
- **Customizable Visualization**:
  - Select the number of top features per sample.
  - Set a minimum value threshold for features.
  - Toggle visibility of different feature families (e.g., primary group, secondary group, processing, fatty acids, carbohydrates, minerals, amino acids, macros).
- **Interactive Graph**:
  - Drag nodes to reposition them.
  - Zoom in/out using the mouse wheel.
  - Pan the graph by dragging empty space.
  - Click nodes to view detailed metadata.
- **Search Functionality**: Find and focus on specific nodes by name.
- **Offline Usage**: No external libraries are required; the file works locally in any modern browser.

## How to Use

1. **Open the File**:
   - Open `food_network.html` in any modern browser (e.g., Chrome, Firefox, Edge).

2. **Paste Data**:
   - Copy and paste your TSV data into the provided text area. Ensure the data includes a header row and at least one data row.

3. **Configure Options**:
   - Adjust the number of top features per sample and the minimum value threshold.
   - Use the checkboxes to toggle the visibility of different feature families.

4. **Render the Graph**:
   - Click the **Render network** button to generate the graph.

5. **Interact with the Graph**:
   - Drag nodes to reposition them.
   - Use the mouse wheel to zoom in/out.
   - Drag empty space to pan the graph.
   - Click nodes to view their metadata in the details panel.

6. **Search for Nodes**:
   - Use the search bar to find nodes by name and focus on them.

7. **Reset View**:
   - Click the **Reset view** button to reset the graph's position and zoom level.

8. **Load Example Data**:
   - Click the **Load example rows** button to load sample TSV data for testing.

## File Structure

- **HTML**: Contains the structure of the application, including the controls and the graph viewer.
- **CSS**: Inline styles define the layout and appearance of the application.
- **JavaScript**: Implements the logic for parsing TSV data, building the graph, and enabling interactivity.

## Example TSV Data

The application includes example TSV data that can be loaded for testing. The data format should look like this:

```tsv
No.	Sample Name	Globally Unique Sample ID	Food Group Primary	Food Group Secondary	Processing	Lipid	Protein	Carbohydrate	Water
16	artichoke	GGB100178	Vegetables	Vegetable	Whole/Raw	0.45719	3.887043	12.68841955	81.49027
21	black olive (canned)	GGB100197	Fruits	Fruit	Processed	8.037159	0.897806	6.713793	82.04388