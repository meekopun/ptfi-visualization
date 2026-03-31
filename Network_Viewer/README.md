# Food Network Viewer

The **Food Network Viewer** is a standalone HTML application for visualizing food composition data as an interactive orbital network. Compounds orbit around food samples like planets, with node size representing concentration, position creating orbital clusters, and colors indicating chemical classification. Foods are clustered by their type (fruits, vegetables, legumes, etc.) and linked to show relationships.

## Key Concepts

- **Food Type Clustering**: Foods are grouped by their primary food group (e.g., Legumes, Vegetables, Fruits, Grains, Animal Products) and positioned together in circular clusters
- **Food-to-Food Links**: Foods within the same type are connected to show relationships
- **Orbital Layout**: Compounds orbit around food samples at consistent distances, creating a "planetary" visualization
- **Chemical Clustering**: Similar compounds (e.g., amino acids, sugars, fatty acids) cluster together based on chemical classification
- **Size = Concentration**: Node size directly represents the concentration of each compound
- **Color-Coded Families**: Different colors distinguish chemical families (fatty acids, carbohydrates, minerals, amino acids, macronutrients)
- **Multi-Level Clusters**: Both food groups and compound chemical families are visually emphasized with background circles

## Features

- **Paste TSV Data**: Users can paste tab-separated data, including a header row and food sample rows.
- **Customizable Visualization**:
  - Select the number of top compounds per food sample
  - Set a minimum value threshold for compounds
  - Toggle visibility of different chemical families (fatty acids/lipids, carbs/sugars, minerals, amino acids, macronutrients)
- **Interactive Graph**:
  - Drag nodes to reposition them
  - Zoom in/out using the mouse wheel to see compound labels
  - Pan the graph by dragging empty space
  - Click nodes to view detailed metadata including concentration values
  - Hover over nodes to highlight their connections
- **Search Functionality**: Find and focus on specific nodes by name
- **Cluster Visualization**: Subtle background circles show chemical family groupings
- **Offline Usage**: No external libraries required; works locally in any modern browser

## How to Use

1. **Open the File**:
   - Open `food_network.html` in any modern browser (e.g., Chrome, Firefox, Edge).

2. **Paste Data**:
   - Copy and paste your TSV data into the provided text area. Ensure the data includes a header row and at least one data row.

3. **Configure Options**:
   - Adjust the number of top compounds per food sample (default: 10)
   - Set the minimum value threshold (default: 0.1)
   - Use the checkboxes to toggle the visibility of different chemical families

4. **Render the Graph**:
   - Click the **Render network** button to generate the orbital visualization

5. **Interact with the Graph**:
   - Drag nodes to reposition them
   - Use the mouse wheel to zoom in/out (zoom in to see compound labels)
   - Drag empty space to pan the graph
   - Click nodes to view their metadata in the details panel
   - Hover over nodes to highlight their connections and see labels

6. **Search for Nodes**:
   - Use the search bar to find nodes by name and focus on them

7. **Reset View**:
   - Click the **Reset view** button to reset the graph's position and zoom level

8. **Load Example Data**:
   - Click the **Load example rows** button to load sample TSV data for testing

## Visual Design

The visualization uses a hierarchical clustering metaphor where:
- **Food groups** (e.g., Legumes, Vegetables, Fruits) are positioned in a large circular arrangement
- **Food samples** within each group cluster together and are linked to show type relationships
- **Compounds** orbit around their food at a consistent radius (~140px)
- **Chemical families** are distributed evenly around each food's orbit
- **Cluster groups** show related compounds positioned near each other
- **Node sizes** scale logarithmically with concentration values
- **Background circles** highlight both food type clusters (blue) and compound chemical clusters (color-coded)
- **Colors** distinguish chemical families:
  - Blue: Food samples
  - Red: Fatty acids & lipids
  - Green: Sugars & carbohydrates
  - Cyan: Minerals
  - Orange: Amino acids
  - Purple: Macronutrients
- **Link types**:
  - Thicker blue links: Connect foods in the same group
  - Subtle gray links: Connect foods to their compounds

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