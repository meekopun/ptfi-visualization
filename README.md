# PTFI Visualization Suite

Interactive data visualization tools for food metabolomics analysis.

## Features

- **Chickpea Atlas Dashboard** (`Dashboard_Viewer/`) - Interactive metabolomics data exploration with plots and comparisons
- **Food Network Viewer** (`Network_Viewer/`) - Relationship visualization between foods based on chemical features
- **UMAP Dashboard** (`UMAP_Viewer/`) - Dimensionality reduction visualization for metabolomics data

## Local Usage

All viewers are standalone HTML files that can be opened directly in a browser:

```bash
# Dashboard
open Dashboard_Viewer/index.html

# Network Viewer
open Network_Viewer/food_network.html

# UMAP Viewer
open UMAP_Viewer/umap_dashboard.html
```

## Web Deployment

To deploy this project on a web server using nginx:

### Quick Start

1. **On your server**, clone this repository:
   ```bash
   sudo git clone <your-repo-url> /var/www/ptfi-visualization
   ```

2. **Run the automated setup script**:
   ```bash
   cd /var/www/ptfi-visualization
   sudo chmod +x setup.sh
   sudo ./setup.sh
   ```

3. **Access your site** at `http://your-domain.com` or `http://your-server-ip`

### Manual Deployment

For detailed manual setup instructions, see [DEPLOYMENT.md](DEPLOYMENT.md).

### Files Included

- `nginx.conf` - Nginx configuration file
- `setup.sh` - Automated deployment script
- `DEPLOYMENT.md` - Comprehensive deployment guide
- `index.html` - Landing page for the visualization suite

## Project Structure

```
ptfi-visualization/
├── Dashboard_Viewer/
│   ├── index.html              # Main dashboard
│   ├── chickpea_metabolomics.tsv
│   ├── plots/                  # Generated plots
│   └── R_scripts/              # R scripts for data processing
├── Network_Viewer/
│   ├── food_network.html       # Network visualization
│   └── sample*.tsv             # Sample data files
├── UMAP_Viewer/
│   └── umap_dashboard.html     # UMAP visualization
├── nginx.conf                  # Nginx configuration
├── setup.sh                    # Deployment script
├── DEPLOYMENT.md               # Deployment guide
└── index.html                  # Landing page
```

## Requirements

- Modern web browser (Chrome, Firefox, Safari, Edge)
- For deployment: Linux server with nginx

## License

See [LICENSE](LICENSE) file for details.