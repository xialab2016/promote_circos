# Installing packages
required_packages <- c("circlize", "RColorBrewer", "htmlwidgets", "htmltools", 
                       "ComplexHeatmap", "d3r", "plotly", "jsonlite")

for(pkg in required_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load libraries
library(circlize)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(ComplexHeatmap)
library(d3r)      # For D3 integration
library(plotly)   # For interactive plots
library(jsonlite) # For JSON handling

# Loading data
load("/corr_final_matrix.RData")
colnames(co_mat) <- c("PDDS", "MSRSR", "PROMIS - physical", "BLCS", "BWCS", "IVIS", "PES",
                      "MFIS-5", "MFIS-21", "PSQI", "SSS", "PSS", "Leisure", "Loneliness", "MSSS",
                      "PROMIS - cognition", "PDQ", "PROMIS - depression", "CESD", "FAMS", "EDSS",
                      "ARMSS", "SDMT", "T25W", "9-HPT")
rownames(co_mat) <- c("PDDS", "MSRSR", "PROMIS - physical", "BLCS", "BWCS", "IVIS", "PES",
                      "MFIS-5", "MFIS-21", "PSQI", "SSS", "PSS", "Leisure", "Loneliness", "MSSS",
                      "PROMIS - cognition", "PDQ", "PROMIS - depression", "CESD", "FAMS", "EDSS",
                      "ARMSS", "SDMT", "T25W", "9-HPT")
overlap_matrix <- co_mat
diag(overlap_matrix) <- 0  # Set diagonal to 0 (no self-overlap)

# Function to generate the interactive circos plot
create_interactive_circos <- function(matrix_data, output_file = "circos_plot.html") {
  # Create color palette for sectors
  sector_colors <- colorRampPalette(brewer.pal(8, "Set3"))(nrow(matrix_data))
  names(sector_colors) <- rownames(matrix_data)
  
  # Create color palette for links (by intensity)
  link_colors <- colorRampPalette(c("#EFEFEF", "#A6CEE3", "#1F78B4", "#08306B"))(100)
  
  # Prepare data in a format suitable for plotly
  plot_data <- list()
  categories <- rownames(matrix_data)
  
  # Create the chord/circos visualization using simpler method
  # First, generate a static plot
  pdf(file = "temp_circos.pdf", width = 12, height = 12)
  
  # Initialize circos plot
  circos.clear()
  circos.par(gap.degree = 3, start.degree = 90)
  
  # Create color mapping function for links
  col_fun <- colorRamp2(seq(0, max(matrix_data), length.out = 100), link_colors)
  
  # Create circos plot
  chordDiagram(
    matrix_data,
    grid.col = sector_colors,
    link.lwd = 1,
    link.lty = 1,
    link.sort = TRUE,
    link.largest.ontop = TRUE,
    annotationTrack = c("grid", "name"),
    preAllocateTracks = list(track.height = 0.05),
    transparency = 0.5,
    link.visible = matrix_data > 0,  # Only show links with values > 0
    col = col_fun,  # Use color mapping function
    annotationTrackHeight = c(0.05, 0.1)
  )
  
  dev.off()
  
  # Now create HTML with interactive tooltips
  # Prepare data in JSON format for the HTML file
  json_data <- toJSON(list(
    matrix = matrix_data,
    categories = categories
  ), auto_unbox = TRUE)
  
  # Create HTML with embedded JavaScript for interactivity
  html_content <- paste0('
  <!DOCTYPE html>
  <html>
  <head>
    <title>Interactive Circos Plot</title>
    <script src="https://d3js.org/d3.v7.min.js"></script>
    <script src="https://d3js.org/d3-chord.v3.min.js"></script>
    <script src="https://d3js.org/d3-scale-chromatic.v1.min.js"></script>
    <style>
      body {
        font-family: Arial, sans-serif;
        margin: 0;
        padding: 20px;
      }
      .tooltip {
        position: absolute;
        background-color: rgba(255, 255, 255, 0.9);
        border: 1px solid #ddd;
        padding: 20px;
        border-radius: 4px;
        pointer-events: none;
        font-size: 14px;
        z-index: 1000;
      }
      #circos-plot {
        width: 1100px;
        height: 900px;
        margin: 0 auto;
        position: relative;
      }
      .controls {
        text-align: center;
        margin: 20px 0;
      }
      h1 {
        text-align: center;
        color: #333;
      }
    </style>
  </head>
  <body>
    <h1>Interactive Circos Plot for Score Overlaps</h1>
    <div class="controls">
      <label for="threshold">Minimum overlap threshold: </label>
      <input type="range" id="threshold" min="0" max="', round(max(matrix_data)), '" value="0" step="1">
      <span id="threshold-value">0</span>
    </div>
    <div id="circos-plot"></div>
    
    <script>
      // Parse the data
      const data = ', json_data, ';
      const matrix = data.matrix;
      const categories = data.categories;
      
      // Set up dimensions
      const width = 700;
      const height = 700;
      const outerRadius = Math.min(width, height) / 2 - 40;
      const innerRadius = outerRadius - 30;
      
      // Color scales
      const categoryColors = d3.scaleOrdinal()
        .domain(categories)
        .range(d3.schemeSet3);
        
      // Create SVG
      const svg = d3.select("#circos-plot")
        .append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform", `translate(${width / 2},${height / 2})`);
      
      // Create tooltip
      const tooltip = d3.select("body")
        .append("div")
        .attr("class", "tooltip")
        .style("opacity", 0);
        
      // Function to update the visualization
      function updateViz(threshold) {
        // Clear previous visualization
        svg.selectAll("*").remove();
        
        // Filter matrix based on threshold
        const filteredMatrix = [];
        for (let i = 0; i < matrix.length; i++) {
          filteredMatrix[i] = [];
          for (let j = 0; j < matrix[i].length; j++) {
            filteredMatrix[i][j] = matrix[i][j] >= threshold ? matrix[i][j] : 0;
          }
        }
        
        // Create chord layout
        const chord = d3.chord()
          .padAngle(0.05)
          .sortSubgroups(d3.descending);
          
        const chords = chord(filteredMatrix);
        
        // Arc generator
        const arc = d3.arc()
          .innerRadius(innerRadius)
          .outerRadius(outerRadius);
          
        // Ribbon generator for chords
        const ribbon = d3.ribbon()
          .radius(innerRadius);
          
        // Add outer arcs
        const group = svg.append("g")
          .selectAll("g")
          .data(chords.groups)
          .join("g");
          
        group.append("path")
          .attr("fill", d => categoryColors(categories[d.index]))
          .attr("d", arc);
          
        // Add category labels
        group.append("text")
          .each(d => { d.angle = (d.startAngle + d.endAngle) / 2; })
          .attr("dy", "0.35em")
          .attr("transform", d => `
            rotate(${(d.angle * 180 / Math.PI - 90)})
            translate(${outerRadius + 10})
            ${d.angle > Math.PI ? "rotate(180)" : ""}
          `)
          .attr("text-anchor", d => d.angle > Math.PI ? "end" : "start")
          .text(d => categories[d.index])
          .style("font-size", "12px");
          
        // Add chords/ribbons
        svg.append("g")
          .attr("fill-opacity", 0.7)
          .selectAll("path")
          .data(chords)
          .join("path")
          .attr("d", ribbon)
          .attr("fill", d => categoryColors(categories[d.source.index]))
          .attr("stroke", d => d3.rgb(categoryColors(categories[d.source.index])).darker())
          .on("mouseover", function(event, d) {
            // Highlight the chord
            d3.select(this)
              .attr("fill-opacity", 1);
              
            // Show tooltip
            tooltip.transition()
              .duration(200)
              .style("opacity", 0.9);
              
            tooltip.html(`
              <strong>${categories[d.source.index]} → ${categories[d.target.index]}</strong><br>
              Overlap count: <strong>${Math.round(d.source.value)}</strong>
            `)
            .style("left", (event.pageX + 10) + "px")
            .style("top", (event.pageY - 28) + "px");
          })
          .on("mouseout", function() {
            // Reset chord opacity
            d3.select(this)
              .attr("fill-opacity", 0.7);
              
            // Hide tooltip
            tooltip.transition()
              .duration(500)
              .style("opacity", 0);
          });
      }
      
      // Initialize with threshold 0
      updateViz(0);
      
      // Update threshold on slider change
      d3.select("#threshold").on("input", function() {
        const threshold = +this.value;
        d3.select("#threshold-value").text(threshold);
        updateViz(threshold);
      });
    </script>
  </body>
  </html>
  ')
  
  # Write HTML file
  writeLines(html_content, output_file)
  
  # Create a basic PDF version as backup
  pdf(file = "temp_circos.pdf", width = 12, height = 12)
  
  # Initialize circos plot
  circos.clear()
  circos.par(gap.degree = 3, start.degree = 90)
  
  # Create circos plot
  # Create color mapping function for links
  col_fun <- colorRamp2(seq(0, max(matrix_data), length.out = 100), link_colors)
  
  # Create circos plot
  chordDiagram(
    matrix_data,
    grid.col = sector_colors,
    link.lwd = 1,
    link.lty = 1,
    link.sort = TRUE,
    link.largest.ontop = TRUE,
    annotationTrack = c("grid", "name"),
    preAllocateTracks = list(track.height = 0.05),
    transparency = 0.5,
    link.visible = matrix_data > 0,  # Only show links with values > 0
    col = col_fun,  # Use color mapping function
    annotationTrackHeight = c(0.05, 0.1)
  )
  
  # Add a legend for the link colors
  lgd_links <- Legend(
    at = round(seq(0, max(matrix_data), length.out = 5)),
    col_fun = col_fun,  # Use the same color mapping function
    title = "Overlap Value"
  )
  
  # Add a legend for the sectors
  lgd_sectors <- Legend(
    labels = categories,
    type = "grid",
    legend_gp = gpar(fill = sector_colors),
    title = "Score Types"
  )
  
  lgd_list <- packLegend(lgd_links, lgd_sectors)
  draw(lgd_list, x = unit(0.8, "npc"), y = unit(0.8, "npc"))
  
  dev.off()
  
  cat("Circos plot created successfully!\n")
}

# If categories variable doesn't exist, create it from row names
if(!exists("categories")) {
  categories <- rownames(overlap_matrix)
}

# Interactive HTML version with improved hovering functionality
create_interactive_circos(overlap_matrix, "score_overlap_circos.html")
