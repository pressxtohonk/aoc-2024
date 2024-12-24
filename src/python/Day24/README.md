# Day 24: part 2

The problem requires identifying misconfigured gates in a 45-bit [ripple carry adder](https://en.wikipedia.org/wiki/Ripple-carry_adder). Due to the small overall circuit size and it's ability to be decomposed into identical [half-adders](https://en.wikipedia.org/wiki/Adder_(electronics)#Half_adder), I decided on doing a manual search for the erroneous gates.

## Environment set up

Dependencies and the notebook environment is managed by nix, except for the dash cytoscape package, which is not available on nixpkgs as of writing.

    nix develop
    pip install dash-cytoscape

## Preparing Graph Data

Run code in `eda.ipynb` to generate node list `nodes.csv` and edge list `edges.csv`.

## Local Network Analysis Dashboard

Network analysis was done via a web app. [Cytoscape](https://cytoscape.org/) was deployed through a [Dash](https://dash.plotly.com/) component.

    python app.py

