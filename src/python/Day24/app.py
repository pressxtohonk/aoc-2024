from dash import Dash, Input, Output, html
import pandas as pd
import dash_cytoscape as cyto

# Load data
nodes = pd.read_csv("nodes.csv")
edges = pd.read_csv("edges.csv")

def node_data(node):
    return {
        'data': {
            'id': node['id'],
            'label': f"{node['id']}: {node['label']}",
        }
    }

def edge_data(edge):
    return {
        'data': {
            'source': edge['source'],
            'target': edge['target'],
        }
    }

# Build dashboard
app = Dash()

app.layout = html.Div([
    html.Button('Rerun Layout', id='layout-button', n_clicks=0),
    cyto.Cytoscape(
        id='cytoscape',
        #layout={'name': 'breadthfirst', 'roots': roots, 'directed': True},
        layout={
            'name': 'cose',
        },
        style={'width': '100%', 'height': '800px'},
        elements=[
            *map(node_data, nodes.to_dict('records')),
            *map(edge_data, edges.to_dict('records')),
        ],
        stylesheet=[
            # Define the arrow style for directed edges
            {
                'selector': 'edge',
                'style': {
                    'width': 2,
                    'line-color': '#ccc',
                    'target-arrow-shape': 'triangle',  # Arrow shape at the target node
                    'target-arrow-color': '#ccc',  # Arrow color
                    'curve-style': 'bezier',
                    'edge-text-visibility': 'hidden'  # Hide edge text (optional)
                }
            },
            {
                'selector': 'node',
                'style': {
                    'label': 'data(label)',  # Display the label property of each node
                    'font-size': 12,  # Adjust font size for the labels
                    'color': '#000',  # Set the font color for labels
                    'text-valign': 'center',  # Vertically center the label
                    'text-halign': 'center',  # Horizontally center the label
                    'border-width': 2,  # Border width for nodes
                }
            },
        ],
        autoRefreshLayout=True,
        boxSelectionEnabled=True,
        responsive=True,
    )
])

# Trigger layout updates on button click
@app.callback(
    Output('cytoscape', 'layout'),
    Input('layout-button', 'n_clicks'),
)
def update_layout(n_clicks):
    return {'name': 'cose', 'n': n_clicks}

# Run network analysis dashboard
if __name__ == '__main__':
    app.run(debug=True)
