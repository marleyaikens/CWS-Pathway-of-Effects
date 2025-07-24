#%% Arguments
import argparse
parser = argparse.ArgumentParser()
parser.add_argument("vsdxfile")
parser.add_argument("-o", "--outfile", required=False)
args = parser.parse_args()
vsdxFile = args.vsdxfile
#%% Default if output file not specified
if args.outfile is None:
    outfile = vsdxFile.replace(".vsdx", ".json")
    print(f'\nNo destination file given. Writing to "{outfile}"')
else:
    outfile = args.outfile
#%% Toolkit
import polars as pl
from vsdx import VisioFile
import json
def get_shapes(page):
    shapes = list()
    for shape in page.all_shapes:
        shapes.append({"id": shape.ID, "label": shape.text, 
         "x": shape.center_x_y[0], "y": shape.center_x_y[1], 
         "width": shape.width, "height": shape.height, 
         "fillcolor": shape.fill_color, "color": shape.line_color, 
         "linewidth": shape.line_weight})
    return pl.DataFrame(shapes)
def get_nodes(page):
    nodes = get_shapes(page)
    nodes = nodes.filter(
        # filter to shapes only, not connectors
        pl.col("id").is_in(get_connect_ids(page)).not_()
    ).to_dicts(
    )
    return nodes
def get_connect_ids(page):
    connectIds = list(set([i.connector_shape_id for i in page.get_connects()]))
    return connectIds
def get_connects(page):
    connects = list()
    for connect in page.get_connects():
        connects.append({"from_id": connect.from_id, "to_id": connect.to_id,
        "id": connect.connector_shape_id, 
        "shape_id": connect.shape_id})
    return connects
def get_edges(page):
    connects = pl.DataFrame(get_connects(page))
    nodes = pl.DataFrame(get_nodes(page))
    edges = connects.select(
        ["to_id", "id"]
    ).join(
        nodes.select(["id", "y"]), left_on="to_id", right_on="id", 
        how="inner"
    ).sort(
        pl.col("y"), descending=True
    ).group_by("id").agg(
        pl.col("to_id")
    ).with_columns(
        pl.col("to_id").list.to_struct(fields=["from", "to"])
    ).unnest(
        "to_id"
    ).to_dicts(
    )
    return edges
def get_page(page):
    diagram = {
        "name": page.name,
        "nodes": get_nodes(page),
        "edges": get_edges(page)
    }
    return diagram
#%% Cycle through pages and get nodes/edges
visio = VisioFile(vsdxFile)
pages = {page.name: get_page(page) for page in visio.pages}
#%% Write to JSON
with open(outfile, 'w', encoding='utf-8') as f:
    json.dump(pages, f, ensure_ascii=False, indent=4)
# %%
