from __future__ import annotations

from pathlib import Path
from typing import Iterable

import geopandas as gpd
import matplotlib as mpl
import matplotlib.pyplot as plt
import osmium
from matplotlib.lines import Line2D
from matplotlib.patches import Patch, Rectangle
from pyproj import Transformer
from shapely.geometry import LineString, Polygon, box


ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "data"
BOUNDARIES = DATA / "boundaries"
DERIVED = DATA / "derived"
OUT = ROOT / "figures" / "publication_nature_revised"
DERIVED.mkdir(parents=True, exist_ok=True)
OUT.mkdir(parents=True, exist_ok=True)

CASE_CODES = [
    "E01000095",
    "E01000094",
    "E01034478",
    "E01000096",
    "E01000090",
    "E01034476",
    "E01000014",
]
COMPARISON_CODES = ["E01003566", "E01003525", "E01003531", "E01003607"]

PBF = DATA / "raw" / "osm" / "greater-london-latest.osm.pbf"
LSOA = BOUNDARIES / "london_lsoa_2021.gpkg"
THAMES = BOUNDARIES / "river_thames_osm.geojson"

BUILDING_CACHE = DERIVED / "figure12_panel_c_osm_buildings.gpkg"
ROAD_CACHE = DERIVED / "figure12_panel_c_osm_roads_python.gpkg"

PAL = {
    "figure_bg": "#FFFFFF",
    "panel_bg": "#F7F7F7",
    "london": "#F1F1F1",
    "admin": "#D0D0D0",
    "roads": "#DCDCDC",
    "main_roads": "#D0D0D0",
    "thames": "#CFE8F3",
    "buildings": "#174A73",
    "case": "#B2182B",
    "case_fill": "#F4A3A3",
    "comparison": "#4C78A8",
    "comparison_fill": "#A6BDD7",
    "border": "#222222",
    "text": "#222222",
    "connectors": "#555555",
}


mpl.rcParams.update(
    {
        "font.family": "serif",
        "font.serif": ["Times New Roman", "DejaVu Serif", "serif"],
        "font.size": 8,
        "pdf.fonttype": 42,
        "svg.fonttype": "none",
    }
)


def union_geometries(series):
    if hasattr(series, "union_all"):
        return series.union_all()
    return series.unary_union


def adjust_bbox(bounds: tuple[float, float, float, float], target_aspect: float):
    minx, miny, maxx, maxy = bounds
    width = maxx - minx
    height = maxy - miny
    current = width / height
    if current < target_aspect:
        new_width = height * target_aspect
        pad = (new_width - width) / 2
        minx -= pad
        maxx += pad
    else:
        new_height = width / target_aspect
        pad = (new_height - height) / 2
        miny -= pad
        maxy += pad
    return (minx, miny, maxx, maxy)


def bbox_gdf(bounds, crs=27700):
    return gpd.GeoDataFrame(geometry=[box(*bounds)], crs=crs)


class OsmContextHandler(osmium.SimpleHandler):
    def __init__(self, bbox_wgs84: tuple[float, float, float, float]):
        super().__init__()
        self.bbox = bbox_wgs84
        self.buildings: list[Polygon] = []
        self.roads: list[LineString] = []
        self.road_class: list[str] = []
        self.building_ways_seen = 0
        self.road_ways_seen = 0

    def _intersects_bbox(self, coords: list[tuple[float, float]]) -> bool:
        xs = [c[0] for c in coords]
        ys = [c[1] for c in coords]
        minx, miny, maxx, maxy = min(xs), min(ys), max(xs), max(ys)
        bx0, by0, bx1, by1 = self.bbox
        return not (maxx < bx0 or minx > bx1 or maxy < by0 or miny > by1)

    @staticmethod
    def _coords(nodes) -> list[tuple[float, float]] | None:
        coords = []
        try:
            for node in nodes:
                coords.append((float(node.lon), float(node.lat)))
        except Exception:
            return None
        return coords

    def way(self, way):
        tags = way.tags
        if "building" in tags:
            self.building_ways_seen += 1
            coords = self._coords(way.nodes)
            if not coords or len(coords) < 4 or coords[0] != coords[-1]:
                return
            if not self._intersects_bbox(coords):
                return
            try:
                poly = Polygon(coords)
            except Exception:
                return
            if poly.is_valid and not poly.is_empty:
                self.buildings.append(poly)
            return

        highway = tags.get("highway")
        if highway:
            self.road_ways_seen += 1
            if highway not in {
                "motorway",
                "trunk",
                "primary",
                "secondary",
                "tertiary",
                "unclassified",
                "residential",
                "living_street",
                "service",
            }:
                return
            coords = self._coords(way.nodes)
            if not coords or len(coords) < 2:
                return
            if not self._intersects_bbox(coords):
                return
            try:
                line = LineString(coords)
            except Exception:
                return
            if line.is_valid and not line.is_empty:
                self.roads.append(line)
                self.road_class.append(highway)


def extract_osm_context(panel_bounds_27700):
    if BUILDING_CACHE.exists() and ROAD_CACHE.exists():
        buildings = gpd.read_file(BUILDING_CACHE).to_crs(27700)
        roads = gpd.read_file(ROAD_CACHE).to_crs(27700)
        return buildings, roads, "cache"

    if not PBF.exists():
        raise FileNotFoundError(f"Missing OSM PBF: {PBF}")

    transformer = Transformer.from_crs(27700, 4326, always_xy=True)
    minlon, minlat = transformer.transform(panel_bounds_27700[0], panel_bounds_27700[1])
    maxlon, maxlat = transformer.transform(panel_bounds_27700[2], panel_bounds_27700[3])
    bbox_wgs84 = (minlon, minlat, maxlon, maxlat)

    handler = OsmContextHandler(bbox_wgs84)
    handler.apply_file(str(PBF), locations=True)

    buildings = gpd.GeoDataFrame(geometry=handler.buildings, crs=4326).to_crs(27700)
    roads = gpd.GeoDataFrame({"highway": handler.road_class, "geometry": handler.roads}, crs=4326).to_crs(27700)
    panel_box = bbox_gdf(panel_bounds_27700)
    if len(buildings):
      buildings = gpd.clip(buildings, panel_box)
    if len(roads):
      roads = gpd.clip(roads, panel_box)
    buildings.to_file(BUILDING_CACHE, driver="GPKG")
    roads.to_file(ROAD_CACHE, driver="GPKG")
    return buildings, roads, "osm-pbf"


def validate_buildings(buildings: gpd.GeoDataFrame):
    if buildings.empty:
        raise RuntimeError(
            "Building footprints are unavailable. Cannot reproduce the Beijing-style built-environment texture without a real building layer."
        )
    areas = buildings.geometry.area
    median_area = float(areas.median())
    if len(buildings) < 200:
        raise RuntimeError(
            f"Building footprint check failed: only {len(buildings)} polygons intersect panel (c), fewer than 200."
        )
    if median_area > 20000:
        raise RuntimeError(
            f"Building footprint check failed: median polygon area is {median_area:.1f} m2, probably not a building layer."
        )
    return median_area


def prep_axes(ax):
    ax.set_facecolor(PAL["panel_bg"])
    ax.set_xticks([])
    ax.set_yticks([])
    for spine in ax.spines.values():
        spine.set_visible(True)
        spine.set_color(PAL["border"])
        spine.set_linewidth(0.65)
    ax.set_aspect("equal")


def set_bounds(ax, bounds):
    ax.set_xlim(bounds[0], bounds[2])
    ax.set_ylim(bounds[1], bounds[3])


def title(ax, text, y_frac=0.055):
    x0, x1 = ax.get_xlim()
    y0, y1 = ax.get_ylim()
    ax.text(
        x0 + 0.035 * (x1 - x0),
        y1 - y_frac * (y1 - y0),
        text,
        fontsize=8.9,
        fontweight="bold",
        color=PAL["text"],
        va="top",
        ha="left",
        zorder=50,
    )


def scale_bar(ax, x, y, segments: list[float], labels: list[str], height=150, fontsize=7.0):
    start = x
    colors = ["#222222", "#BDBDBD", "#222222", "#BDBDBD"]
    for i, seg in enumerate(segments):
        ax.add_patch(
            Rectangle(
                (start, y),
                seg,
                height,
                facecolor=colors[i % len(colors)],
                edgecolor="none",
                zorder=60,
            )
        )
        start += seg
    tick_positions = [0]
    cumulative = 0
    for seg in segments:
        cumulative += seg
        tick_positions.append(cumulative)
    for lab, pos in zip(labels, tick_positions):
        ax.text(x + pos, y + height + height * 0.85, lab, ha="center", va="bottom", fontsize=fontsize, color=PAL["text"], zorder=60)


def north_arrow(ax, x, y, length=700):
    ax.annotate(
        "",
        xy=(x, y + length),
        xytext=(x, y),
        arrowprops=dict(arrowstyle="->", color=PAL["text"], lw=0.9),
        zorder=60,
    )
    ax.text(x, y + length + 130, "N", ha="center", va="bottom", fontsize=9, fontweight="bold", color=PAL["text"], zorder=60)


def plot_boundary(gdf, ax, color, linewidth, alpha=1.0):
    gdf.boundary.plot(ax=ax, color=color, linewidth=linewidth, alpha=alpha, zorder=20)


def draw_zoom_box(ax, bounds, linewidth=0.6):
    minx, miny, maxx, maxy = bounds
    rect = Rectangle(
        (minx, miny),
        maxx - minx,
        maxy - miny,
        fill=False,
        edgecolor=PAL["connectors"],
        linewidth=linewidth,
        linestyle=(0, (4, 3)),
        zorder=55,
    )
    ax.add_patch(rect)


def build_figure():
    lsoas = gpd.read_file(LSOA).to_crs(27700)
    thames = gpd.read_file(THAMES).to_crs(27700)
    lsoas["borough_name"] = lsoas["LSOA21NM"].str.replace(r" [0-9]{3}[A-Z]$", "", regex=True)
    boroughs = lsoas.dissolve(by="borough_name").reset_index()
    london = gpd.GeoDataFrame(geometry=[union_geometries(lsoas.geometry)], crs=27700)

    found_case = sorted(set(lsoas["LSOA21CD"]).intersection(CASE_CODES))
    found_comp = sorted(set(lsoas["LSOA21CD"]).intersection(COMPARISON_CODES))
    missing_case = sorted(set(CASE_CODES) - set(found_case))
    missing_comp = sorted(set(COMPARISON_CODES) - set(found_comp))
    if missing_case:
        print("WARNING missing selected case LSOAs:", ", ".join(missing_case))
    if missing_comp:
        print("WARNING missing comparison LSOAs:", ", ".join(missing_comp))

    case = lsoas[lsoas["LSOA21CD"].isin(CASE_CODES)]
    comparison = lsoas[lsoas["LSOA21CD"].isin(COMPARISON_CODES)]
    if case.empty:
        raise RuntimeError("No selected case LSOAs were found.")
    case_union = union_geometries(case.geometry)
    comp_union = union_geometries(comparison.geometry) if not comparison.empty else None
    case_gdf = gpd.GeoDataFrame(geometry=[case_union], crs=27700)
    comp_gdf = gpd.GeoDataFrame(geometry=[comp_union], crs=27700) if comp_union is not None else gpd.GeoDataFrame(geometry=[], crs=27700)

    a_bounds = adjust_bbox(tuple(london.total_bounds), 1.12)
    b_base = union_geometries(gpd.GeoSeries([case_union, comp_union], crs=27700).dropna()).buffer(4300)
    b_bounds = adjust_bbox(tuple(b_base.bounds), 1.12)
    c_base = case_union.buffer(1700)
    c_bounds = adjust_bbox(tuple(c_base.bounds), 0.94)

    buildings, roads, osm_source = extract_osm_context(c_bounds)
    panel_box = bbox_gdf(c_bounds)
    buildings = gpd.clip(buildings, panel_box)
    roads = gpd.clip(roads, panel_box) if not roads.empty else roads
    median_building_area = validate_buildings(buildings)

    main_roads = roads[roads["highway"].isin(["motorway", "trunk", "primary", "secondary", "tertiary"])] if "highway" in roads else roads.iloc[0:0]
    minor_roads = roads[~roads.index.isin(main_roads.index)] if "highway" in roads else roads

    fig = plt.figure(figsize=(9.5, 6.3), facecolor=PAL["figure_bg"])
    gs = fig.add_gridspec(2, 2, width_ratios=[1.0, 1.65], height_ratios=[1, 1], wspace=0.025, hspace=0.03)
    ax_a = fig.add_subplot(gs[0, 0])
    ax_b = fig.add_subplot(gs[1, 0])
    ax_c = fig.add_subplot(gs[:, 1])
    for ax in [ax_a, ax_b, ax_c]:
        prep_axes(ax)

    # Panel A
    london.plot(ax=ax_a, color=PAL["london"], edgecolor="none", zorder=1)
    plot_boundary(boroughs, ax_a, "#DADADA", 0.25, alpha=0.78)
    london.boundary.plot(ax=ax_a, color="#BDBDBD", linewidth=0.60, zorder=4)
    thames.plot(ax=ax_a, color=PAL["thames"], linewidth=1.45, zorder=5)
    case_gdf.boundary.plot(ax=ax_a, color="white", linewidth=2.1, zorder=9)
    case_gdf.plot(ax=ax_a, color=PAL["case_fill"], alpha=0.10, edgecolor="none", zorder=10)
    case_gdf.boundary.plot(ax=ax_a, color=PAL["case"], linewidth=1.35, zorder=11)
    set_bounds(ax_a, a_bounds)
    draw_zoom_box(ax_a, b_bounds, linewidth=0.55)
    title(ax_a, "(a) Greater London")
    scale_bar(ax_a, a_bounds[0] + 7000, a_bounds[1] + 6500, [20000, 20000], ["0", "20", "40 km"], height=320, fontsize=6.7)

    # Panel B
    b_box = bbox_gdf(b_bounds)
    b_lsoas = gpd.clip(lsoas, b_box)
    b_boroughs = gpd.clip(boroughs, b_box)
    b_thames = gpd.clip(thames, b_box)
    b_lsoas.plot(ax=ax_b, color=PAL["panel_bg"], edgecolor="#DCDCDC", linewidth=0.18, zorder=1)
    plot_boundary(b_boroughs, ax_b, "#D0D0D0", 0.30, alpha=0.88)
    b_thames.plot(ax=ax_b, color=PAL["thames"], linewidth=1.75, zorder=5)
    if not comp_gdf.empty:
        comp_gdf.plot(ax=ax_b, color=PAL["comparison_fill"], alpha=0.12, edgecolor="none", zorder=12)
        comp_gdf.boundary.plot(ax=ax_b, color=PAL["comparison"], linewidth=1.0, zorder=13)
    case_gdf.plot(ax=ax_b, color=PAL["case_fill"], alpha=0.10, edgecolor="none", zorder=14)
    case_gdf.boundary.plot(ax=ax_b, color=PAL["case"], linewidth=1.1, zorder=15)
    set_bounds(ax_b, b_bounds)
    draw_zoom_box(ax_b, c_bounds, linewidth=0.55)
    title(ax_b, "(b) East London")
    scale_bar(ax_b, b_bounds[0] + 850, b_bounds[1] + 850, [5000, 5000], ["0", "5", "10 km"], height=150, fontsize=6.7)

    # Panel C
    c_box = bbox_gdf(c_bounds)
    c_lsoas = gpd.clip(lsoas, c_box)
    c_thames = gpd.clip(thames, c_box)
    c_lsoas.plot(ax=ax_c, color=PAL["panel_bg"], edgecolor="none", zorder=0)
    c_thames.plot(ax=ax_c, color=PAL["thames"], linewidth=2.4, zorder=4)
    if not minor_roads.empty:
        minor_roads.plot(ax=ax_c, color=PAL["roads"], linewidth=0.25, alpha=0.65, zorder=8)
    if not main_roads.empty:
        main_roads.plot(ax=ax_c, color=PAL["main_roads"], linewidth=0.45, alpha=0.78, zorder=9)
    plot_boundary(c_lsoas, ax_c, PAL["admin"], 0.25, alpha=0.75)
    buildings.plot(ax=ax_c, color=PAL["buildings"], edgecolor="none", alpha=0.85, zorder=18)
    case_gdf.plot(ax=ax_c, color=PAL["case_fill"], alpha=0.05, edgecolor="none", zorder=30)
    case_gdf.boundary.plot(ax=ax_c, color=PAL["case"], linewidth=1.50, zorder=32)
    case.boundary.plot(ax=ax_c, color=PAL["case"], linewidth=0.35, alpha=0.25, zorder=31)
    set_bounds(ax_c, c_bounds)
    title(ax_c, "(c) Barking Riverside / Thames View case area", y_frac=0.070)
    scale_bar(ax_c, c_bounds[0] + 450, c_bounds[1] + 420, [500, 500, 1000], ["0", "0.5", "1", "2 km"], height=90, fontsize=6.7)
    north_arrow(ax_c, c_bounds[2] - 520, c_bounds[3] - 930, length=520)

    # Hand-drawn legend keeps the panel C key legible after the full figure is
    # scaled into the Word manuscript page.
    lx, ly, lw, lh = 0.655, 0.075, 0.315, 0.185
    ax_c.add_patch(
        Rectangle(
            (lx, ly),
            lw,
            lh,
            transform=ax_c.transAxes,
            facecolor="white",
            edgecolor="#BDBDBD",
            linewidth=0.55,
            zorder=80,
            clip_on=False,
        )
    )
    rows = [
        ("patch_case", "Case area"),
        ("patch_building", "Buildings"),
        ("line_road", "Roads"),
        ("line_thames", "River Thames"),
    ]
    for idx, (kind, label) in enumerate(rows):
        y = ly + lh - 0.042 - idx * 0.042
        x0 = lx + 0.035
        x1 = lx + 0.080
        if kind == "patch_case":
            ax_c.add_patch(Rectangle((x0, y - 0.011), 0.038, 0.022, transform=ax_c.transAxes,
                                     facecolor=PAL["case_fill"], edgecolor=PAL["case"], linewidth=0.85,
                                     alpha=0.24, zorder=81, clip_on=False))
        elif kind == "patch_building":
            ax_c.add_patch(Rectangle((x0, y - 0.011), 0.038, 0.022, transform=ax_c.transAxes,
                                     facecolor=PAL["buildings"], edgecolor="none", alpha=0.85,
                                     zorder=81, clip_on=False))
        elif kind == "line_road":
            ax_c.plot([x0, x1], [y, y], transform=ax_c.transAxes, color=PAL["main_roads"],
                      linewidth=1.0, zorder=81, clip_on=False)
        else:
            ax_c.plot([x0, x1], [y, y], transform=ax_c.transAxes, color=PAL["thames"],
                      linewidth=2.5, zorder=81, clip_on=False)
        ax_c.text(lx + 0.095, y, label, transform=ax_c.transAxes, ha="left", va="center",
                  fontsize=10.5, color=PAL["text"], zorder=82, clip_on=False)

    return fig, {
        "case_found": len(found_case),
        "case_missing": missing_case,
        "comparison_found": len(found_comp),
        "comparison_missing": missing_comp,
        "buildings": len(buildings),
        "median_building_area": median_building_area,
        "roads": len(roads),
        "osm_source": osm_source,
    }


def main():
    fig, report = build_figure()
    stem = OUT / "figure12_study_area_locator"
    fig.savefig(stem.with_suffix(".png"), dpi=600, bbox_inches="tight", facecolor=PAL["figure_bg"])
    fig.savefig(stem.with_suffix(".pdf"), bbox_inches="tight", facecolor=PAL["figure_bg"])
    fig.savefig(stem.with_suffix(".svg"), bbox_inches="tight", facecolor=PAL["figure_bg"])
    print("Saved:")
    print(stem.with_suffix(".png"))
    print(stem.with_suffix(".pdf"))
    print(stem.with_suffix(".svg"))
    print("Report:")
    print(f"- LSOA layer: {LSOA}")
    print(f"- River Thames layer: {THAMES}")
    print(f"- OSM PBF layer: {PBF}")
    print(f"- selected case LSOAs found: {report['case_found']} / {len(CASE_CODES)}")
    if report["case_missing"]:
        print(f"- missing selected case LSOAs: {', '.join(report['case_missing'])}")
    print(f"- comparison LSOAs found: {report['comparison_found']} / {len(COMPARISON_CODES)}")
    if report["comparison_missing"]:
        print(f"- missing comparison LSOAs: {', '.join(report['comparison_missing'])}")
    print(f"- building footprints plotted in panel (c): {report['buildings']}")
    print(f"- median building footprint area: {report['median_building_area']:.1f} m2")
    print(f"- roads plotted in panel (c): {report['roads']}")
    print(f"- OSM context source: {report['osm_source']}")
    print("- required building footprint layer: available and passed safety checks")


if __name__ == "__main__":
    main()
