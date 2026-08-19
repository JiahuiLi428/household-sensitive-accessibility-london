from pathlib import Path
import warnings

import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib as mpl
import matplotlib.patheffects as pe
import networkx as nx
import numpy as np
import osmnx as ox
import pandas as pd
import pyogrio
from pyproj import Transformer
from matplotlib.lines import Line2D
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.patches import ConnectionPatch, FancyArrowPatch, Rectangle
from matplotlib.ticker import FuncFormatter
from shapely.geometry import box
from shapely.ops import unary_union

# Study walking speed is 3.6 km/h (r5r default), i.e. 60 m per minute.
WALK_KMH = 3.6
WALK_METRES_PER_MIN = WALK_KMH * 1000 / 60


ROOT = Path(__file__).resolve().parents[1]
ANALYSIS = ROOT / "data" / "output" / "analysis"
BOUNDARIES = ROOT / "data" / "boundaries"
POI = ROOT / "data" / "poi"
OUT = ROOT / "figures" / "publication_revised"
OUT.mkdir(parents=True, exist_ok=True)
FINAL_OUT = ROOT / "outputs" / "manuscript" / "final"
FINAL_OUT.mkdir(parents=True, exist_ok=True)
GRID_GPKG = ROOT / "extension" / "data_intermediate" / "london_grid_100m_residential.gpkg"
GRID_CSV = ROOT / ".codex_work" / "figure12" / "grid_access_e01000096.csv"

mpl.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Arial", "Helvetica", "DejaVu Sans", "sans-serif"],
    "svg.fonttype": "none",
    "font.size": 9,
    "axes.titlesize": 10.5,
    "axes.titleweight": "bold",
    "axes.labelsize": 8.5,
    "xtick.labelsize": 8,
    "ytick.labelsize": 8,
    "figure.dpi": 130,
    "savefig.dpi": 450,
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
})

NEGATIVE_CASE = [
    "E01000095", "E01000094", "E01034478", "E01000096",
    "E01000090", "E01034476", "E01000014",
]
POSITIVE_CASE = ["E01003566", "E01003525", "E01003531", "E01003607"]

COLORS = {
    "ink": "#202124",
    "muted": "#616161",
    "grid": "#D8DEE4",
    "land": "#F7F7F4",
    "boundary": "#A9A9A2",
    "thames": "#9EC5D3",
    "red": "#B8504F",
    "blue": "#477CA8",
    "orange": "#D69A45",
    "green": "#5B8F72",
    "purple": "#7C6BAA",
}


def load_base():
    lsoas = gpd.read_file(BOUNDARIES / "london_lsoa_2021.gpkg").to_crs(27700)
    lsoas["borough_name"] = lsoas["LSOA21NM"].str.replace(r" [0-9]{3}[A-Z]$", "", regex=True)
    boroughs = lsoas.dissolve("borough_name", as_index=False)[["borough_name", "geometry"]]
    thames = gpd.read_file(BOUNDARIES / "river_thames_osm.geojson").to_crs(27700)
    return lsoas, boroughs, thames


def add_north_scale(ax, bounds, scale_m=10000):
    xmin, ymin, xmax, ymax = bounds
    w, h = xmax - xmin, ymax - ymin
    x0, y0 = xmin + 0.075 * w, ymin + 0.065 * h
    ax.plot([x0, x0 + scale_m], [y0, y0], color=COLORS["ink"], lw=0.7, zorder=8)
    ax.plot([x0, x0], [y0 - 0.008 * h, y0 + 0.008 * h], color=COLORS["ink"], lw=0.7, zorder=8)
    ax.plot([x0 + scale_m, x0 + scale_m], [y0 - 0.008 * h, y0 + 0.008 * h], color=COLORS["ink"], lw=0.7, zorder=8)
    scale_label = f"{scale_m / 1000:g} km" if scale_m >= 1000 else f"{scale_m:g} m"
    ax.text(x0 + scale_m / 2, y0 - 0.025 * h, scale_label, ha="center", va="top", fontsize=7, color=COLORS["ink"])
    nx0, ny0 = xmax - 0.08 * w, ymax - 0.11 * h
    ax.annotate("N", xy=(nx0, ny0 + 0.045 * h), ha="center", fontsize=8, weight="bold", color=COLORS["ink"])
    ax.annotate("", xy=(nx0, ny0 + 0.035 * h), xytext=(nx0, ny0 - 0.035 * h),
                arrowprops=dict(arrowstyle="-|>", lw=0.8, color=COLORS["ink"]))


def plot_london_base(ax, lsoas, boroughs, thames):
    lsoas.boundary.plot(ax=ax, color="#FFFFFF", linewidth=0.015, zorder=2)
    boroughs.boundary.plot(ax=ax, color=COLORS["boundary"], linewidth=0.17, zorder=4)
    thames.plot(ax=ax, color=COLORS["thames"], linewidth=0.55, alpha=0.9, zorder=5)
    ax.set_axis_off()
    ax.set_aspect("equal")


def figure7():
    lsoas, boroughs, thames = load_base()
    care = pd.read_csv(ANALYSIS / "care_mobility_accessibility_lsoa.csv")
    typ = pd.read_csv(ANALYSIS / "vulnerability_typology_lsoa.csv")
    lisa = pd.read_csv(ANALYSIS / "family_care_mismatch_lisa_lsoa.csv")
    gdf = lsoas.merge(
        care[["LSOA21CD", "care_walk_index", "care_mismatch_index", "care_need_score", "care_demand_group"]],
        on="LSOA21CD", how="left"
    ).merge(
        typ[["LSOA21CD", "vulnerability_typology"]],
        on="LSOA21CD", how="left"
    ).merge(
        lisa[["LSOA21CD", "lisa_cluster"]],
        on="LSOA21CD", how="left"
    )

    typ_colors = {
        "Low caregiving adult demand + Less deprived": "#E8E8E3",
        "Low caregiving adult demand + More deprived": "#80ACC6",
        "High caregiving adult demand + Less deprived": "#E5B461",
        "High caregiving adult demand + More deprived": "#B85253",
    }
    lisa_colors = {
        "High-high mismatch cluster": "#B8504F",
        "Low-low surplus cluster": "#477CA8",
        "High-low spatial outlier": "#D6AA4D",
        "Low-high spatial outlier": "#A7C9DA",
        "Not significant": "#ECECE8",
    }

    fig, axes = plt.subplots(2, 2, figsize=(11.2, 8.85), constrained_layout=False)
    axes = axes.ravel()
    bounds = gdf.total_bounds
    for ax in axes:
        ax.set_facecolor("white")
        ax.set_xlim(bounds[0], bounds[2])
        ax.set_ylim(bounds[1], bounds[3])
        ax.set_axis_off()
        ax.set_aspect("equal")

    gdf.plot(column="care_walk_index", ax=axes[0], cmap="YlGnBu", vmin=0, vmax=100,
             linewidth=0, legend=False, missing_kwds={"color": "#EFEFEB"})
    plot_london_base(axes[0], gdf, boroughs, thames)
    axes[0].set_title("A. Walking accessibility baseline", loc="left", pad=4)
    axes[0].text(0.0, 0.985, "Caregiving service-basket score, WALK 15", transform=axes[0].transAxes,
                 va="top", fontsize=8, color=COLORS["muted"])
    sm = mpl.cm.ScalarMappable(norm=mpl.colors.Normalize(0, 100), cmap="YlGnBu")
    cb = fig.colorbar(sm, ax=axes[0], orientation="horizontal", fraction=0.045, pad=0.025)
    cb.set_label("Access score (0-100)", fontsize=7)
    cb.ax.tick_params(labelsize=7, length=2)

    lim = np.nanquantile(np.abs(gdf["care_mismatch_index"]), 0.98)
    gdf.plot(column="care_mismatch_index", ax=axes[1], cmap="RdBu_r", vmin=-lim, vmax=lim,
             linewidth=0, legend=False, missing_kwds={"color": "#EFEFEB"})
    plot_london_base(axes[1], gdf, boroughs, thames)
    axes[1].set_title("B. Need-access mismatch", loc="left", pad=4)
    axes[1].text(0.0, 0.985, "Positive values mean need exceeds modelled access", transform=axes[1].transAxes,
                 va="top", fontsize=8, color=COLORS["muted"])
    sm = mpl.cm.ScalarMappable(norm=mpl.colors.Normalize(-lim, lim), cmap="RdBu_r")
    cb = fig.colorbar(sm, ax=axes[1], orientation="horizontal", fraction=0.045, pad=0.025)
    cb.set_label("Mismatch score", fontsize=7)
    cb.ax.tick_params(labelsize=7, length=2)

    gdf.assign(_color=gdf["vulnerability_typology"].map(typ_colors).fillna("#EFEFEB")).plot(
        ax=axes[2], color=gdf["vulnerability_typology"].map(typ_colors).fillna("#EFEFEB"), linewidth=0
    )
    plot_london_base(axes[2], gdf, boroughs, thames)
    axes[2].set_title("C. Care-demand and deprivation context", loc="left", pad=4)
    axes[2].text(0.0, 0.985, "Vulnerability typology used to interpret mismatch", transform=axes[2].transAxes,
                 va="top", fontsize=8, color=COLORS["muted"])
    typ_handles = [
        Line2D([0], [0], marker="s", color="none", markerfacecolor=typ_colors[k], markersize=7,
               label=v)
        for k, v in [
            ("Low caregiving adult demand + Less deprived", "Low care + less deprived"),
            ("Low caregiving adult demand + More deprived", "Low care + more deprived"),
            ("High caregiving adult demand + Less deprived", "High care + less deprived"),
            ("High caregiving adult demand + More deprived", "High care + more deprived"),
        ]
    ]
    axes[2].legend(handles=typ_handles, loc="lower left", bbox_to_anchor=(0.0, -0.03),
                   frameon=False, ncol=2, fontsize=6.8, handletextpad=0.35, columnspacing=0.8)

    gdf.plot(ax=axes[3], color=gdf["lisa_cluster"].map(lisa_colors).fillna("#EFEFEB"), linewidth=0)
    plot_london_base(axes[3], gdf, boroughs, thames)
    axes[3].set_title("D. Spatial concentration of mismatch", loc="left", pad=4)
    axes[3].text(0.0, 0.985, "Local Moran's I clusters for caregiving mismatch", transform=axes[3].transAxes,
                 va="top", fontsize=8, color=COLORS["muted"])
    lisa_handles = [
        Line2D([0], [0], marker="s", color="none", markerfacecolor=lisa_colors[k], markersize=7,
               label=v)
        for k, v in [
            ("High-high mismatch cluster", "High-high deficit"),
            ("Low-low surplus cluster", "Low-low surplus"),
            ("High-low spatial outlier", "High-low outlier"),
            ("Low-high spatial outlier", "Low-high outlier"),
            ("Not significant", "Not significant"),
        ]
    ]
    axes[3].legend(handles=lisa_handles, loc="lower left", bbox_to_anchor=(0.0, -0.035),
                   frameon=False, ncol=2, fontsize=6.8, handletextpad=0.35, columnspacing=0.8)

    add_north_scale(axes[0], bounds)
    fig.suptitle("Figure 7A-D. Caregiving-Adult Accessibility Mismatch Diagnostics",
                 x=0.04, y=0.985, ha="left", fontsize=15, fontweight="bold", color=COLORS["ink"])
    fig.text(0.04, 0.952,
             "Each panel answers a separate question: where access is high or low, where care need exceeds access, "
             "which household contexts are involved, and whether deficits cluster spatially.",
             ha="left", va="top", fontsize=9, color=COLORS["muted"])
    fig.subplots_adjust(left=0.035, right=0.985, top=0.91, bottom=0.055, wspace=0.045, hspace=0.24)
    for ext in ("png", "pdf"):
        fig.savefig(OUT / f"figure7_family_care_diagnostics_abcd_revised.{ext}", bbox_inches="tight")
    plt.close(fig)


def score_time(x, threshold):
    x = pd.to_numeric(x, errors="coerce")
    score = 100 * (1 - np.minimum(x, threshold) / threshold)
    return np.maximum(score.fillna(threshold * 0), 0)


def figure9():
    wide = pd.read_csv(ANALYSIS / "r5r_accessibility_enriched_wide.csv")
    eta = pd.read_csv(ANALYSIS / "walk_vs_transit_imd_inequality_effect_sizes.csv")
    lookup = [
        ("education_schools", "Schools", "education_schools"),
        ("food_retail_supermarkets", "Supermarkets", "food_retail_supermarkets"),
        ("green_space_parks", "Parks", "green_space_parks"),
        ("healthcare_gps", "GPs", "healthcare_gps"),
        ("healthcare_pharmacies", "Pharmacies", "healthcare_pharmacies"),
        ("transport_bus_stops", "Bus stops", "transport_bus_stops"),
        ("transport_underground_metro_stations", "Underground/metro", "transport_underground_metro_stations"),
    ]
    rows = []
    for cat, label, prefix in lookup:
        walk_score = score_time(wide[f"{prefix}_walk_15min"], 15).mean()
        pt_score = score_time(wide[f"{prefix}_walk_transit_30min"], 30).mean()
        e = eta.loc[eta["facility_category"].eq(cat)].iloc[0]
        rows.extend([
            {"facility": label, "scenario": "Walk 15", "metric": "Mean access score", "value": walk_score},
            {"facility": label, "scenario": "Walk + PT 30", "metric": "Mean access score", "value": pt_score},
            {"facility": label, "scenario": "Walk 15", "metric": "IMD effect size", "value": e["eta_sq_h_walk_15min"]},
            {"facility": label, "scenario": "Walk + PT 30", "metric": "IMD effect size", "value": e["eta_sq_h_walk_transit_30min"]},
        ])
    df = pd.DataFrame(rows)
    order = [x[1] for x in lookup][::-1]
    colors = {"Walk 15": "#477CA8", "Walk + PT 30": "#C47F66"}

    fig, axes = plt.subplots(1, 2, figsize=(10.4, 5.15), gridspec_kw={"width_ratios": [1.25, 1.0]})
    for ax, metric, xmax, xlabel in [
        (axes[0], "Mean access score", 100, "Mean score (0-100; higher is better)"),
        (axes[1], "IMD effect size", None, "Eta-squared H (higher = stronger IMD difference)"),
    ]:
        sub = df[df["metric"].eq(metric)]
        y = np.arange(len(order))
        for offset, scen in [(-0.18, "Walk 15"), (0.18, "Walk + PT 30")]:
            vals = sub[sub["scenario"].eq(scen)].set_index("facility").loc[order, "value"].to_numpy()
            ax.barh(y + offset, vals, height=0.32, color=colors[scen], label=scen)
            for yy, val in zip(y + offset, vals):
                txt = f"{val:.1f}" if metric == "Mean access score" else f"{val:.3f}"
                ax.text(val + (1.0 if metric == "Mean access score" else 0.002), yy, txt,
                        va="center", fontsize=7, color=COLORS["muted"])
        ax.set_yticks(y)
        ax.set_yticklabels(order)
        ax.grid(axis="x", color=COLORS["grid"], lw=0.6)
        ax.spines[["top", "right", "left"]].set_visible(False)
        ax.tick_params(axis="y", length=0)
        ax.set_xlabel(xlabel)
        ax.set_title("A. Access improves" if metric == "Mean access score" else "B. Inequality differences can remain",
                     loc="left")
        if xmax:
            ax.set_xlim(0, xmax + 10)
        else:
            ax.set_xlim(0, max(sub["value"]) * 1.25)
    axes[0].legend(loc="lower right", frameon=False, ncol=1, fontsize=8)
    fig.suptitle("Figure 9. Accessibility Paradox: Access Improves, Differences Remain",
                 x=0.035, y=0.995, ha="left", fontsize=14, fontweight="bold", color=COLORS["ink"])
    fig.text(0.035, 0.94,
             "The public-transport scenario raises mean facility access, but deprivation-related differences do not disappear uniformly.",
             ha="left", fontsize=9, color=COLORS["muted"])
    fig.subplots_adjust(top=0.83, bottom=0.12, left=0.16, right=0.98, wspace=0.26)
    for ext in ("png", "pdf"):
        fig.savefig(OUT / f"figure9_accessibility_paradox_revised.{ext}", bbox_inches="tight")
    plt.close(fig)


def figure10():
    summary = pd.read_csv(ANALYSIS / "transit_gain_winners_losers_summary.csv")
    summary = summary[summary["transit_gain_group"].isin(["Bottom 20% gain", "Top 20% gain"])].copy()
    metrics = [
        ("mean_transit_gain", "Transit gain", "score points"),
        ("mean_walk_access", "Walking baseline", "score"),
        ("mean_imd_decile", "Mean IMD decile", "decile"),
        ("inner_london_pct", "Inner London share", "%"),
        ("care_mismatch_index", "Care mismatch", "score"),
    ]
    colors = {"Bottom 20% gain": "#5F8F88", "Top 20% gain": "#C47F66"}
    fig, axes = plt.subplots(1, 5, figsize=(11.2, 3.65), constrained_layout=False)
    for ax, (col, title, unit) in zip(axes, metrics):
        vals = summary.set_index("transit_gain_group").loc[["Bottom 20% gain", "Top 20% gain"], col]
        bars = ax.bar([0, 1], vals, width=0.58, color=[colors[k] for k in vals.index])
        ymin = min(0, vals.min() * 1.2 if vals.min() < 0 else 0)
        ymax = vals.max() * 1.22 if vals.max() > 0 else 1
        ax.set_ylim(ymin, ymax)
        for b, v in zip(bars, vals):
            va = "bottom" if v >= 0 else "top"
            ytxt = v + (ymax - ymin) * (0.025 if v >= 0 else -0.035)
            ax.text(b.get_x() + b.get_width() / 2, ytxt, f"{v:.1f}", ha="center", va=va,
                    fontsize=8, color=COLORS["ink"])
        ax.axhline(0, color="#777777", lw=0.55)
        ax.set_title(title, fontsize=9.6, pad=5)
        ax.set_xticks([0, 1])
        ax.set_xticklabels(["Bottom\n20%", "Top\n20%"], fontsize=8)
        ax.set_ylabel(unit, fontsize=7.5, color=COLORS["muted"])
        ax.grid(axis="y", color=COLORS["grid"], lw=0.55)
        ax.spines[["top", "right", "left"]].set_visible(False)
        ax.tick_params(axis="y", length=0, labelsize=7)
    handles = [Line2D([0], [0], color=colors[k], lw=7, label=k) for k in ["Bottom 20% gain", "Top 20% gain"]]
    fig.legend(handles=handles, loc="lower center", bbox_to_anchor=(0.5, 0.01), ncol=2, frameon=False, fontsize=8.5)
    fig.suptitle("Figure 10. High-Gain and Low-Gain Areas of Public Transport Accessibility",
                 x=0.035, y=0.99, ha="left", fontsize=13.5, fontweight="bold", color=COLORS["ink"])
    fig.text(0.035, 0.91,
             "Top and bottom quintiles are compared with identical visual treatment; labels show group means.",
             fontsize=9, color=COLORS["muted"], ha="left")
    fig.subplots_adjust(left=0.055, right=0.985, top=0.78, bottom=0.23, wspace=0.45)
    for ext in ("png", "pdf"):
        fig.savefig(OUT / f"figure10_transit_gain_profile_revised.{ext}", bbox_inches="tight")
    plt.close(fig)


def load_pois(crs=27700):
    specs = [
        ("School", POI / "education_schools.geojson", "#C7864B", "^"),
        ("GP", POI / "healthcare_gps.geojson", "#B5534B", "D"),
        ("Hospital", POI / "healthcare_hospitals.geojson", "#7A3E65", "*"),
        ("Pharmacy", POI / "healthcare_pharmacies.geojson", "#9568A3", "D"),
        ("Supermarket", POI / "food_retail_supermarkets.geojson", "#C99A32", "s"),
        ("Park", POI / "green_space_parks.geojson", "#5F8A65", "o"),
        ("Bus stop", POI / "transport_bus_stops.geojson", "#79A6BD", "o"),
        ("Station", POI / "transport_underground_metro_stations.geojson", "#305F72", "^"),
    ]
    out = []
    for label, path, color, marker in specs:
        g = gpd.read_file(path).to_crs(crs)
        g["facility"] = label
        if "name" not in g.columns:
            g["name"] = pd.NA
        non_points = ~g.geometry.geom_type.eq("Point")
        if non_points.any():
            g.loc[non_points, "geometry"] = g.loc[non_points, "geometry"].representative_point()
        g["plot_color"] = color
        g["marker"] = marker
        out.append(g[["facility", "name", "plot_color", "marker", "geometry"]])
    return pd.concat(out, ignore_index=True)


def graph_edges_for_case(case_area):
    poly = case_area.buffer(1800).to_crs(4326).geometry.iloc[0]
    try:
        ox.settings.use_cache = True
        ox.settings.log_console = False
        G = ox.graph_from_polygon(poly, network_type="walk", simplify=True, retain_all=True, truncate_by_edge=True)
        Gp = ox.project_graph(G, to_crs="EPSG:27700")
        nodes, edges = ox.graph_to_gdfs(Gp, nodes=True, edges=True)
        return Gp, nodes, edges
    except Exception as exc:
        warnings.warn(f"OSMnx network download failed; using no-road fallback. {exc}")
        return None, None, None


def reachable_edges(G, nodes, edges, origin_point, distance_m):
    if G is None:
        return gpd.GeoDataFrame(geometry=[], crs=27700)
    # OSM walk graphs can contain tiny disconnected service-road components.
    # Anchor the illustrative reach calculation to the largest walkable
    # component so a nearby isolated driveway does not collapse the map.
    if G.is_directed():
        largest_nodes = max(nx.weakly_connected_components(G), key=len)
    else:
        largest_nodes = max(nx.connected_components(G), key=len)
    main_graph = G.subgraph(largest_nodes).copy()
    origin_node = ox.distance.nearest_nodes(main_graph, origin_point.x, origin_point.y)
    lengths = nx.single_source_dijkstra_path_length(
        main_graph, origin_node, cutoff=distance_m, weight="length"
    )
    reachable_nodes = set(lengths.keys())
    uvals = edges.index.get_level_values(0)
    vvals = edges.index.get_level_values(1)
    mask = [u in reachable_nodes and v in reachable_nodes for u, v in zip(uvals, vvals)]
    return edges.loc[mask].copy()


def load_detail_context(bounds_27700, cache_tag="v6"):
    """Load the same understated OSM context used by the Appendix D case maps."""
    cache_dir = ROOT / ".codex_work" / "figure12" / f"detail_context_{cache_tag}"
    cache_dir.mkdir(parents=True, exist_ok=True)
    cache = {
        "landuse": cache_dir / "landuse.pkl",
        "buildings": cache_dir / "buildings.pkl",
        "roads": cache_dir / "roads.pkl",
    }
    if all(path.exists() for path in cache.values()):
        return tuple(pd.read_pickle(cache[key]) for key in ("landuse", "buildings", "roads"))

    xmin, ymin, xmax, ymax = bounds_27700
    clip_poly = gpd.GeoDataFrame(geometry=[box(xmin, ymin, xmax, ymax)], crs=27700)
    clip_wgs = clip_poly.to_crs(4326).total_bounds
    osm_pbf = ROOT / "data" / "raw" / "osm" / "greater-london-latest.osm.pbf"
    residential = ROOT / "extension" / "data_intermediate" / "osm_residential_features.gpkg"

    landuse = gpd.read_file(residential, layer="residential_landuse", bbox=(xmin, ymin, xmax, ymax))
    landuse = landuse.to_crs(27700).clip(clip_poly.geometry.iloc[0])

    buildings = pyogrio.read_dataframe(
        osm_pbf,
        layer="multipolygons",
        bbox=tuple(clip_wgs),
        columns=["osm_id", "building"],
        where="building IS NOT NULL",
    ).to_crs(27700)
    buildings = buildings.clip(clip_poly.geometry.iloc[0])

    roads = pyogrio.read_dataframe(
        osm_pbf,
        layer="lines",
        bbox=tuple(clip_wgs),
        columns=["highway"],
        where="highway IS NOT NULL",
    ).to_crs(27700)
    roads = roads.clip(clip_poly.geometry.iloc[0])
    roads["road_group"] = np.select(
        [
            roads["highway"].isin(["motorway", "trunk", "primary", "secondary"]),
            roads["highway"].isin(["tertiary", "unclassified"]),
            roads["highway"].isin(["residential", "living_street", "service"]),
        ],
        ["Major road", "Connector road", "Local street"],
        default="Other",
    )
    roads = roads[~roads["road_group"].eq("Other")].copy()
    for key, frame in (("landuse", landuse), ("buildings", buildings), ("roads", roads)):
        frame.to_pickle(cache[key])
    return landuse, buildings, roads


def figure12():
    lsoas, boroughs, thames = load_base()
    neg = lsoas[lsoas["LSOA21CD"].isin(NEGATIVE_CASE)].copy()
    pos = lsoas[lsoas["LSOA21CD"].isin(POSITIVE_CASE)].copy()
    neg_area = gpd.GeoDataFrame(geometry=[unary_union(neg.geometry)], crs=lsoas.crs)
    pos_area = gpd.GeoDataFrame(geometry=[unary_union(pos.geometry)], crs=lsoas.crs)
    representative = neg[neg["LSOA21CD"].eq("E01000096")].copy()
    goresbrook = neg[neg["LSOA21CD"].eq("E01000014")].copy()
    origin = representative.geometry.iloc[0].centroid
    pois = load_pois(27700)
    local_window = representative.buffer(1800).geometry.iloc[0]
    local_pois = pois[pois.geometry.within(local_window)].copy()
    try:
        rail = ox.features_from_polygon(
            representative.buffer(2000).to_crs(4326).geometry.iloc[0],
            {"railway": "station"},
        ).to_crs(27700)
        rail = rail[rail.geometry.geom_type.eq("Point")].copy()
        rail["facility"] = "Rail station"
        rail["plot_color"] = "#173F5F"
        rail["marker"] = "*"
        local_pois = pd.concat(
            [local_pois, rail[["facility", "name", "plot_color", "marker", "geometry"]]],
            ignore_index=True,
        )
    except Exception as exc:
        warnings.warn(f"Rail-station query failed; continuing with the local station file. {exc}")

    G, nodes, edges = graph_edges_for_case(representative)
    reach_5 = reachable_edges(G, nodes, edges, origin, 5 * WALK_METRES_PER_MIN)
    reach_10 = reachable_edges(G, nodes, edges, origin, 10 * WALK_METRES_PER_MIN)
    reach_15 = reachable_edges(G, nodes, edges, origin, 15 * WALK_METRES_PER_MIN)

    grid = gpd.read_file(GRID_GPKG)[["grid_id", "geometry"]]
    scores = pd.read_csv(GRID_CSV)[["grid_id", "basket_score"]]
    grid = grid.merge(scores, on="grid_id", how="inner").to_crs(27700)
    low_cut = grid["basket_score"].quantile(0.10)
    lowest = grid[grid["basket_score"].le(low_cut)]

    # Asymmetric locator-to-mechanism layout. The local accessibility map is the
    # hero panel; the two left panels establish case geography and retain the
    # genuine discontinuous Goresbrook LSOA without competing for equal space.
    fig = plt.figure(figsize=(7.35, 4.90), facecolor="white")
    ax_a = fig.add_axes([0.035, 0.565, 0.235, 0.355])
    ax_inset = fig.add_axes([0.035, 0.155, 0.235, 0.315])
    ax_b = fig.add_axes([0.300, 0.125, 0.675, 0.815])
    cax = fig.add_axes([0.700, 0.052, 0.275, 0.020])

    # Panel A: East London context, filled and directly labelled.
    context = gpd.GeoDataFrame(geometry=[unary_union([neg_area.geometry.iloc[0], pos_area.geometry.iloc[0]])], crs=27700)
    cxmin, cymin, cxmax, cymax = context.buffer(1800).total_bounds
    lsoas.cx[cxmin:cxmax, cymin:cymax].plot(ax=ax_a, color="#F6F6F3", edgecolor="white", linewidth=0.06)
    boroughs.cx[cxmin:cxmax, cymin:cymax].boundary.plot(ax=ax_a, color="#B7B7B0", linewidth=0.32)
    thames.cx[cxmin:cxmax, cymin:cymax].plot(ax=ax_a, color="#9CC7D2", linewidth=0.95, alpha=0.95)
    neg.plot(ax=ax_a, facecolor="#E66A4A", edgecolor="#A83725", linewidth=0.95, alpha=0.62, zorder=6)
    pos.plot(ax=ax_a, facecolor="#35A7A0", edgecolor="#087771", linewidth=0.95, alpha=0.60, zorder=6)
    nxy = neg[~neg["LSOA21CD"].eq("E01000014")].geometry.unary_union.centroid
    pxy = pos.geometry.unary_union.centroid
    gxy = goresbrook.geometry.iloc[0].centroid
    ax_a.annotate("Barking Riverside /\nThames View", xy=(nxy.x, nxy.y),
                  xytext=(cxmin + 0.52*(cxmax-cxmin), cymin + 0.08*(cymax-cymin)),
                  fontsize=6.5, weight="bold", color="#8E271C", ha="left",
                  arrowprops=dict(arrowstyle="-", color="#8E271C", lw=0.8))
    ax_a.annotate("Newham counter-case\n(four LSOAs)", xy=(pxy.x, pxy.y),
                  xytext=(cxmin + 0.04*(cxmax-cxmin), cymax - 0.18*(cymax-cymin)),
                  fontsize=6.5, weight="bold", color="#006F6B", ha="left",
                  arrowprops=dict(arrowstyle="-", color="#006F6B", lw=0.8))
    ax_a.set_xlim(cxmin, cxmax); ax_a.set_ylim(cymin, cymax)
    ax_a.set_title("a  East London case locations", loc="left", pad=2, fontsize=8.1, weight="bold")
    add_north_scale(ax_a, (cxmin, cymin, cxmax, cymax), scale_m=3000)
    ax_a.set_axis_off(); ax_a.set_aspect("equal"); ax_a.set_anchor("C")
    ax_a.add_patch(mpl.patches.Rectangle((0, 0), 1, 1, transform=ax_a.transAxes,
                                         fill=False, edgecolor="#B8B8B4", linewidth=0.55,
                                         clip_on=False, zorder=20))

    # Panel A inset: retain and explicitly identify the genuine selected LSOA.
    # It is not a geometry artefact and remains inside the fixed seven-LSOA case.
    main_neg = neg[~neg["LSOA21CD"].eq("E01000014")]
    inset_union = unary_union([main_neg.geometry.unary_union, goresbrook.geometry.iloc[0]])
    ixmin, iymin, ixmax, iymax = gpd.GeoSeries([inset_union], crs=27700).buffer(650).total_bounds
    lsoas.cx[ixmin:ixmax, iymin:iymax].plot(
        ax=ax_inset, color="#F7F7F4", edgecolor="white", linewidth=0.10
    )
    thames.cx[ixmin:ixmax, iymin:iymax].plot(
        ax=ax_inset, color="#9CC7D2", linewidth=0.85, alpha=0.92
    )
    main_neg.plot(
        ax=ax_inset, facecolor="#E66A4A", edgecolor="#A83725",
        linewidth=0.90, alpha=0.62, zorder=5
    )
    goresbrook.plot(
        ax=ax_inset, facecolor="#F08A6D", edgecolor="#7C2118",
        linewidth=1.25, alpha=0.78, hatch="///", zorder=6
    )
    main_xy = main_neg.geometry.unary_union.centroid
    ax_inset.plot(
        [gxy.x, main_xy.x], [gxy.y, main_xy.y], color="#8E271C",
        linewidth=0.7, linestyle=(0, (2, 2)), alpha=0.75, zorder=4
    )
    ax_inset.annotate(
        "E01000014\nreal selected LSOA",
        xy=(gxy.x, gxy.y),
        xytext=(ixmin + 0.04*(ixmax-ixmin), iymax - 0.14*(iymax-iymin)),
        fontsize=6.1, weight="bold", color="#7C2118", ha="left", va="top",
        bbox=dict(boxstyle="round,pad=0.16", fc="white", ec="none", alpha=0.90),
        arrowprops=dict(arrowstyle="->", color="#7C2118", lw=0.75),
    )
    ax_inset.annotate(
        "main six-LSOA cluster",
        xy=(main_xy.x, main_xy.y),
        xytext=(ixmin + 0.50*(ixmax-ixmin), iymin + 0.07*(iymax-iymin)),
        fontsize=5.8, color="#8E271C", ha="left",
        arrowprops=dict(arrowstyle="-", color="#8E271C", lw=0.65),
    )
    ax_inset.set_xlim(ixmin, ixmax); ax_inset.set_ylim(iymin, iymax)
    ax_inset.set_title("b  Barking case and Goresbrook edge", loc="left", pad=2,
                       fontsize=7.3, weight="bold", color="#5A5A5A")
    add_north_scale(ax_inset, (ixmin, iymin, ixmax, iymax), scale_m=1000)
    ax_inset.set_axis_off(); ax_inset.set_aspect("equal"); ax_inset.set_anchor("C")
    ax_inset.add_patch(mpl.patches.Rectangle((0, 0), 1, 1, transform=ax_inset.transAxes,
                                             fill=False, edgecolor="#B8B8B4", linewidth=0.55,
                                             clip_on=False, zorder=20))

    # Panel B: dominant map of local 100 m evidence and network reach.
    xmin, ymin, xmax, ymax = representative.buffer(1250).total_bounds
    ax_b.set_xlim(xmin, xmax)
    ax_b.set_ylim(ymin, ymax)
    lsoas.cx[xmin:xmax, ymin:ymax].plot(ax=ax_b, color="#FAFAF8", edgecolor="#E7E7E2", linewidth=0.16)
    thames.cx[xmin:xmax, ymin:ymax].plot(ax=ax_b, color="#9CC7D2", linewidth=1.15, alpha=0.9)
    local_case = neg.cx[xmin:xmax, ymin:ymax]
    if not local_case.empty:
        local_case.boundary.plot(ax=ax_b, color="#C66A52", linewidth=0.45,
                                 alpha=0.42, zorder=2.5)
    representative.plot(ax=ax_b, facecolor="#E66A4A", edgecolor="none",
                        alpha=0.07, zorder=2.6)
    grid.plot(ax=ax_b, column="basket_score", cmap="YlGnBu", vmin=0, vmax=65,
              edgecolor="white", linewidth=0.10, alpha=0.92, zorder=3)
    if edges is not None:
        edges.cx[xmin:xmax, ymin:ymax].plot(ax=ax_b, color="#DADAD6", linewidth=0.20, alpha=0.45, zorder=2)
        reach_15.plot(ax=ax_b, color="#78A6C8", linewidth=0.95, alpha=0.72, zorder=4)
        reach_10.plot(ax=ax_b, color="#2A788E", linewidth=1.15, alpha=0.86, zorder=5)
        reach_5.plot(ax=ax_b, color="#F28E2B", linewidth=1.35, alpha=0.95, zorder=6)
    lowest.boundary.plot(ax=ax_b, color="#8C2D6A", linewidth=1.05, zorder=7)
    representative.boundary.plot(ax=ax_b, color="#A83725", linewidth=1.35, zorder=8)
    local_pois = local_pois.cx[xmin:xmax, ymin:ymax]
    for facility, grp in local_pois.groupby("facility"):
        grp = grp.assign(_dist=grp.geometry.distance(origin)).sort_values("_dist")
        if facility == "Bus stop":
            sample = grp.head(18); size, alpha = 9, 0.46
        elif facility == "Rail station":
            sample = grp[grp["name"].astype(str).str.contains("Barking Riverside", case=False, na=False)].head(1)
            size, alpha = 54, 0.98
        elif facility == "Station":
            sample = grp[~grp["name"].astype(str).str.fullmatch(r"\d+|Platform\s+\d+", case=False, na=False)]
            sample = sample.drop_duplicates("name").head(2); size, alpha = 30, 0.90
        else:
            sample, size, alpha = grp.head(5), 29, 0.92
        if sample.empty:
            continue
        ax_b.scatter(sample.geometry.x, sample.geometry.y, s=size,
                     c=sample["plot_color"].iloc[0], marker=sample["marker"].iloc[0],
                     edgecolors="white" if facility != "Bus stop" else "none",
                     linewidths=0.35, alpha=alpha, zorder=9, label=facility)
    ax_b.scatter([origin.x], [origin.y], s=34, color="#202020", marker="x", linewidths=1.0, zorder=10)
    ax_b.annotate("model origin", xy=(origin.x, origin.y), xytext=(5, 5), textcoords="offset points",
                  fontsize=5.8, color="#444444", zorder=11)
    stations = local_pois[
        local_pois["facility"].eq("Rail station")
        & local_pois["name"].astype(str).str.contains("Barking Riverside", case=False, na=False)
    ].copy()
    if not stations.empty:
        stations["_dist"] = stations.geometry.distance(origin)
        st = stations.sort_values("_dist").iloc[0]
        station_name = st.get("name") if pd.notna(st.get("name")) else "Nearest rail/metro station"
        ax_b.annotate(str(station_name), xy=(st.geometry.x, st.geometry.y), xytext=(6, 7), textcoords="offset points",
                      fontsize=6.4, weight="bold", color="#244A6B",
                      bbox=dict(boxstyle="round,pad=0.15", fc="white", ec="none", alpha=0.90),
                      arrowprops=dict(arrowstyle="-", color="#244A6B", lw=0.7), zorder=11)
    ax_b.set_title("c  Representative local mechanism: LSOA E01000096", loc="left", pad=3,
                   fontsize=8.5, weight="bold")
    ax_b.text(0.01, 0.985,
              "Scheduled/potential walking access | 5/10/15-minute streets | 100 m basket score",
              transform=ax_b.transAxes, va="top", fontsize=6.2, color=COLORS["muted"],
              bbox=dict(boxstyle="round,pad=0.18", fc="white", ec="none", alpha=0.82))
    network_handles = [
        Line2D([0], [0], color="#F28E2B", lw=1.8, label="5 min"),
        Line2D([0], [0], color="#2A788E", lw=1.8, label="10 min"),
        Line2D([0], [0], color="#78A6C8", lw=1.8, label="15 min"),
        Line2D([0], [0], color="#8C2D6A", lw=1.4, label="Lowest 10% cells"),
    ]
    poi_handles = [
        Line2D([0], [0], marker="o", color="none", markerfacecolor=COLORS["orange"], markeredgecolor="white", markersize=6, label="School"),
        Line2D([0], [0], marker="s", color="none", markerfacecolor=COLORS["green"], markeredgecolor="white", markersize=6, label="Supermarket"),
        Line2D([0], [0], marker="^", color="none", markerfacecolor=COLORS["red"], markeredgecolor="white", markersize=6, label="GP"),
        Line2D([0], [0], marker="D", color="none", markerfacecolor=COLORS["purple"], markeredgecolor="white", markersize=5.5, label="Pharmacy"),
        Line2D([0], [0], marker="P", color="none", markerfacecolor=COLORS["blue"], markeredgecolor="white", markersize=6, label="Underground/metro"),
        Line2D([0], [0], marker="*", color="none", markerfacecolor="#173F5F", markeredgecolor="white", markersize=7, label="Rail station"),
        Line2D([0], [0], marker=".", color="#5A5A5A", markersize=7, label="Bus stop"),
    ]
    leg1 = ax_b.legend(handles=network_handles, title="Walking evidence", loc="upper right",
                       bbox_to_anchor=(0.995, 0.91), frameon=True, fontsize=6.0,
                       title_fontsize=6.2, facecolor="white", edgecolor="#D0D0CC",
                       framealpha=0.92, ncol=2, handlelength=1.8, columnspacing=0.9)
    ax_b.add_artist(leg1)
    ax_b.legend(handles=poi_handles, title="Facilities", loc="lower right", frameon=True,
                fontsize=5.95, title_fontsize=6.2, ncol=2, facecolor="white",
                edgecolor="#D0D0CC", framealpha=0.92, handletextpad=0.42,
                columnspacing=0.9, borderpad=0.55)
    sm = mpl.cm.ScalarMappable(norm=mpl.colors.Normalize(0, 65), cmap="YlGnBu")
    cb = fig.colorbar(sm, cax=cax, orientation="horizontal")
    cb.set_label("100 m caregiving service-basket accessibility (0–65)", fontsize=5.8, labelpad=1.5)
    cb.ax.tick_params(labelsize=5.4, length=1.8, pad=1)
    cb.outline.set_linewidth(0.45)
    add_north_scale(ax_b, (xmin, ymin, xmax, ymax), scale_m=500)
    ax_b.set_axis_off(); ax_b.set_aspect("equal"); ax_b.set_anchor("C")
    ax_b.add_patch(mpl.patches.Rectangle((0, 0), 1, 1, transform=ax_b.transAxes,
                                         fill=False, edgecolor="#B8B8B4", linewidth=0.60,
                                         clip_on=False, zorder=20))

    fig.text(0.035, 0.060,
             "Orange-red: Barking Riverside/Thames View | Teal: Newham illustrative counter-case",
             fontsize=5.5, color=COLORS["muted"], ha="left", va="center")
    fig.text(0.035, 0.037,
             "Fixed seven-LSOA Barking case; reported statistics unchanged.",
             fontsize=5.2, color=COLORS["muted"], ha="left", va="center")
    qa_svg = ROOT / ".codex_work" / "figure12" / "figure12_case_accessibility_v5.svg"
    fig.savefig(qa_svg, facecolor="white")
    fig.savefig(FINAL_OUT / "figure12_case_accessibility_v5.png", dpi=600, facecolor="white")
    fig.savefig(FINAL_OUT / "figure12_case_accessibility_v5.pdf", facecolor="white")
    plt.close(fig)


def figure12_v6():
    """Appendix-D-style Figure 12 with a larger local-accessibility hero panel."""
    lsoas, boroughs, thames = load_base()
    neg = lsoas[lsoas["LSOA21CD"].isin(NEGATIVE_CASE)].copy()
    pos = lsoas[lsoas["LSOA21CD"].isin(POSITIVE_CASE)].copy()
    representative = neg[neg["LSOA21CD"].eq("E01000096")].copy()
    goresbrook = neg[neg["LSOA21CD"].eq("E01000014")].copy()
    main_neg = neg[~neg["LSOA21CD"].eq("E01000014")].copy()
    origin = representative.geometry.iloc[0].centroid

    # The fixed detailed frame deliberately leaves room on the east side for the
    # same internal vertical legend used by the Appendix D case maps.
    map_width, map_height, x_offset = 6200.0, 4000.0, 850.0
    cx, cy = origin.x + x_offset, origin.y
    xmin, xmax = cx - map_width / 2, cx + map_width / 2
    ymin, ymax = cy - map_height / 2, cy + map_height / 2
    detail_bounds = (xmin, ymin, xmax, ymax)
    landuse, buildings, roads = load_detail_context(detail_bounds)

    pois = load_pois(27700)
    local_pois = pois.cx[xmin:xmax, ymin:ymax].copy()
    gtfs = ROOT / "data" / "tfl_step_free" / "gtfs" / "stops.txt"
    if gtfs.exists():
        stops = pd.read_csv(gtfs, low_memory=False)
        rail = stops[stops["stop_name"].astype(str).str.contains("Barking Riverside", case=False, na=False)].copy()
        if not rail.empty:
            rail = rail.drop_duplicates("stop_name").head(1)
            rail = gpd.GeoDataFrame(
                rail,
                geometry=gpd.points_from_xy(rail["stop_lon"], rail["stop_lat"]),
                crs=4326,
            ).to_crs(27700)
            rail["facility"] = "Rail station"
            rail["name"] = rail["stop_name"]
            rail["plot_color"] = "#173F5F"
            rail["marker"] = "*"
            local_pois = pd.concat(
                [local_pois, rail[["facility", "name", "plot_color", "marker", "geometry"]]],
                ignore_index=True,
            )

    G, nodes, edges = graph_edges_for_case(representative)
    reach_5 = reachable_edges(G, nodes, edges, origin, 5 * WALK_METRES_PER_MIN)
    reach_10 = reachable_edges(G, nodes, edges, origin, 10 * WALK_METRES_PER_MIN)
    reach_15 = reachable_edges(G, nodes, edges, origin, 15 * WALK_METRES_PER_MIN)

    grid = gpd.read_file(GRID_GPKG)[["grid_id", "geometry"]]
    scores = pd.read_csv(GRID_CSV)[["grid_id", "basket_score"]]
    grid = grid.merge(scores, on="grid_id", how="inner").to_crs(27700)
    low_cut = grid["basket_score"].quantile(0.10)
    lowest = grid[grid["basket_score"].le(low_cut)].copy()
    access_cmap = LinearSegmentedColormap.from_list(
        "appendix_access_blues",
        ["#EEF3F4", "#C8DDE2", "#9FC2CE", "#769FB5", "#4F7894"],
    )

    fig = plt.figure(figsize=(7.35, 4.90), facecolor="white")
    ax_a = fig.add_axes([0.025, 0.590, 0.185, 0.300])
    ax_inset = fig.add_axes([0.025, 0.205, 0.185, 0.285])
    ax_b = fig.add_axes([0.235, 0.105, 0.745, 0.805])

    # a: regional locator with both study-area colours retained.
    context_union = unary_union([neg.geometry.unary_union, pos.geometry.unary_union])
    cxmin, cymin, cxmax, cymax = gpd.GeoSeries([context_union], crs=27700).buffer(1800).total_bounds
    lsoas.cx[cxmin:cxmax, cymin:cymax].plot(ax=ax_a, color="#F6F6F3", edgecolor="white", linewidth=0.05)
    boroughs.cx[cxmin:cxmax, cymin:cymax].boundary.plot(ax=ax_a, color="#B7B7B0", linewidth=0.28)
    thames.cx[cxmin:cxmax, cymin:cymax].plot(ax=ax_a, color="#9FC3D2", linewidth=0.82)
    neg.plot(ax=ax_a, facecolor="#E66A4A", edgecolor="#A83725", linewidth=0.85, alpha=0.62, zorder=6)
    pos.plot(ax=ax_a, facecolor="#35A7A0", edgecolor="#087771", linewidth=0.85, alpha=0.60, zorder=6)
    nxy = main_neg.geometry.unary_union.centroid
    pxy = pos.geometry.unary_union.centroid
    ax_a.annotate(
        "Barking Riverside /\nThames View",
        xy=(nxy.x, nxy.y),
        xytext=(cxmin + 0.50 * (cxmax-cxmin), cymin + 0.06 * (cymax-cymin)),
        fontsize=5.4, weight="bold", color="#8E271C", ha="left",
        arrowprops=dict(arrowstyle="-", color="#8E271C", lw=0.65),
    )
    ax_a.annotate(
        "Newham counter-case\n(four LSOAs)",
        xy=(pxy.x, pxy.y),
        xytext=(cxmin + 0.03 * (cxmax-cxmin), cymax - 0.17 * (cymax-cymin)),
        fontsize=5.4, weight="bold", color="#006F6B", ha="left",
        arrowprops=dict(arrowstyle="-", color="#006F6B", lw=0.65),
    )
    ax_a.set_xlim(cxmin, cxmax); ax_a.set_ylim(cymin, cymax)
    ax_a.set_title("a  East London\ncase locations", loc="left", pad=2, fontsize=6.4, weight="bold", linespacing=0.95)
    add_north_scale(ax_a, (cxmin, cymin, cxmax, cymax), scale_m=3000)
    ax_a.set_axis_off(); ax_a.set_aspect("equal"); ax_a.set_anchor("C")

    # b: the real discontinuous selected LSOA remains visible rather than being
    # cosmetically removed from the case geography.
    gxy = goresbrook.geometry.iloc[0].centroid
    main_xy = main_neg.geometry.unary_union.centroid
    inset_union = unary_union([main_neg.geometry.unary_union, goresbrook.geometry.iloc[0]])
    ixmin, iymin, ixmax, iymax = gpd.GeoSeries([inset_union], crs=27700).buffer(650).total_bounds
    lsoas.cx[ixmin:ixmax, iymin:iymax].plot(ax=ax_inset, color="#F7F7F4", edgecolor="white", linewidth=0.08)
    thames.cx[ixmin:ixmax, iymin:iymax].plot(ax=ax_inset, color="#9FC3D2", linewidth=0.76)
    main_neg.plot(ax=ax_inset, facecolor="#E66A4A", edgecolor="#A83725", linewidth=0.80, alpha=0.62, zorder=5)
    goresbrook.plot(ax=ax_inset, facecolor="#F08A6D", edgecolor="#7C2118", linewidth=1.05, alpha=0.80, hatch="///", zorder=6)
    ax_inset.plot([gxy.x, main_xy.x], [gxy.y, main_xy.y], color="#8E271C", lw=0.65, ls=(0, (2, 2)), alpha=0.75)
    ax_inset.annotate(
        "E01000014\nreal selected LSOA",
        xy=(gxy.x, gxy.y),
        xytext=(ixmin + 0.03*(ixmax-ixmin), iymax - 0.13*(iymax-iymin)),
        fontsize=5.2, weight="bold", color="#7C2118", ha="left", va="top",
        bbox=dict(boxstyle="round,pad=0.14", fc="white", ec="none", alpha=0.90),
        arrowprops=dict(arrowstyle="->", color="#7C2118", lw=0.65),
    )
    ax_inset.annotate(
        "main six-LSOA cluster", xy=(main_xy.x, main_xy.y),
        xytext=(ixmin + 0.48*(ixmax-ixmin), iymin + 0.055*(iymax-iymin)),
        fontsize=4.9, color="#8E271C", ha="left",
        arrowprops=dict(arrowstyle="-", color="#8E271C", lw=0.58),
    )
    ax_inset.set_xlim(ixmin, ixmax); ax_inset.set_ylim(iymin, iymax)
    ax_inset.set_title("b  Barking case and\nGoresbrook edge", loc="left", pad=2, fontsize=6.0, weight="bold", linespacing=0.95)
    add_north_scale(ax_inset, (ixmin, iymin, ixmax, iymax), scale_m=1000)
    ax_inset.set_axis_off(); ax_inset.set_aspect("equal"); ax_inset.set_anchor("C")

    # c: Appendix-D-style detailed mechanism map.
    local_lsoas = lsoas.cx[xmin:xmax, ymin:ymax]
    local_lsoas.plot(ax=ax_b, color="#F8F7F3", edgecolor="#D4D1C9", linewidth=0.16, zorder=1)
    if not landuse.empty:
        landuse.plot(ax=ax_b, color="#E8EEE3", edgecolor="none", alpha=0.42, zorder=1.2)
    if not buildings.empty:
        buildings.plot(ax=ax_b, color="#D5D9DA", edgecolor="none", alpha=0.56, zorder=1.4)
    road_specs = {
        "Local street": ("#C5C3BD", 0.12, 0.62),
        "Connector road": ("#AAA9A4", 0.22, 0.72),
        "Major road": ("#878B89", 0.38, 0.78),
    }
    for group, (colour, width, alpha) in road_specs.items():
        subset = roads[roads["road_group"].eq(group)]
        if not subset.empty:
            subset.plot(ax=ax_b, color=colour, linewidth=width, alpha=alpha, zorder=1.6)
    thames.cx[xmin:xmax, ymin:ymax].plot(ax=ax_b, color="#9FC3D2", linewidth=0.76, zorder=1.8)
    grid.plot(ax=ax_b, column="basket_score", cmap=access_cmap, vmin=0, vmax=65,
              edgecolor="none", alpha=0.90, zorder=3)
    if edges is not None:
        reach_15.plot(ax=ax_b, color="#315F78", linewidth=0.70, linestyle=(0, (6, 3, 1.5, 3)), alpha=0.94, zorder=4)
        reach_10.plot(ax=ax_b, color="#315F78", linewidth=0.70, linestyle=(0, (3, 2)), alpha=0.94, zorder=5)
        reach_5.plot(ax=ax_b, color="#315F78", linewidth=0.78, linestyle="solid", alpha=0.98, zorder=6)
    lowest.boundary.plot(ax=ax_b, color="#7C5268", linewidth=0.78, zorder=7)
    neg.cx[xmin:xmax, ymin:ymax].boundary.plot(ax=ax_b, color="#C85B43", linewidth=0.30, alpha=0.46, zorder=7.5)
    representative.boundary.plot(ax=ax_b, color="#111111", linewidth=1.05, zorder=8)

    facility_styles = {
        "School": ("^", "white", "#C7864B", 18),
        "GP": ("D", "white", "#B5534B", 15),
        "Hospital": ("*", "#7A3E65", "#7A3E65", 24),
        "Pharmacy": ("D", "#9568A3", "#9568A3", 13),
        "Supermarket": ("s", "white", "#C99A32", 14),
        "Park": ("o", "white", "#5F8A65", 16),
        "Station": ("^", "#305F72", "#305F72", 20),
        "Rail station": ("*", "#173F5F", "#173F5F", 29),
        "Bus stop": ("o", "#79A6BD", "#79A6BD", 4.5),
    }
    for facility, grp in local_pois.groupby("facility"):
        if facility not in facility_styles:
            continue
        grp = grp.copy()
        if facility == "Bus stop":
            grp["_gx"] = (grp.geometry.x // 150).astype(int)
            grp["_gy"] = (grp.geometry.y // 150).astype(int)
            sample = grp.drop_duplicates(["_gx", "_gy"])
        elif facility == "Park":
            grp["_gx"] = (grp.geometry.x // 300).astype(int)
            grp["_gy"] = (grp.geometry.y // 300).astype(int)
            sample = grp.drop_duplicates(["_gx", "_gy"])
        elif facility == "Rail station":
            sample = grp.head(1)
        else:
            sample = grp.assign(_dist=grp.geometry.distance(origin)).sort_values("_dist").head(24)
        marker, face, edge, size = facility_styles[facility]
        ax_b.scatter(
            sample.geometry.x, sample.geometry.y, s=size, marker=marker,
            facecolors=face, edgecolors=edge, linewidths=0.55 if facility != "Bus stop" else 0,
            alpha=0.94, zorder=9,
        )
    ax_b.scatter([origin.x], [origin.y], s=26, marker="x", color="#111111", linewidths=0.9, zorder=10)

    ax_b.set_xlim(xmin, xmax); ax_b.set_ylim(ymin, ymax); ax_b.set_aspect("equal")
    for spine in ax_b.spines.values():
        spine.set_visible(True); spine.set_color("#222222"); spine.set_linewidth(0.48)
    transformer = Transformer.from_crs(27700, 4326, always_xy=True)
    def degree_minutes(value, suffix_positive, suffix_negative):
        absolute = abs(value); degrees = int(np.floor(absolute)); minutes = int(round((absolute-degrees)*60))
        if minutes == 60: degrees += 1; minutes = 0
        return f"{degrees}°{minutes:02d}′{suffix_negative if value < 0 else suffix_positive}"
    ax_b.xaxis.set_major_formatter(FuncFormatter(lambda value, _pos: degree_minutes(transformer.transform(value, (ymin+ymax)/2)[0], "E", "W")))
    ax_b.yaxis.set_major_formatter(FuncFormatter(lambda value, _pos: degree_minutes(transformer.transform((xmin+xmax)/2, value)[1], "N", "S")))
    ax_b.set_xticks(np.linspace(xmin + 0.08*map_width, xmax - 0.08*map_width, 5))
    ax_b.set_yticks(np.linspace(ymin + 0.10*map_height, ymax - 0.10*map_height, 4))
    ax_b.tick_params(axis="x", which="major", labelsize=4.4, length=2.0, width=0.4,
                     top=True, bottom=True, labeltop=True, labelbottom=True, pad=1.2)
    ax_b.tick_params(axis="y", which="major", labelsize=4.4, length=2.0, width=0.4,
                     left=True, right=True, labelleft=True, labelright=True, labelrotation=90, pad=1.2)
    ax_b.grid(color="#D8D6D0", linewidth=0.20, linestyle="dotted", zorder=0)

    # Scale and a two-tone north arrow match the Appendix D maps.
    sx, sy = xmin + 0.055*map_width, ymin + 0.075*map_height
    ax_b.plot([sx, sx+1000], [sy, sy], color="#202020", lw=1.05, zorder=12)
    ax_b.text(sx+500, sy+0.040*map_height, "1 km", ha="center", va="bottom", fontsize=5.0, zorder=12)
    nx0, ny0 = xmin + 0.695*map_width, ymin + 0.12*map_height
    nw, nh = 0.014*map_width, 0.105*map_height
    ax_b.add_patch(mpl.patches.Polygon([(nx0, ny0+nh), (nx0-nw, ny0), (nx0, ny0+0.025*map_height)],
                                       closed=True, facecolor="#111111", edgecolor="#111111", lw=0.35, zorder=12))
    ax_b.add_patch(mpl.patches.Polygon([(nx0, ny0+nh), (nx0+nw, ny0), (nx0, ny0+0.025*map_height)],
                                       closed=True, facecolor="white", edgecolor="#111111", lw=0.35, zorder=12))
    ax_b.text(nx0, ny0+nh+0.018*map_height, "N", ha="center", va="bottom", fontsize=5.5, weight="bold", zorder=12)

    # Internal vertical legend: same information architecture as Appendix D,
    # scaled for a composite figure rather than an entire A4 page.
    legend_ax = ax_b.inset_axes([0.745, 0.045, 0.240, 0.910], zorder=20)
    legend_ax.set_facecolor((0.99, 0.99, 0.98, 0.97))
    legend_ax.set_xlim(0, 1); legend_ax.set_ylim(0, 1)
    legend_ax.set_xticks([]); legend_ax.set_yticks([])
    for spine in legend_ax.spines.values():
        spine.set_visible(True); spine.set_color("#C8C6C0"); spine.set_linewidth(0.55)
    legend_ax.text(0.08, 0.965, "Accessibility score", fontsize=5.8, weight="bold", va="top")
    cbar_ax = legend_ax.inset_axes([0.10, 0.785, 0.13, 0.145])
    cb = fig.colorbar(mpl.cm.ScalarMappable(norm=mpl.colors.Normalize(0, 65), cmap=access_cmap), cax=cbar_ax)
    cb.set_ticks([0, 15, 30, 45, 60]); cb.ax.tick_params(labelsize=4.4, length=1.4, width=0.35, pad=1)
    cb.outline.set_visible(False)

    legend_ax.text(0.08, 0.755, "Reachable street network", fontsize=5.6, weight="bold", va="top")
    line_specs = [(0.710, "solid", "5 min"), (0.670, (0, (3, 2)), "10 min"), (0.630, (0, (6, 3, 1.5, 3)), "15 min")]
    for y, style, label in line_specs:
        legend_ax.plot([0.10, 0.33], [y, y], color="#315F78", lw=1.05, ls=style)
        legend_ax.text(0.39, y, label, va="center", fontsize=4.8)

    legend_ax.text(0.08, 0.590, "Facilities", fontsize=5.6, weight="bold", va="top")
    legend_facilities = [
        ("School", "Schools"), ("GP", "GPs"), ("Hospital", "Hospitals"),
        ("Pharmacy", "Pharmacies"), ("Supermarket", "Supermarkets"),
        ("Park", "Parks"), ("Station", "Underground/metro"),
        ("Rail station", "Rail station"), ("Bus stop", "Bus stops"),
    ]
    for i, (facility, label) in enumerate(legend_facilities):
        x = 0.11; y = 0.545 - i*0.035
        marker, face, edge, _size = facility_styles[facility]
        legend_ax.scatter([x], [y], s=17 if facility != "Bus stop" else 8, marker=marker,
                          facecolors=face, edgecolors=edge, linewidths=0.55, zorder=3)
        legend_ax.text(x+0.09, y, label, va="center", fontsize=4.45)

    legend_ax.text(0.08, 0.225, "Analytical features", fontsize=5.6, weight="bold", va="top")
    analytic = [
        (0.175, "#111111", 0.95, "Representative LSOA"),
        (0.130, "#C85B43", 0.55, "Barking case LSOAs"),
        (0.085, "#7C5268", 0.78, "Lowest 10% cells"),
    ]
    for y, colour, width, label in analytic:
        legend_ax.add_patch(Rectangle((0.10, y-0.014), 0.055, 0.028, fill=False, edgecolor=colour, linewidth=width))
        legend_ax.text(0.20, y, label, va="center", fontsize=4.5)

    fig.text(0.235, 0.962, "c  Representative local mechanism — Barking Riverside / Thames View",
             fontsize=8.5, weight="bold", ha="left", va="top", color="#1F1F1F")
    fig.text(0.235, 0.938,
             "LSOA E01000096 | shared accessibility scale 0–65 | model origin marked ×",
             fontsize=5.8, ha="left", va="top", color="#555555")
    fig.text(0.025, 0.060,
             "Orange-red: Barking Riverside/Thames View | Teal: Newham illustrative counter-case",
             fontsize=5.2, color=COLORS["muted"], ha="left")
    fig.text(0.025, 0.039,
             "Fixed seven-LSOA Barking case; reported statistics unchanged.",
             fontsize=4.9, color=COLORS["muted"], ha="left")

    qa_svg = ROOT / ".codex_work" / "figure12" / "figure12_case_accessibility_v6.svg"
    fig.savefig(qa_svg, facecolor="white")
    fig.savefig(FINAL_OUT / "figure12_case_accessibility_v6.png", dpi=600, facecolor="white")
    fig.savefig(FINAL_OUT / "figure12_case_accessibility_v6.pdf", facecolor="white")
    plt.close(fig)


def figure12_v8():
    """Nested-locator Figure 12 with a dominant local-accessibility panel.

    This revision deliberately returns to the earlier United Kingdom -> East
    London -> local environment narrative while retaining the accessibility
    evidence and verified case geography added during the later review.
    """
    lsoas, boroughs, thames = load_base()
    neg = lsoas[lsoas["LSOA21CD"].isin(NEGATIVE_CASE)].copy()
    pos = lsoas[lsoas["LSOA21CD"].isin(POSITIVE_CASE)].copy()
    representative = neg[neg["LSOA21CD"].eq("E01000096")].copy()
    goresbrook = neg[neg["LSOA21CD"].eq("E01000014")].copy()
    br_case = neg[neg["LSOA21CD"].isin(["E01000094", "E01000095", "E01034478"])].copy()
    thames_view_case = neg[neg["LSOA21CD"].eq("E01034476")].copy()
    eastbury_case = representative.copy()
    edge_case = neg[neg["LSOA21CD"].isin(["E01000090", "E01000014"])].copy()
    origin = representative.geometry.iloc[0].centroid

    # The local frame follows the earlier casework map, but is shifted slightly
    # east to reserve quiet map space for a compact internal legend.
    map_width, map_height, x_offset = 6200.0, 4000.0, 850.0
    cx, cy = origin.x + x_offset, origin.y
    xmin, xmax = cx - map_width / 2, cx + map_width / 2
    ymin, ymax = cy - map_height / 2, cy + map_height / 2
    bounds = (xmin, ymin, xmax, ymax)
    landuse, buildings, roads = load_detail_context(bounds)

    pois = load_pois(27700)
    local_pois = pois.cx[xmin:xmax, ymin:ymax].copy()
    gtfs = ROOT / "data" / "tfl_step_free" / "gtfs" / "stops.txt"
    if gtfs.exists():
        stops = pd.read_csv(gtfs, low_memory=False)
        rail = stops[
            stops["stop_name"].astype(str).str.contains(
                "Barking Riverside", case=False, na=False
            )
        ].drop_duplicates("stop_name").head(1)
        if not rail.empty:
            rail = gpd.GeoDataFrame(
                rail,
                geometry=gpd.points_from_xy(rail["stop_lon"], rail["stop_lat"]),
                crs=4326,
            ).to_crs(27700)
            rail["facility"] = "Rail station"
            rail["name"] = rail["stop_name"]
            rail["plot_color"] = "#173F5F"
            rail["marker"] = "*"
            local_pois = pd.concat(
                [
                    local_pois,
                    rail[["facility", "name", "plot_color", "marker", "geometry"]],
                ],
                ignore_index=True,
            )

    G, nodes, edges = graph_edges_for_case(representative)
    reach_5 = reachable_edges(G, nodes, edges, origin, 5 * WALK_METRES_PER_MIN)
    reach_10 = reachable_edges(G, nodes, edges, origin, 10 * WALK_METRES_PER_MIN)
    reach_15 = reachable_edges(G, nodes, edges, origin, 15 * WALK_METRES_PER_MIN)

    grid = gpd.read_file(GRID_GPKG)[["grid_id", "geometry"]]
    scores = pd.read_csv(GRID_CSV)[["grid_id", "basket_score"]]
    grid = grid.merge(scores, on="grid_id", how="inner").to_crs(27700)
    low_cut = grid["basket_score"].quantile(0.10)
    lowest = grid[grid["basket_score"].le(low_cut)].copy()
    access_cmap = LinearSegmentedColormap.from_list(
        "nested_access_blues_v8",
        ["#F4F8F8", "#C6E1E5", "#82BAC5", "#3D8197", "#123F63"],
    )

    fig = plt.figure(figsize=(7.35, 4.90), facecolor="white")
    ax_uk = fig.add_axes([0.025, 0.575, 0.190, 0.305])
    ax_east = fig.add_axes([0.025, 0.175, 0.190, 0.305])
    ax_local = fig.add_axes([0.240, 0.105, 0.735, 0.785])

    # a: national locator. The London point is only a locator, not an analytical
    # unit, so the panel remains intentionally quiet.
    countries_path = BOUNDARIES / "Countries_December_2022_Boundaries_UK_BGC.gpkg"
    countries = gpd.read_file(countries_path).to_crs(27700)
    countries.plot(
        ax=ax_uk, facecolor="#D9DEDB", edgecolor="#A7AFAB", linewidth=0.38
    )
    london_outline = gpd.GeoSeries([lsoas.geometry.unary_union], crs=27700)
    london_point = london_outline.representative_point().iloc[0]
    ax_uk.scatter(
        [london_point.x], [london_point.y], s=26, facecolor="#E66A4A",
        edgecolor="#8E271C", linewidth=0.65, zorder=5
    )
    uxmin, uymin, uxmax, uymax = countries.total_bounds
    uw, uh = uxmax - uxmin, uymax - uymin
    ax_uk.set_xlim(uxmin - 0.04 * uw, uxmax + 0.04 * uw)
    ax_uk.set_ylim(uymin - 0.02 * uh, uymax + 0.03 * uh)
    add_north_scale(
        ax_uk, (uxmin - 0.04 * uw, uymin - 0.02 * uh, uxmax + 0.04 * uw, uymax + 0.03 * uh),
        scale_m=200000,
    )
    ax_uk.set_axis_off(); ax_uk.set_aspect("equal", adjustable="datalim"); ax_uk.set_anchor("C")
    ax_uk.add_patch(Rectangle((0, 0), 1, 1, transform=ax_uk.transAxes, fill=False,
                              edgecolor="#777773", linewidth=0.55, clip_on=False))

    # b: East London context with both cases directly encoded and the genuine
    # Goresbrook component explicitly retained.
    context_union = unary_union([neg.geometry.unary_union, pos.geometry.unary_union])
    exmin, eymin, exmax, eymax = gpd.GeoSeries([context_union], crs=27700).buffer(2100).total_bounds
    lsoas.cx[exmin:exmax, eymin:eymax].plot(
        ax=ax_east, color="#F2F3F0", edgecolor="#FFFFFF", linewidth=0.045
    )
    boroughs.cx[exmin:exmax, eymin:eymax].boundary.plot(
        ax=ax_east, color="#AEB3AF", linewidth=0.24
    )
    thames.cx[exmin:exmax, eymin:eymax].plot(
        ax=ax_east, color="#8FC7D8", linewidth=0.92, alpha=0.95
    )
    br_case.plot(ax=ax_east, facecolor="#E66A4A", edgecolor="#A83725",
                 linewidth=0.92, alpha=0.64, zorder=6)
    thames_view_case.plot(ax=ax_east, facecolor="#35A7A0", edgecolor="#087771",
                          linewidth=0.92, alpha=0.64, zorder=6)
    eastbury_case.plot(ax=ax_east, facecolor="#8B6BB1", edgecolor="#5D3D82",
                       linewidth=0.98, alpha=0.66, zorder=6)
    edge_case.plot(ax=ax_east, facecolor="#D9D6CF", edgecolor="#6C6A66",
                   linewidth=0.78, linestyle=(0, (3, 2)), alpha=0.72, zorder=6)
    pos.plot(ax=ax_east, facecolor="#4C91C6", edgecolor="#225C87",
             linewidth=0.88, alpha=0.58, zorder=6)
    brxy = br_case.geometry.unary_union.centroid
    tvxy = thames_view_case.geometry.unary_union.centroid
    exy = eastbury_case.geometry.unary_union.centroid
    pxy = pos.geometry.unary_union.centroid
    gxy = goresbrook.geometry.iloc[0].centroid
    ax_east.annotate(
        "Barking Riverside",
        xy=(brxy.x, brxy.y),
        xytext=(exmin + 0.48 * (exmax-exmin), eymin + 0.055 * (eymax-eymin)),
        fontsize=4.9, weight="bold", color="#8E271C", ha="left",
        arrowprops=dict(arrowstyle="-", color="#8E271C", lw=0.60),
    )
    ax_east.annotate(
        "Thames View",
        xy=(tvxy.x, tvxy.y),
        xytext=(exmin + 0.58 * (exmax-exmin), eymin + 0.20 * (eymax-eymin)),
        fontsize=4.8, weight="bold", color="#006F6B", ha="left",
        arrowprops=dict(arrowstyle="-", color="#006F6B", lw=0.58),
    )
    ax_east.annotate(
        "Eastbury\nE01000096",
        xy=(exy.x, exy.y),
        xytext=(exmin + 0.30 * (exmax-exmin), eymin + 0.31 * (eymax-eymin)),
        fontsize=4.6, weight="bold", color="#5D3D82", ha="right",
        arrowprops=dict(arrowstyle="-", color="#5D3D82", lw=0.58),
    )
    ax_east.annotate(
        "Newham\ncounter-case",
        xy=(pxy.x, pxy.y),
        xytext=(exmin + 0.035 * (exmax-exmin), eymax - 0.17 * (eymax-eymin)),
        fontsize=4.9, weight="bold", color="#225C87", ha="left", va="top",
        arrowprops=dict(arrowstyle="-", color="#225C87", lw=0.60),
    )
    ax_east.annotate(
        "Goresbrook edge\nE01000014",
        xy=(gxy.x, gxy.y),
        xytext=(exmin + 0.46 * (exmax-exmin), eymax - 0.10 * (eymax-eymin)),
        fontsize=4.5, color="#55524E", ha="left", va="top",
        bbox=dict(boxstyle="round,pad=0.12", fc="white", ec="none", alpha=0.90),
        arrowprops=dict(arrowstyle="->", color="#55524E", lw=0.58),
    )
    ax_east.set_xlim(exmin, exmax); ax_east.set_ylim(eymin, eymax)
    add_north_scale(ax_east, (exmin, eymin, exmax, eymax), scale_m=5000)
    ax_east.set_axis_off(); ax_east.set_aspect("equal"); ax_east.set_anchor("C")
    ax_east.add_patch(Rectangle((0, 0), 1, 1, transform=ax_east.transAxes, fill=False,
                                edgecolor="#777773", linewidth=0.55, clip_on=False))

    # c: the earlier pale local-environment map becomes the hero panel, with
    # analytical accessibility layers added without allowing them to obscure
    # the buildings, roads or case geography.
    local_lsoas = lsoas.cx[xmin:xmax, ymin:ymax]
    local_lsoas.plot(ax=ax_local, color="#F7F8F5", edgecolor="#DDDED9",
                     linewidth=0.12, zorder=1)
    if not landuse.empty:
        landuse.plot(ax=ax_local, color="#E8EEE3", edgecolor="none",
                     alpha=0.34, zorder=1.1)
    if not buildings.empty:
        buildings.plot(ax=ax_local, color="#9AAEB9", edgecolor="none",
                       alpha=0.58, zorder=1.3)
    road_specs = {
        "Local street": ("#C8C7C1", 0.12, 0.55),
        "Connector road": ("#B4B2AC", 0.21, 0.62),
        "Major road": ("#979994", 0.34, 0.70),
    }
    for group, (colour, width, alpha) in road_specs.items():
        subset = roads[roads["road_group"].eq(group)]
        if not subset.empty:
            subset.plot(ax=ax_local, color=colour, linewidth=width,
                        alpha=alpha, zorder=1.5)
    thames.cx[xmin:xmax, ymin:ymax].plot(
        ax=ax_local, color="#8FC7D8", linewidth=1.10, alpha=0.95, zorder=1.7
    )
    grid.plot(ax=ax_local, column="basket_score", cmap=access_cmap, vmin=0, vmax=65,
              edgecolor="#F8FAFA", linewidth=0.035, alpha=0.96, zorder=3)

    def _plot_reach(ax, layer, colour, width, style, zorder):
        before = len(ax.collections)
        layer.plot(ax=ax, color=colour, linewidth=width, linestyle=style,
                   alpha=0.98, zorder=zorder)
        for collection in ax.collections[before:]:
            collection.set_path_effects([
                pe.Stroke(linewidth=width + 1.15, foreground="white", alpha=0.94),
                pe.Normal(),
            ])

    if edges is not None:
        _plot_reach(ax_local, reach_15, "#356AA0", 1.05, (0, (5, 2, 1, 2)), 4)
        _plot_reach(ax_local, reach_10, "#008F87", 1.16, (0, (3, 1.8)), 5)
        _plot_reach(ax_local, reach_5, "#F28E2B", 1.30, "solid", 6)
    lowest.boundary.plot(ax=ax_local, color="#202020", linewidth=1.05,
                         linestyle=(0, (3.5, 2.2)), zorder=7)
    br_case.cx[xmin:xmax, ymin:ymax].boundary.plot(
        ax=ax_local, color="#A83725", linewidth=0.68, alpha=0.86, zorder=7.4
    )
    thames_view_case.cx[xmin:xmax, ymin:ymax].boundary.plot(
        ax=ax_local, color="#087771", linewidth=0.68, alpha=0.86, zorder=7.4
    )
    edge_case.cx[xmin:xmax, ymin:ymax].boundary.plot(
        ax=ax_local, color="#6C6A66", linewidth=0.58, linestyle=(0, (3, 2)),
        alpha=0.82, zorder=7.4
    )
    representative.plot(ax=ax_local, facecolor="#8B6BB1", edgecolor="none",
                        alpha=0.075, zorder=7.6)
    representative.boundary.plot(ax=ax_local, color="#5D3D82", linewidth=1.25, zorder=8)

    facility_styles = {
        "School": ("p", "white", "#D07A2D", 20),
        "GP": ("P", "#C34F4A", "white", 20),
        "Hospital": ("X", "#A43D79", "white", 24),
        "Pharmacy": ("h", "#7651A5", "white", 19),
        "Supermarket": ("s", "white", "#B9861C", 17),
        "Park": ("o", "white", "#4E8A5B", 17),
        "Station": ("v", "#2B7184", "white", 22),
        "Rail station": ("D", "#173F5F", "white", 24),
        "Bus stop": ("o", "#6EA2BE", "white", 6.0),
    }
    plotted = {}
    for facility, grp in local_pois.groupby("facility"):
        if facility not in facility_styles:
            continue
        grp = grp.copy()
        if facility == "Bus stop":
            grp["_gx"] = (grp.geometry.x // 250).astype(int)
            grp["_gy"] = (grp.geometry.y // 250).astype(int)
            sample = grp.drop_duplicates(["_gx", "_gy"])
        elif facility == "Park":
            grp["_gx"] = (grp.geometry.x // 500).astype(int)
            grp["_gy"] = (grp.geometry.y // 500).astype(int)
            sample = grp.drop_duplicates(["_gx", "_gy"])
        elif facility == "Rail station":
            sample = grp.head(1)
        else:
            sample = grp.assign(_dist=grp.geometry.distance(origin)).sort_values("_dist").head(16)
        marker, face, edge, size = facility_styles[facility]
        ax_local.scatter(
            sample.geometry.x, sample.geometry.y, s=size, marker=marker,
            facecolors=face, edgecolors=edge,
            linewidths=0.50 if facility != "Bus stop" else 0,
            alpha=0.93, zorder=9,
        )
        plotted[facility] = sample
    ax_local.scatter([origin.x], [origin.y], s=25, marker="x", color="#202020",
                     linewidths=0.9, zorder=10)
    ax_local.annotate("model origin", xy=(origin.x, origin.y), xytext=(4, 4),
                      textcoords="offset points", fontsize=4.6, color="#444444", zorder=11)

    rail = plotted.get("Rail station")
    if rail is not None and not rail.empty:
        station = rail.iloc[0]
        ax_local.annotate(
            "Barking Riverside station",
            xy=(station.geometry.x, station.geometry.y), xytext=(5, 6),
            textcoords="offset points", fontsize=5.0, weight="bold", color="#244A6B",
            bbox=dict(boxstyle="round,pad=0.13", fc="white", ec="none", alpha=0.90),
            arrowprops=dict(arrowstyle="-", color="#244A6B", lw=0.55), zorder=11,
        )

    # Dedicated analytical zoom: the main map keeps the wider service context,
    # while this inset makes the nested 5/10/15-minute street reach and 100 m
    # score cells legible at printed A4 size.
    zxmin, zxmax = origin.x - 950, origin.x + 950
    zymin, zymax = origin.y - 650, origin.y + 650
    zoom_ax = ax_local.inset_axes([0.025, 0.650, 0.355, 0.310], zorder=20)
    zoom_ax.set_facecolor("#FAFBF9")
    local_lsoas.plot(ax=zoom_ax, color="#F8F8F5", edgecolor="#E5E5E0",
                     linewidth=0.10, zorder=1)
    for group, (colour, width, alpha) in road_specs.items():
        subset = roads[roads["road_group"].eq(group)]
        if not subset.empty:
            subset.plot(ax=zoom_ax, color=colour, linewidth=max(width * 0.90, 0.10),
                        alpha=min(alpha, 0.48), zorder=1.4)
    grid.plot(ax=zoom_ax, column="basket_score", cmap=access_cmap, vmin=0, vmax=65,
              edgecolor="#F8FAFA", linewidth=0.04, alpha=0.98, zorder=3)
    if edges is not None:
        _plot_reach(zoom_ax, reach_15, "#356AA0", 1.28, (0, (5, 2, 1, 2)), 4)
        _plot_reach(zoom_ax, reach_10, "#008F87", 1.40, (0, (3, 1.8)), 5)
        _plot_reach(zoom_ax, reach_5, "#F28E2B", 1.55, "solid", 6)
    lowest.boundary.plot(ax=zoom_ax, color="#202020", linewidth=1.10,
                         linestyle=(0, (3.5, 2.2)), zorder=7)
    representative.boundary.plot(ax=zoom_ax, color="#5D3D82", linewidth=1.05, zorder=8)
    zoom_ax.scatter([origin.x], [origin.y], s=22, marker="x", color="#202020",
                    linewidths=0.95, zorder=10)
    zoom_ax.set_xlim(zxmin, zxmax); zoom_ax.set_ylim(zymin, zymax)
    zoom_ax.set_aspect("equal"); zoom_ax.set_xticks([]); zoom_ax.set_yticks([])
    for spine in zoom_ax.spines.values():
        spine.set_visible(True); spine.set_color("#4C4C49"); spine.set_linewidth(0.72)
    zoom_ax.text(0.025, 0.965, "Zoom: local walking evidence", transform=zoom_ax.transAxes,
                 fontsize=5.3, weight="bold", va="top", ha="left",
                 bbox=dict(boxstyle="square,pad=0.12", fc="white", ec="none", alpha=0.90))
    ax_local.add_patch(Rectangle(
        (zxmin, zymin), zxmax-zxmin, zymax-zymin, fill=False,
        edgecolor="#8F8F8A", linewidth=0.42, linestyle=(0, (1.5, 2.2)), zorder=8.5,
    ))

    ax_local.set_xlim(xmin, xmax); ax_local.set_ylim(ymin, ymax); ax_local.set_aspect("equal")
    ax_local.set_xticks([]); ax_local.set_yticks([])
    for spine in ax_local.spines.values():
        spine.set_visible(True); spine.set_color("#777773"); spine.set_linewidth(0.55)
    sx, sy = xmin + 0.055 * map_width, ymin + 0.065 * map_height
    ax_local.plot([sx, sx + 1000], [sy, sy], color="#202020", lw=0.90, zorder=12)
    ax_local.plot([sx, sx], [sy - 22, sy + 22], color="#202020", lw=0.65, zorder=12)
    ax_local.plot([sx + 1000, sx + 1000], [sy - 22, sy + 22], color="#202020", lw=0.65, zorder=12)
    ax_local.text(sx + 500, sy - 55, "1 km", ha="center", va="top", fontsize=4.9, zorder=12)
    nx0, ny0 = xmin + 0.675 * map_width, ymin + 0.115 * map_height
    ax_local.annotate("", xy=(nx0, ny0 + 255), xytext=(nx0, ny0 - 85),
                      arrowprops=dict(arrowstyle="-|>", color="#202020", lw=0.85), zorder=12)
    ax_local.text(nx0, ny0 + 285, "N", ha="center", va="bottom", fontsize=5.4,
                  weight="bold", zorder=12)

    # A compact two-part legend preserves the older boxed-map look while making
    # the accessibility and facilities evidence explicit at A4 size.
    access_ax = ax_local.inset_axes([0.715, 0.710, 0.265, 0.260], zorder=20)
    access_ax.set_facecolor((1, 1, 1, 0.94)); access_ax.set_xlim(0, 1); access_ax.set_ylim(0, 1)
    access_ax.set_xticks([]); access_ax.set_yticks([])
    for spine in access_ax.spines.values():
        spine.set_visible(True); spine.set_color("#C8C6C0"); spine.set_linewidth(0.50)
    access_ax.text(0.07, 0.93, "Accessibility evidence", fontsize=5.6, weight="bold", va="top")
    cbar_ax = access_ax.inset_axes([0.09, 0.57, 0.50, 0.12])
    cb = fig.colorbar(
        mpl.cm.ScalarMappable(norm=mpl.colors.Normalize(0, 65), cmap=access_cmap),
        cax=cbar_ax, orientation="horizontal",
    )
    cb.set_ticks([0, 30, 60]); cb.ax.tick_params(labelsize=4.2, length=1.2, pad=1)
    cb.outline.set_linewidth(0.35)
    access_ax.text(0.09, 0.74, "100 m basket score", fontsize=4.9, va="bottom")
    walk_specs = [
        (0.43, "#F28E2B", "solid", "5 min"),
        (0.30, "#008F87", (0, (3, 1.8)), "10 min"),
        (0.17, "#356AA0", (0, (5, 2, 1, 2)), "15 min"),
    ]
    for y, colour, style, label in walk_specs:
        access_ax.plot([0.09, 0.34], [y, y], color=colour, lw=1.05, ls=style)
        access_ax.text(0.40, y, label, va="center", fontsize=4.8)
    access_ax.add_patch(Rectangle((0.66, 0.16), 0.08, 0.11, fill=False,
                                  edgecolor="#202020", linewidth=0.85,
                                  linestyle=(0, (3.5, 2.2))))
    access_ax.text(0.78, 0.215, "lowest 10%", fontsize=4.55, va="center")

    case_ax = ax_local.inset_axes([0.715, 0.442, 0.265, 0.215], zorder=20)
    case_ax.set_facecolor((1, 1, 1, 0.94)); case_ax.set_xlim(0, 1); case_ax.set_ylim(0, 1)
    case_ax.set_xticks([]); case_ax.set_yticks([])
    for spine in case_ax.spines.values():
        spine.set_visible(True); spine.set_color("#C8C6C0"); spine.set_linewidth(0.50)
    case_ax.text(0.06, 0.91, "Selected-area boundaries", fontsize=5.4,
                 weight="bold", va="top")
    boundary_specs = [
        (0.68, "#A83725", "solid", "Barking Riverside"),
        (0.49, "#087771", "solid", "Thames View"),
        (0.30, "#5D3D82", "solid", "Eastbury representative"),
        (0.11, "#6C6A66", (0, (3, 2)), "Beam / Parsloes edge"),
    ]
    for y, colour, style, label in boundary_specs:
        case_ax.plot([0.08, 0.26], [y, y], color=colour, lw=1.25, ls=style)
        case_ax.text(0.32, y, label, fontsize=4.55, va="center")

    facility_ax = ax_local.inset_axes([0.715, 0.055, 0.265, 0.360], zorder=20)
    facility_ax.set_facecolor((1, 1, 1, 0.94)); facility_ax.set_xlim(0, 1); facility_ax.set_ylim(0, 1)
    facility_ax.set_xticks([]); facility_ax.set_yticks([])
    for spine in facility_ax.spines.values():
        spine.set_visible(True); spine.set_color("#C8C6C0"); spine.set_linewidth(0.50)
    facility_ax.text(0.06, 0.94, "Facilities", fontsize=5.6, weight="bold", va="top")
    legend_facilities = [
        ("School", "Schools"), ("GP", "GPs"),
        ("Hospital", "Hospitals"), ("Pharmacy", "Pharmacies"),
        ("Supermarket", "Supermarkets"), ("Park", "Parks"),
        ("Station", "Underground/metro"), ("Rail station", "Rail station"),
        ("Bus stop", "Bus stops"),
    ]
    for i, (facility, label) in enumerate(legend_facilities):
        col, row = i % 2, i // 2
        x = 0.08 + col * 0.48
        y = 0.78 - row * 0.145
        marker, face, edge, _ = facility_styles[facility]
        facility_ax.scatter([x], [y], s=16 if facility != "Bus stop" else 7,
                            marker=marker, facecolors=face, edgecolors=edge,
                            linewidths=0.50)
        facility_ax.text(x + 0.08, y, label, fontsize=4.55, va="center")

    fig.text(0.025, 0.942, "a  United Kingdom", fontsize=7.0,
             weight="bold", ha="left", va="top", color="#202124")
    fig.text(0.025, 0.910, "Greater London highlighted", fontsize=5.3,
             ha="left", va="top", color="#666666")
    fig.text(0.025, 0.535, "b  East London context", fontsize=6.8,
             weight="bold", ha="left", va="top", color="#202124")
    fig.text(0.025, 0.505, "Selected-area composition and Newham counter-case", fontsize=4.9,
             ha="left", va="top", color="#666666")
    fig.text(0.240, 0.962, "c  Local accessibility environment", fontsize=8.3,
             weight="bold", ha="left", va="top", color="#202124")
    fig.text(0.240, 0.936,
             "Representative local mechanism: E01000096 (Eastbury best-fit ward)",
             fontsize=5.5, ha="left", va="top", color="#555555")
    fig.text(0.240, 0.916,
             "100 m basket score combines modelled walking access to everyday care-related services; higher values indicate broader/closer local access.",
             fontsize=4.75, ha="left", va="top", color="#666666")
    fig.text(0.025, 0.145,
             "E01000014 is a real discontinuous Parsloes/Goresbrook-edge LSOA, not a geometry error.",
             fontsize=4.45, color="#555555", ha="left")
    fig.text(0.025, 0.123,
             "Grey dashed boundaries retain Beam and Parsloes edge components.",
             fontsize=4.35, color="#666666", ha="left")

    qa_svg = ROOT / ".codex_work" / "figure12" / "figure12_case_accessibility_v8.svg"
    fig.savefig(qa_svg, facecolor="white")
    fig.savefig(FINAL_OUT / "figure12_case_accessibility_v8.png", dpi=600, facecolor="white")
    fig.savefig(FINAL_OUT / "figure12_case_accessibility_v8.pdf", facecolor="white")
    plt.close(fig)


def figure12_v10():
    """True Barking--Newham comparison with matched locator and local-map frames."""
    lsoas, boroughs, thames = load_base()
    neg = lsoas[lsoas["LSOA21CD"].isin(NEGATIVE_CASE)].copy()
    pos = lsoas[lsoas["LSOA21CD"].isin(POSITIVE_CASE)].copy()
    goresbrook = neg[neg["LSOA21CD"].eq("E01000014")].copy()
    barking_main = neg[~neg["LSOA21CD"].eq("E01000014")].copy()
    br_case = neg[neg["LSOA21CD"].isin(["E01000094", "E01000095", "E01034478"])].copy()
    thames_view_case = neg[neg["LSOA21CD"].eq("E01034476")].copy()
    barking_edge = neg[neg["LSOA21CD"].isin(["E01000090", "E01000096"])].copy()

    # Existing grid-model output only: no model rerun and no change to the
    # reported LSOA case statistics.  The threshold is the London-wide p10 for
    # the same caregiving-adult / walk-15-minute grid measure in both panels.
    grid_scores = pd.read_parquet(
        ROOT / "extension" / "data_intermediate" / "grid_accessibility_summary.parquet"
    )
    grid_scores = grid_scores[
        grid_scores["group"].eq("caregiving_adults")
        & grid_scores["mode"].eq("walk_15min")
    ][["grid_id", "lsoa21cd", "basket_score"]].copy()
    london_p10 = float(grid_scores["basket_score"].quantile(0.10))
    grid_geometry = gpd.read_file(GRID_GPKG)[["grid_id", "geometry"]].to_crs(27700)
    grid_all = grid_geometry.merge(grid_scores, on="grid_id", how="inner")
    grid_barking = grid_all[grid_all["lsoa21cd"].isin(NEGATIVE_CASE)].copy()
    grid_newham = grid_all[grid_all["lsoa21cd"].isin(POSITIVE_CASE)].copy()

    access_cmap = LinearSegmentedColormap.from_list(
        "comparison_access_v9",
        ["#F4F8F8", "#C6E1E5", "#82BAC5", "#3D8197", "#123F63"],
    )
    facility_styles = {
        "School": ("p", "white", "#D07A2D", 19),
        "GP": ("P", "#C34F4A", "white", 19),
        "Hospital": ("s", "#A43D79", "white", 22),
        "Pharmacy": ("h", "white", "#7B3F72", 19),
        "Supermarket": ("s", "white", "#B9861C", 16),
        "Park": ("o", "white", "#4E8A5B", 16),
        "Station": ("v", "#2B7184", "white", 21),
        "Rail station": ("D", "#173F5F", "white", 22),
        "Bus stop": ("o", "#6EA2BE", "white", 5.5),
    }

    # Add station locations from the existing TfL accessibility files.  These
    # are used as map symbols only and do not alter accessibility calculations.
    station_meta = pd.read_csv(ROOT / "data" / "tfl_step_free" / "detailed" / "Stations.csv")
    station_points = pd.read_csv(ROOT / "data" / "tfl_step_free" / "detailed" / "StationPoints.csv")
    station_xy = (
        station_points.dropna(subset=["Lat", "Lon"])
        .groupby("StationUniqueId", as_index=False)[["Lat", "Lon"]].median()
        .merge(station_meta[["UniqueId", "Name"]], left_on="StationUniqueId", right_on="UniqueId", how="left")
    )
    rail_stations = gpd.GeoDataFrame(
        station_xy.rename(columns={"Name": "name"}),
        geometry=gpd.points_from_xy(station_xy["Lon"], station_xy["Lat"]), crs=4326,
    ).to_crs(27700)
    rail_stations["facility"] = "Rail station"
    rail_stations["plot_color"] = "#173F5F"
    rail_stations["marker"] = "D"
    rail_stations = rail_stations[["facility", "name", "plot_color", "marker", "geometry"]]
    pois = load_pois(27700)
    pois = gpd.GeoDataFrame(
        pd.concat([pois, rail_stations], ignore_index=True), geometry="geometry", crs=27700
    )

    map_width, map_height = 6500.0, 2700.0

    def matched_bounds(area):
        axmin, aymin, axmax, aymax = area.total_bounds
        cx, cy = (axmin + axmax) / 2, (aymin + aymax) / 2
        return (cx - map_width/2, cy - map_height/2,
                cx + map_width/2, cy + map_height/2)

    barking_bounds = matched_bounds(barking_main)
    newham_bounds = matched_bounds(pos)
    barking_context = load_detail_context(barking_bounds, "barking_v9")
    newham_context = load_detail_context(newham_bounds, "newham_v9")

    # Reference points are deterministic representative points of each selected
    # cluster, not newly selected analytical cases.
    # Thames View provides a residential, well-connected reference point inside
    # the named Barking Riverside / Thames View case, avoiding an industrial
    # parcel whose nearest main walk-network component is almost 1 km away.
    barking_origin = thames_view_case.geometry.iloc[0].centroid
    newham_origin = pos.geometry.unary_union.representative_point()

    def network_for(origin):
        origin_area = gpd.GeoDataFrame(geometry=[origin], crs=27700)
        graph, nodes, edges = graph_edges_for_case(origin_area)
        return (
            edges,
            reachable_edges(graph, nodes, edges, origin, 5 * WALK_METRES_PER_MIN),
            reachable_edges(graph, nodes, edges, origin, 10 * WALK_METRES_PER_MIN),
            reachable_edges(graph, nodes, edges, origin, 15 * WALK_METRES_PER_MIN),
        )

    barking_network = network_for(barking_origin)
    newham_network = network_for(newham_origin)

    fig = plt.figure(figsize=(7.35, 6.25), facecolor="white")
    map_note_fs = 4.45
    # Each row uses equal-height map frames; the left panels remain subordinate
    # through their narrower width, not through reduced height.
    ax_uk = fig.add_axes([0.025, 0.565, 0.190, 0.350])
    ax_context = fig.add_axes([0.025, 0.145, 0.190, 0.350])
    # The two analytical panels also use identical physical and geographic frames.
    ax_barking = fig.add_axes([0.235, 0.565, 0.740, 0.350])
    ax_newham = fig.add_axes([0.235, 0.145, 0.740, 0.350])
    legend_ax = fig.add_axes([0.235, 0.025, 0.740, 0.095])

    countries = gpd.read_file(
        BOUNDARIES / "Countries_December_2022_Boundaries_UK_BGC.gpkg"
    ).to_crs(27700)
    countries.plot(ax=ax_uk, facecolor="#D9DEDB", edgecolor="#A7AFAB", linewidth=0.38)
    london_point = gpd.GeoSeries([lsoas.geometry.unary_union], crs=27700).representative_point().iloc[0]
    ax_uk.scatter([london_point.x], [london_point.y], s=28, facecolor="#E66A4A",
                  edgecolor="#8E271C", linewidth=0.70, zorder=5)
    uxmin, uymin, uxmax, uymax = countries.total_bounds
    uw, uh = uxmax-uxmin, uymax-uymin
    uk_bounds = (uxmin-0.04*uw, uymin-0.02*uh, uxmax+0.04*uw, uymax+0.03*uh)
    ax_uk.set_xlim(uk_bounds[0], uk_bounds[2]); ax_uk.set_ylim(uk_bounds[1], uk_bounds[3])
    add_north_scale(ax_uk, uk_bounds, scale_m=200000)
    ax_uk.set_axis_off(); ax_uk.set_aspect("equal"); ax_uk.set_anchor("C")
    ax_uk.add_patch(Rectangle((0, 0), 1, 1, transform=ax_uk.transAxes, fill=False,
                              edgecolor="none", linewidth=0, clip_on=False))

    context_union = unary_union([neg.geometry.unary_union, pos.geometry.unary_union])
    exmin, eymin, exmax, eymax = gpd.GeoSeries([context_union], crs=27700).buffer(1900).total_bounds
    lsoas.cx[exmin:exmax, eymin:eymax].plot(
        ax=ax_context, color="#F3F4F1", edgecolor="#FFFFFF", linewidth=0.05
    )
    boroughs.cx[exmin:exmax, eymin:eymax].boundary.plot(
        ax=ax_context, color="#AEB3AF", linewidth=0.25
    )
    thames.cx[exmin:exmax, eymin:eymax].plot(
        ax=ax_context, color="#8FC7D8", linewidth=0.90, alpha=0.95
    )
    barking_main.plot(ax=ax_context, facecolor="#E66A4A", edgecolor="#A83725",
                      linewidth=0.85, alpha=0.62, zorder=6)
    goresbrook.plot(ax=ax_context, facecolor="#D9D6CF", edgecolor="#55524E",
                    linewidth=0.85, linestyle=(0, (3, 2)), alpha=0.78, zorder=6)
    pos.plot(ax=ax_context, facecolor="#4C91C6", edgecolor="#225C87",
             linewidth=0.85, alpha=0.60, zorder=6)
    bxy = barking_main.geometry.unary_union.centroid
    pxy = pos.geometry.unary_union.centroid
    gxy = goresbrook.geometry.iloc[0].centroid
    ax_context.annotate(
        "Barking Riverside /\nThames View case", xy=(bxy.x, bxy.y),
        xytext=(exmin+0.49*(exmax-exmin), eymin+0.045*(eymax-eymin)),
        fontsize=map_note_fs, weight="bold", color="#8E271C", ha="left",
        arrowprops=dict(arrowstyle="-", color="#8E271C", lw=0.58),
    )
    ax_context.annotate(
        "Newham\ncounter-case", xy=(pxy.x, pxy.y),
        xytext=(exmin+0.035*(exmax-exmin), eymax-0.16*(eymax-eymin)),
        fontsize=map_note_fs, weight="bold", color="#225C87", ha="left", va="top",
        arrowprops=dict(arrowstyle="-", color="#225C87", lw=0.58),
    )
    ax_context.annotate(
        "E01000014\nreal discontinuous component", xy=(gxy.x, gxy.y),
        xytext=(exmin+0.45*(exmax-exmin), eymax-0.09*(eymax-eymin)),
        fontsize=map_note_fs, color="#55524E", ha="left", va="top",
        bbox=dict(boxstyle="round,pad=0.12", fc="white", ec="none", alpha=0.72),
        arrowprops=dict(arrowstyle="->", color="#55524E", lw=0.55),
    )
    ax_context.set_xlim(exmin, exmax); ax_context.set_ylim(eymin, eymax)
    add_north_scale(ax_context, (exmin, eymin, exmax, eymax), scale_m=5000)
    ax_context.set_axis_off(); ax_context.set_aspect("equal", adjustable="datalim"); ax_context.set_anchor("C")
    ax_context.add_patch(Rectangle((0, 0), 1, 1, transform=ax_context.transAxes,
                                   fill=False, edgecolor="none", linewidth=0,
                                   clip_on=False))
    # Fixed figure-coordinate frames remain exactly equal even though the UK
    # and East London geographies have different aspect ratios.
    for x, y, w, h in [(0.025, 0.565, 0.190, 0.350), (0.025, 0.145, 0.190, 0.350)]:
        fig.add_artist(Rectangle((x, y), w, h, transform=fig.transFigure,
                                 fill=False, edgecolor="#777773", linewidth=0.62,
                                 clip_on=False, zorder=30))

    road_specs = {
        "Local street": ("#D4D4CF", 0.12, 0.34),
        "Connector road": ("#C1C0BB", 0.20, 0.41),
        "Major road": ("#A9AAA5", 0.32, 0.48),
    }

    def plot_reach(ax, layer, colour, width, style, zorder):
        before = len(ax.collections)
        layer.plot(ax=ax, color=colour, linewidth=width, linestyle=style,
                   alpha=0.98, zorder=zorder)
        for collection in ax.collections[before:]:
            collection.set_path_effects([
                pe.Stroke(linewidth=width+1.05, foreground="white", alpha=0.94), pe.Normal()
            ])

    def plot_local_case(ax, bounds, case_grid, selected_area, context_layers,
                        network_layers, origin, boundary_mode):
        xmin, ymin, xmax, ymax = bounds
        landuse, buildings, roads = context_layers
        lsoas.cx[xmin:xmax, ymin:ymax].plot(
            ax=ax, color="#FAFBF8", edgecolor="#E8E9E4", linewidth=0.09, zorder=1
        )
        if not landuse.empty:
            landuse.plot(ax=ax, color="#E8EEE3", edgecolor="none", alpha=0.28, zorder=1.1)
        if not buildings.empty:
            buildings.plot(ax=ax, color="#AAB8BF", edgecolor="none", alpha=0.34, zorder=1.3)
        for group, (colour, width, alpha) in road_specs.items():
            subset = roads[roads["road_group"].eq(group)]
            if not subset.empty:
                subset.plot(ax=ax, color=colour, linewidth=width, alpha=alpha, zorder=1.5)
        local_thames = thames.cx[xmin:xmax, ymin:ymax]
        if not local_thames.empty:
            local_thames.plot(
                ax=ax, color="#8FC7D8", linewidth=1.05, alpha=0.95, zorder=1.7
            )
        case_grid.plot(ax=ax, column="basket_score", cmap=access_cmap, vmin=0, vmax=80,
                       edgecolor="#F8FAFA", linewidth=0.035, alpha=0.97, zorder=3)
        edges, reach5, reach10, reach15 = network_layers
        if edges is not None:
            plot_reach(ax, reach15, "#356AA0", 0.95, (0, (5, 2, 1, 2)), 4)
            plot_reach(ax, reach10, "#008F87", 1.06, (0, (3, 1.8)), 5)
            plot_reach(ax, reach5, "#F28E2B", 1.20, "solid", 6)
        low = case_grid[case_grid["basket_score"].le(london_p10)]
        if not low.empty:
            low.boundary.plot(ax=ax, color="#6656B3", linewidth=1.00,
                              linestyle=(0, (4, 1.4, 1, 1.4)), zorder=7)
        if boundary_mode == "barking":
            br_case.boundary.plot(ax=ax, color="#A83725", linewidth=0.90, zorder=7.5)
            thames_view_case.boundary.plot(ax=ax, color="#087771", linewidth=0.90, zorder=7.5)
            barking_edge.boundary.plot(ax=ax, color="#6C6A66", linewidth=0.72,
                                       linestyle=(0, (3, 2)), zorder=7.5)
        else:
            selected_area.boundary.plot(ax=ax, color="#225C87", linewidth=0.90, zorder=7.5)

        local_pois = pois.cx[xmin:xmax, ymin:ymax].copy()
        for facility, grp in local_pois.groupby("facility"):
            if facility not in facility_styles:
                continue
            grp = grp.copy()
            if facility == "Bus stop":
                grp["_gx"] = (grp.geometry.x // 400).astype(int)
                grp["_gy"] = (grp.geometry.y // 400).astype(int)
                sample = grp.drop_duplicates(["_gx", "_gy"])
            elif facility == "Park":
                grp["_gx"] = (grp.geometry.x // 600).astype(int)
                grp["_gy"] = (grp.geometry.y // 600).astype(int)
                sample = grp.drop_duplicates(["_gx", "_gy"])
            elif facility == "Hospital":
                # OSM may list departments/wards as separate hospital POIs on
                # one campus. Keep the raw records, but plot one symbol per
                # 500 m cell so a campus is not misread as several hospitals.
                grp["_gx"] = (grp.geometry.x // 500).astype(int)
                grp["_gy"] = (grp.geometry.y // 500).astype(int)
                sample = (
                    grp.assign(_dist=grp.geometry.distance(origin))
                    .sort_values("_dist")
                    .drop_duplicates(["_gx", "_gy"])
                )
            elif facility in ("Station", "Rail station"):
                sample = grp.drop_duplicates("name").assign(
                    _dist=grp.drop_duplicates("name").geometry.distance(origin)
                ).sort_values("_dist").head(3)
            else:
                sample = grp.assign(_dist=grp.geometry.distance(origin)).sort_values("_dist").head(8)
            marker, face, edge, size = facility_styles[facility]
            ax.scatter(sample.geometry.x, sample.geometry.y, s=size, marker=marker,
                       facecolors=face, edgecolors=edge,
                       linewidths=0.48 if facility != "Bus stop" else 0.18,
                       alpha=0.92, zorder=9)
        ax.scatter([origin.x], [origin.y], s=23, marker="x", color="#202020",
                   linewidths=0.95, zorder=10)
        origin_label = ax.annotate(
            "illustrative origin", xy=(origin.x, origin.y), xytext=(9, 9),
            textcoords="offset points", fontsize=map_note_fs, color="#444444", zorder=11,
        )
        origin_label.set_path_effects([
            pe.withStroke(linewidth=2.8, foreground="white"), pe.Normal()
        ])
        if boundary_mode == "barking":
            selection_note = ax.text(
                0.015, 0.965,
                "6 of 7 selected LSOAs shown; E01000014 is retained in panel b.",
                transform=ax.transAxes, ha="left", va="top", fontsize=map_note_fs,
                color="#404040", zorder=13,
            )
            selection_note.set_path_effects([
                pe.withStroke(linewidth=2.5, foreground="white"), pe.Normal()
            ])
            cell_note = ax.text(
                0.785, 0.105,
                "Residential 100 m cells only;\nblank interiors are non-residential.",
                transform=ax.transAxes, ha="center", va="center", fontsize=map_note_fs,
                color="#555555", zorder=13,
            )
            cell_note.set_path_effects([
                pe.withStroke(linewidth=2.5, foreground="white"), pe.Normal()
            ])
        ax.set_xlim(xmin, xmax); ax.set_ylim(ymin, ymax); ax.set_aspect("equal")
        ax.set_xticks([]); ax.set_yticks([])
        for spine in ax.spines.values():
            spine.set_visible(True); spine.set_color("#777773"); spine.set_linewidth(0.62)
        sx, sy = xmin+0.035*map_width, ymin+0.075*map_height
        ax.plot([sx, sx+1000], [sy, sy], color="#202020", lw=0.85, zorder=12)
        ax.plot([sx, sx], [sy-18, sy+18], color="#202020", lw=0.60, zorder=12)
        ax.plot([sx+1000, sx+1000], [sy-18, sy+18], color="#202020", lw=0.60, zorder=12)
        ax.text(sx+500, sy-38, "1 km", ha="center", va="top", fontsize=4.7, zorder=12)
        nx0, ny0 = xmin+0.955*map_width, ymin+0.765*map_height
        ax.annotate("", xy=(nx0, ny0+210), xytext=(nx0, ny0-70),
                    arrowprops=dict(arrowstyle="-|>", color="#202020", lw=0.78), zorder=12)
        ax.text(nx0, ny0+235, "N", ha="center", va="bottom", fontsize=5.0,
                weight="bold", zorder=12)

    plot_local_case(ax_barking, barking_bounds, grid_barking, barking_main,
                    barking_context, barking_network, barking_origin, "barking")
    plot_local_case(ax_newham, newham_bounds, grid_newham, pos,
                    newham_context, newham_network, newham_origin, "newham")

    # Shared legend guarantees identical visual semantics in the two maps.
    legend_ax.set_facecolor("white"); legend_ax.set_xlim(0, 1); legend_ax.set_ylim(0, 1)
    legend_ax.set_xticks([]); legend_ax.set_yticks([])
    for spine in legend_ax.spines.values():
        spine.set_visible(True); spine.set_color("#C8C6C0"); spine.set_linewidth(0.50)
    legend_ax.text(0.015, 0.88, "Shared key", fontsize=5.4, weight="bold", va="top")
    legend_ax.text(0.110, 0.88, "100 m basket score — shared 0–80 scale",
                   fontsize=4.35, va="top")
    cbar_ax = legend_ax.inset_axes([0.110, 0.56, 0.205, 0.18])
    cb = fig.colorbar(
        mpl.cm.ScalarMappable(norm=mpl.colors.Normalize(0, 80), cmap=access_cmap),
        cax=cbar_ax, orientation="horizontal",
    )
    cb.set_ticks([0, 40, 80]); cb.ax.tick_params(labelsize=4.1, length=1.2, pad=1)
    cb.outline.set_linewidth(0.35)
    walking = [
        (0.365, "#F28E2B", "solid", "5"),
        (0.435, "#008F87", (0, (3, 1.8)), "10"),
        (0.505, "#356AA0", (0, (5, 2, 1, 2)), "15 min"),
    ]
    for x, colour, style, label in walking:
        legend_ax.plot([x, x+0.045], [0.68, 0.68], color=colour, lw=1.15, ls=style)
        legend_ax.text(x+0.052, 0.68, label, fontsize=4.4, va="center")
    legend_ax.plot([0.605, 0.650], [0.68, 0.68], color="#6656B3", lw=1.00,
                   ls=(0, (4, 1.4, 1, 1.4)))
    legend_ax.text(0.657, 0.68, f"London lowest 10% (≤{london_p10:.1f})",
                   fontsize=4.35, va="center")
    boundary_keys = [
        (0.015, "#A83725", "Barking Riverside"),
        (0.145, "#087771", "Thames View"),
        (0.255, "#6C6A66", "other Barking selected"),
        (0.415, "#225C87", "Newham selected"),
    ]
    for x, colour, label in boundary_keys:
        style = (0, (3, 2)) if label == "other Barking selected" else "solid"
        legend_ax.plot([x, x+0.032], [0.31, 0.31], color=colour, lw=1.10, ls=style)
        legend_ax.text(x+0.038, 0.31, label, fontsize=4.3, va="center")
    facility_labels = [
        ("School", "School"), ("GP", "GP"), ("Hospital", "Hospital"),
        ("Pharmacy", "Pharmacy"), ("Supermarket", "Supermarket"),
        ("Park", "Park"), ("Station", "Metro"),
        ("Rail station", "Rail"), ("Bus stop", "Bus"),
    ]
    legend_ax.text(0.595, 0.31, "Facilities", fontsize=4.35, weight="bold", va="center")
    for i, (facility, label) in enumerate(facility_labels):
        x = 0.070 + i*0.103
        marker, face, edge, _ = facility_styles[facility]
        legend_ax.scatter([x], [0.075], s=15 if facility != "Bus stop" else 6,
                          marker=marker, facecolors=face, edgecolors=edge, linewidths=0.45)
        legend_ax.text(x+0.012, 0.075, label, fontsize=4.4, ha="left", va="center")

    fig.text(0.025, 0.955, "a  United Kingdom", fontsize=7.4, weight="bold",
             ha="left", va="top", color="#202124")
    fig.text(0.025, 0.930, "Greater London highlighted", fontsize=5.2,
             ha="left", va="top", color="#666666")
    fig.text(0.025, 0.540, "b  East London case context", fontsize=7.4, weight="bold",
             ha="left", va="top", color="#202124")
    fig.text(0.025, 0.518, "Barking (7 LSOAs) / Newham (4 LSOAs)",
             fontsize=4.8, ha="left", va="top", color="#666666")
    fig.text(0.235, 0.960, "c  Barking Riverside / Thames View case", fontsize=7.4,
             weight="bold", ha="left", va="top", color="#202124")
    fig.text(0.235, 0.938,
             "Main six-LSOA cluster; E01000014 in b | lower scores: fewer or less-near weighted everyday services within 15 min",
             fontsize=5.0, ha="left", va="top", color="#666666")
    fig.text(0.235, 0.540, "d  Newham counter-case", fontsize=7.4,
             weight="bold", ha="left", va="top", color="#202124")
    fig.text(0.235, 0.518,
             "Four selected LSOAs | higher scores: more or nearer weighted everyday services within 15 min | matched frame",
             fontsize=5.0, ha="left", va="top", color="#666666")

    qa_svg = ROOT / ".codex_work" / "figure12" / "figure12_case_accessibility_v10.svg"
    fig.savefig(qa_svg, facecolor="white")
    fig.savefig(FINAL_OUT / "figure12_case_accessibility_v10.png", dpi=600, facecolor="white")
    fig.savefig(FINAL_OUT / "figure12_case_accessibility_v10.pdf", facecolor="white")
    plt.close(fig)


def appendix_a_step_free():
    """Redraw Appendix A with deterministic point thinning for A4 legibility."""
    lsoas, boroughs, thames = load_base()
    older = pd.read_csv(ANALYSIS / "continuous_need_access_mismatch_indices.csv")
    care = pd.read_csv(ANALYSIS / "care_mobility_accessibility_lsoa.csv")
    m = older[["LSOA21CD", "older_mismatch_index"]].merge(
        care[["LSOA21CD", "care_mismatch_index"]], on="LSOA21CD", how="outer"
    )
    oq = m["older_mismatch_index"].quantile(0.75)
    cq = m["care_mismatch_index"].quantile(0.75)
    m["class"] = np.select(
        [
            m["older_mismatch_index"].ge(oq) & m["care_mismatch_index"].ge(cq),
            m["older_mismatch_index"].ge(oq),
            m["care_mismatch_index"].ge(cq),
        ],
        ["High older + care mismatch", "High older mismatch", "High care mismatch"],
        default="Other LSOAs",
    )
    map_gdf = lsoas.merge(m[["LSOA21CD", "class"]], on="LSOA21CD", how="left")

    detail = ROOT / "data" / "tfl_step_free" / "detailed"
    stations = pd.read_csv(detail / "Stations.csv")
    points = pd.read_csv(detail / "StationPoints.csv")
    platforms = pd.read_csv(detail / "Platforms.csv")
    p = platforms[platforms["IsCustomerFacing"].astype(str).str.lower().eq("true")].copy()
    p["step"] = p["HasStepFreeRouteInformation"].astype(str).str.lower().eq("true")
    ps = p.groupby("StationUniqueId").agg(total=("step", "size"), step=("step", "sum")).reset_index()
    ps["coverage"] = np.select(
        [(ps["total"].gt(0) & ps["step"].eq(ps["total"])), ps["step"].gt(0)],
        ["Full recorded route coverage", "Partial recorded route coverage"],
        default="No recorded platform route info",
    )
    xy = points.dropna(subset=["Lat", "Lon"]).groupby("StationUniqueId")[["Lat", "Lon"]].mean().reset_index()
    st = stations[["UniqueId", "Name"]].rename(columns={"UniqueId": "StationUniqueId"}).merge(xy).merge(ps, how="left")
    st["coverage"] = st["coverage"].fillna("No recorded platform route info")
    st = gpd.GeoDataFrame(st, geometry=gpd.points_from_xy(st["Lon"], st["Lat"]), crs=4326).to_crs(27700)
    london = lsoas.geometry.unary_union.buffer(2500)
    st = st[st.geometry.within(london)].copy()
    priority = {"Full recorded route coverage": 0, "Partial recorded route coverage": 1,
                "No recorded platform route info": 2}
    st["priority"] = st["coverage"].map(priority)
    st["cell_x"] = (st.geometry.x // 750).astype(int)
    st["cell_y"] = (st.geometry.y // 750).astype(int)
    shown = st.sort_values(["priority", "Name"]).drop_duplicates(["cell_x", "cell_y"], keep="first")

    fill = {"Other LSOAs": "#F2F1ED", "High older mismatch": "#D9B486",
            "High care mismatch": "#E7A6A0", "High older + care mismatch": "#B95558"}
    dot = {"Full recorded route coverage": "#006D77", "Partial recorded route coverage": "#E09F3E",
           "No recorded platform route info": "#8D4A50"}
    fig, ax = plt.subplots(figsize=(7.15, 5.15))
    map_gdf.plot(ax=ax, color=map_gdf["class"].map(fill), linewidth=0, alpha=0.82)
    boroughs.boundary.plot(ax=ax, color="#FFFFFF", linewidth=0.18, alpha=0.9)
    thames.plot(ax=ax, color="#9EC5D3", linewidth=0.75, alpha=0.95)
    for cls in ["No recorded platform route info", "Partial recorded route coverage", "Full recorded route coverage"]:
        q = shown[shown["coverage"].eq(cls)]
        ax.scatter(q.geometry.x, q.geometry.y, s=12 if cls.startswith("Full") else 8,
                   c=dot[cls], alpha=0.68, linewidths=0, label=cls, zorder=5)
    # Title and subtitle are both placed in axes coordinates. set_title's pad is
    # measured in points, which does not clear the subtitle reliably once
    # set_axis_off() and aspect="equal" have resized the map frame.
    ax.text(0, 1.005, "High older/care mismatch and recorded TfL station route coverage",
            transform=ax.transAxes, fontsize=8.4, color=COLORS["muted"], va="bottom")
    ax.text(0, 1.045, "Exploratory step-free usability mechanism",
            transform=ax.transAxes, fontsize=12.5, weight="bold", color=COLORS["ink"], va="bottom")
    fill_handles = [Line2D([0],[0], marker="s", color="none", markerfacecolor=fill[k], markersize=7, label=k)
                    for k in fill]
    dot_handles = [Line2D([0],[0], marker="o", color="none", markerfacecolor=dot[k], markersize=5.5, label=k)
                   for k in dot]
    leg1 = ax.legend(handles=fill_handles, title="Mismatch areas", loc="lower left", frameon=True,
                     fontsize=7, title_fontsize=7.3, facecolor="white", edgecolor="#DDDDDD")
    ax.add_artist(leg1)
    ax.legend(handles=dot_handles, title="TfL station topology", loc="lower right", frameon=True,
              fontsize=7, title_fontsize=7.3, facecolor="white", edgecolor="#DDDDDD")
    ax.text(0.0, -0.035, "Display points are thinned to one station per 750 m cell for legibility; Appendix Table A1 uses the full station set.",
            transform=ax.transAxes, fontsize=7, color=COLORS["muted"], ha="left", va="top")
    ax.set_axis_off(); ax.set_aspect("equal")
    fig.subplots_adjust(left=0.015, right=0.985, top=0.92, bottom=0.06)
    fig.savefig(FINAL_OUT / "appendix_figure_A1_revised.png", dpi=600, bbox_inches="tight", facecolor="white")
    plt.close(fig)


def formula_block():
    fig = plt.figure(figsize=(7.1, 3.25))
    ax = fig.add_axes([0, 0, 1, 1])
    ax.set_axis_off()
    # Matplotlib mathtext does not support the LaTeX cases environment, so the
    # piecewise equation is assembled as separate math fragments.
    ax.text(0.05, 0.86, r"$(1)\quad S_{iks}=$", fontsize=14, color=COLORS["ink"], va="center")
    ax.text(0.25, 0.86, "{", fontsize=46, color=COLORS["ink"], va="center")
    ax.text(0.315, 0.915, r"$100\left(1-\frac{T_{iks}}{\tau_s}\right),\quad T_{iks}\leq \tau_s$",
            fontsize=13.5, color=COLORS["ink"], va="center")
    ax.text(0.315, 0.805, r"$0,\quad T_{iks}>\tau_s$",
            fontsize=13.5, color=COLORS["ink"], va="center")

    formulas = [
        (0.66, r"$(2)\quad A_{igs}=\sum_k w_{gk}S_{iks}$"),
        (0.49, r"$(3)\quad N_{ig}=\sum_m \alpha_{gm}X_{im}$"),
        (0.32, r"$(4)\quad M_{igs}=N_{ig}-A_{igs}$"),
        (0.17, r"$(5)\quad G_{ig}=A_{ig,PT30}-A_{ig,W15}$"),
    ]
    for y, f in formulas:
        ax.text(0.05, y, f, fontsize=13.2, color=COLORS["ink"], va="center")
    ax.text(0.05, 0.06,
            "Note: scores are scaled 0-100; positive mismatch indicates that population need exceeds modelled accessibility.",
            fontsize=8.3, color=COLORS["muted"], va="bottom")
    fig.savefig(OUT / "methodology_equations_1_5_revised.png", bbox_inches="tight", facecolor="white")
    fig.savefig(OUT / "methodology_equations_1_5_revised.pdf", bbox_inches="tight", facecolor="white")
    plt.close(fig)


def table3_image():
    rows = [
        ["Schools", "0.30", "0.15", "0.03", "education / escorting"],
        ["Supermarkets", "0.08", "0.15", "0.10", "daily food-shopping"],
        ["Parks", "0.20", "0.08", "0.16", "play, recreation, wellbeing"],
        ["GPs", "0.14", "0.10", "0.24", "primary healthcare need"],
        ["Pharmacies", "0.10", "0.12", "0.22", "medication and routine care"],
        ["Bus stops", "0.10", "0.18", "0.14", "local network entry"],
        ["Underground/metro", "0.04", "0.16", "0.06", "cross-city network entry"],
        ["Hospitals", "0.04", "0.06", "0.05", "episodic specialist access"],
    ]
    cols = ["Facility", "Children", "Caregiving\nadults", "Older\nadults", "Rationale"]
    fig, ax = plt.subplots(figsize=(7.15, 3.35))
    ax.axis("off")
    tbl = ax.table(cellText=rows, colLabels=cols, loc="center", cellLoc="left", colLoc="left",
                   colWidths=[0.20, 0.13, 0.22, 0.16, 0.29])
    tbl.auto_set_font_size(False)
    tbl.set_fontsize(7.9)
    tbl.scale(1, 1.18)
    for (r, c), cell in tbl.get_celld().items():
        cell.visible_edges = ""
        cell.set_facecolor("white")
        cell.PAD = 0.035
        if r == 0:
            cell.set_text_props(weight="bold", color=COLORS["ink"])
            cell.visible_edges = "TB"
            cell.set_linewidth(0.9)
            cell.set_height(cell.get_height() * 1.65)
        elif r == len(rows):
            cell.visible_edges = "B"
            cell.set_linewidth(0.9)
        else:
            cell.set_height(cell.get_height() * 1.03)
        if c in [1, 2, 3] and r > 0:
            cell.set_text_props(ha="right")
    ax.text(0, 1.05, "Table 3. Service-basket weights and concise rationale",
            transform=ax.transAxes, ha="left", va="bottom", fontsize=10.5, fontweight="bold", color=COLORS["ink"])
    ax.text(0, -0.08,
            "Note: weights sum to 1.00 within each group. Rationale condenses the literature basis; full citations are discussed in the methodology text.",
            transform=ax.transAxes, ha="left", va="top", fontsize=7.7, color=COLORS["muted"])
    fig.savefig(OUT / "table3_service_basket_weights_revised.png", bbox_inches="tight", facecolor="white")
    fig.savefig(OUT / "table3_service_basket_weights_revised.pdf", bbox_inches="tight", facecolor="white")
    plt.close(fig)


def table10_image():
    fig, ax = plt.subplots(figsize=(7.15, 3.0))
    ax.axis("off")
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.text(0, 1.08, "Table 10. Negative and positive care-access case comparison",
            transform=ax.transAxes, ha="left", va="bottom", fontsize=10.2, fontweight="bold", color=COLORS["ink"])
    left, right = 0.0, 1.0
    top, header, mid, bottom = 0.82, 0.72, 0.62, 0.20
    ax.hlines([top, mid, bottom], left, right, colors=COLORS["ink"], linewidths=[0.9, 0.8, 0.9])
    col_x = [0.01, 0.24, 0.53, 0.79]
    headers = ["Case", "Need profile", "Access / mismatch", "Interpretation"]
    for x, h in zip(col_x, headers):
        ax.text(x, header, h, fontsize=7.5, fontweight="bold", color=COLORS["ink"], va="center", ha="left")

    row_y = [0.49, 0.30]
    rows = [
        [
            "Barking Riverside /\nThames View edge",
            "High: children 30.01%;\ncaregiver HH 52.60%;\nneed score 68.75",
            "Low walk access:\n14.22\nHigh deficit: +54.53",
            "Negative case:\ncare-chain mismatch\nis unusually clear.",
        ],
        [
            "Newham micro\ncounter-case",
            "High: children 25.20%;\ncaregiver HH 49.22%;\nneed score 55.72",
            "High walk access:\n64.82\nAccess surplus: -9.10",
            "Positive case:\nlocal service density\noffsets care pressure.",
        ],
    ]
    for y, row in zip(row_y, rows):
        for c, (x, txt) in enumerate(zip(col_x, row)):
            ax.text(x, y, txt, fontsize=6.75, color=COLORS["ink"], va="center", ha="left", linespacing=1.12)
    ax.text(0, -0.08,
            "Note: values are LSOA-cluster means. Positive mismatch indicates need exceeds modelled walking accessibility.",
            transform=ax.transAxes, ha="left", va="top", fontsize=7.6, color=COLORS["muted"])
    fig.savefig(OUT / "table10_case_comparison_revised.png", bbox_inches="tight", facecolor="white")
    fig.savefig(OUT / "table10_case_comparison_revised.pdf", bbox_inches="tight", facecolor="white")
    plt.close(fig)


def main():
    print(f"Writing revised visual assets to {OUT}")
    figure7()
    figure9()
    figure10()
    figure12_v10()
    formula_block()
    table3_image()
    table10_image()
    print("Done.")


if __name__ == "__main__":
    main()
