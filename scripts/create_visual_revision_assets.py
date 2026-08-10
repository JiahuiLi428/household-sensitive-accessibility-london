from pathlib import Path
import warnings

import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib as mpl
import networkx as nx
import numpy as np
import osmnx as ox
import pandas as pd
from matplotlib.lines import Line2D
from matplotlib.patches import FancyArrowPatch
from shapely.ops import unary_union


ROOT = Path(__file__).resolve().parents[1]
ANALYSIS = ROOT / "data" / "output" / "analysis"
BOUNDARIES = ROOT / "data" / "boundaries"
POI = ROOT / "data" / "poi"
OUT = ROOT / "figures" / "publication_revised"
OUT.mkdir(parents=True, exist_ok=True)

mpl.rcParams.update({
    "font.family": "DejaVu Sans",
    "font.size": 9,
    "axes.titlesize": 10.5,
    "axes.titleweight": "bold",
    "axes.labelsize": 8.5,
    "xtick.labelsize": 8,
    "ytick.labelsize": 8,
    "figure.dpi": 130,
    "savefig.dpi": 450,
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
    ax.text(x0 + scale_m / 2, y0 - 0.025 * h, "10 km", ha="center", va="top", fontsize=7, color=COLORS["ink"])
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
        ("School", POI / "education_schools.geojson", COLORS["orange"], "o"),
        ("Supermarket", POI / "food_retail_supermarkets.geojson", COLORS["green"], "s"),
        ("GP", POI / "healthcare_gps.geojson", COLORS["red"], "^"),
        ("Pharmacy", POI / "healthcare_pharmacies.geojson", COLORS["purple"], "D"),
        ("Bus stop", POI / "transport_bus_stops.geojson", "#5A5A5A", "."),
        ("Station", POI / "transport_underground_metro_stations.geojson", COLORS["blue"], "P"),
    ]
    out = []
    for label, path, color, marker in specs:
        g = gpd.read_file(path).to_crs(crs)
        g["facility"] = label
        g["plot_color"] = color
        g["marker"] = marker
        out.append(g[["facility", "plot_color", "marker", "geometry"]])
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
    origin_node = ox.distance.nearest_nodes(G, origin_point.x, origin_point.y)
    lengths = nx.single_source_dijkstra_path_length(G, origin_node, cutoff=distance_m, weight="length")
    reachable_nodes = set(lengths.keys())
    uvals = edges.index.get_level_values(0)
    vvals = edges.index.get_level_values(1)
    mask = [u in reachable_nodes and v in reachable_nodes for u, v in zip(uvals, vvals)]
    return edges.loc[mask].copy()


def figure12():
    lsoas, boroughs, thames = load_base()
    neg = lsoas[lsoas["LSOA21CD"].isin(NEGATIVE_CASE)].copy()
    pos = lsoas[lsoas["LSOA21CD"].isin(POSITIVE_CASE)].copy()
    neg_area = gpd.GeoDataFrame(geometry=[unary_union(neg.geometry)], crs=lsoas.crs)
    pos_area = gpd.GeoDataFrame(geometry=[unary_union(pos.geometry)], crs=lsoas.crs)
    origin = neg_area.geometry.iloc[0].centroid
    pois = load_pois(27700)
    local_window = neg_area.buffer(2400).geometry.iloc[0]
    local_pois = pois[pois.geometry.within(local_window)].copy()

    G, nodes, edges = graph_edges_for_case(neg_area)
    adult = reachable_edges(G, nodes, edges, origin, 1080)
    caregiver = reachable_edges(G, nodes, edges, origin, 810)
    older = reachable_edges(G, nodes, edges, origin, 675)

    fig = plt.figure(figsize=(11.2, 7.0))
    gs = fig.add_gridspec(2, 2, height_ratios=[1, 1.08], width_ratios=[0.95, 1.35], hspace=0.14, wspace=0.08)
    ax_a = fig.add_subplot(gs[0, 0])
    ax_b = fig.add_subplot(gs[:, 1])
    ax_c = fig.add_subplot(gs[1, 0])

    # Panel A: London context.
    lsoas.plot(ax=ax_a, color="#F4F4F1", edgecolor="white", linewidth=0.015)
    boroughs.boundary.plot(ax=ax_a, color=COLORS["boundary"], linewidth=0.16)
    thames.plot(ax=ax_a, color=COLORS["thames"], linewidth=0.5)
    neg_area.boundary.plot(ax=ax_a, color=COLORS["red"], linewidth=1.3)
    neg_area.plot(ax=ax_a, color=COLORS["red"], alpha=0.45)
    pos_area.boundary.plot(ax=ax_a, color=COLORS["blue"], linewidth=1.1)
    pos_area.plot(ax=ax_a, color=COLORS["blue"], alpha=0.35)
    ax_a.set_title("A. Case locations", loc="left")
    ax_a.text(0.02, 0.02, "Red: Barking Riverside / Thames View edge\nBlue: Newham positive counter-case",
              transform=ax_a.transAxes, fontsize=7.6, color=COLORS["muted"], va="bottom")
    ax_a.set_axis_off()
    ax_a.set_aspect("equal")

    # Panel B: local network, facilities and network catchments.
    xmin, ymin, xmax, ymax = neg_area.buffer(2100).total_bounds
    ax_b.set_xlim(xmin, xmax)
    ax_b.set_ylim(ymin, ymax)
    lsoas.cx[xmin:xmax, ymin:ymax].plot(ax=ax_b, color="#F7F7F4", edgecolor="#D5D5CE", linewidth=0.25)
    thames.cx[xmin:xmax, ymin:ymax].plot(ax=ax_b, color=COLORS["thames"], linewidth=1.5, alpha=0.8)
    if edges is not None:
        edges.cx[xmin:xmax, ymin:ymax].plot(ax=ax_b, color="#CCCCCC", linewidth=0.25, alpha=0.8, zorder=3)
        adult.plot(ax=ax_b, color=COLORS["blue"], linewidth=1.15, alpha=0.85, zorder=5)
        caregiver.plot(ax=ax_b, color=COLORS["orange"], linewidth=1.45, alpha=0.95, zorder=6)
        older.plot(ax=ax_b, color=COLORS["red"], linewidth=1.55, alpha=0.95, zorder=7)
    neg.boundary.plot(ax=ax_b, color="#111111", linewidth=1.0, zorder=8)
    local_pois = local_pois.cx[xmin:xmax, ymin:ymax]
    for facility, grp in local_pois.groupby("facility"):
        if facility == "Bus stop":
            sample = grp.sample(min(len(grp), 130), random_state=12) if len(grp) > 130 else grp
            size, alpha = 8, 0.45
        else:
            sample, size, alpha = grp, 30, 0.82
        ax_b.scatter(sample.geometry.x, sample.geometry.y, s=size,
                     c=sample["plot_color"].iloc[0], marker=sample["marker"].iloc[0],
                     edgecolors="white" if facility != "Bus stop" else "none",
                     linewidths=0.35, alpha=alpha, zorder=9, label=facility)
    ax_b.scatter([origin.x], [origin.y], s=60, color="#111111", marker="*", zorder=10, label="Case centroid")
    ax_b.set_title("B. Real walk network catchments and actual facility points", loc="left")
    ax_b.text(0.01, 0.985,
              "Reachable street segments within 15 minutes: adult 1.2 m/s, caregiver/child 0.9 m/s, older adult 0.75 m/s.",
              transform=ax_b.transAxes, va="top", fontsize=7.6, color=COLORS["muted"],
              bbox=dict(boxstyle="round,pad=0.25", fc="white", ec="none", alpha=0.86))
    network_handles = [
        Line2D([0], [0], color=COLORS["blue"], lw=2.2, label="Adult 15 min"),
        Line2D([0], [0], color=COLORS["orange"], lw=2.2, label="Caregiver/child 15 min"),
        Line2D([0], [0], color=COLORS["red"], lw=2.2, label="Older adult 15 min"),
    ]
    poi_handles = [
        Line2D([0], [0], marker="o", color="none", markerfacecolor=COLORS["orange"], markeredgecolor="white", markersize=6, label="School"),
        Line2D([0], [0], marker="s", color="none", markerfacecolor=COLORS["green"], markeredgecolor="white", markersize=6, label="Supermarket"),
        Line2D([0], [0], marker="^", color="none", markerfacecolor=COLORS["red"], markeredgecolor="white", markersize=6, label="GP"),
        Line2D([0], [0], marker="D", color="none", markerfacecolor=COLORS["purple"], markeredgecolor="white", markersize=5.5, label="Pharmacy"),
        Line2D([0], [0], marker="P", color="none", markerfacecolor=COLORS["blue"], markeredgecolor="white", markersize=6, label="Station"),
        Line2D([0], [0], marker=".", color="#5A5A5A", markersize=7, label="Bus stop"),
    ]
    leg1 = ax_b.legend(handles=network_handles, loc="lower left", frameon=True, fontsize=7.5,
                       facecolor="white", edgecolor="#DDDDDD")
    ax_b.add_artist(leg1)
    ax_b.legend(handles=poi_handles, loc="lower right", frameon=True, fontsize=7.2,
                ncol=2, facecolor="white", edgecolor="#DDDDDD")
    ax_b.set_axis_off()
    ax_b.set_aspect("equal")

    # Panel C: simplified care-chain schematic.
    ax_c.set_xlim(0, 1)
    ax_c.set_ylim(0, 1)
    ax_c.set_axis_off()
    ax_c.set_title("C. Care-chain interpretation", loc="left", pad=2)
    steps = ["Home", "School", "Food shop", "Pharmacy / GP", "Station / home"]
    xs = np.linspace(0.08, 0.92, len(steps))
    for i, (x, label) in enumerate(zip(xs, steps)):
        ax_c.scatter([x], [0.64], s=250, color="#F2F2EE", edgecolor="#444444", linewidth=0.9, zorder=3)
        ax_c.text(x, 0.64, str(i + 1), ha="center", va="center", fontsize=8.5, weight="bold", color=COLORS["ink"])
        ax_c.text(x, 0.49, label, ha="center", va="center", fontsize=6.8, color=COLORS["ink"])
        if i < len(xs) - 1:
            ax_c.add_patch(FancyArrowPatch((x + 0.047, 0.64), (xs[i + 1] - 0.047, 0.64),
                                           arrowstyle="-|>", mutation_scale=9, lw=1.0, color="#555555"))
    ax_c.text(0.05, 0.22,
              "Negative case mechanism:\nhigh care need + edge location +\nsmaller usable street network under slower walking speeds.",
              ha="left", va="center", fontsize=8, color=COLORS["muted"], linespacing=1.25)

    fig.suptitle("Figure 12. Case Study: Care-Chain Accessibility at the Barking Riverside Edge",
                 x=0.035, y=0.99, ha="left", fontsize=14.5, fontweight="bold", color=COLORS["ink"])
    fig.text(0.035, 0.945,
             "The revised case figure links London-scale diagnosis to local street-network reach and actual service points.",
             fontsize=9, color=COLORS["muted"], ha="left")
    fig.subplots_adjust(left=0.035, right=0.985, top=0.89, bottom=0.055)
    for ext in ("png", "pdf"):
        fig.savefig(OUT / f"figure12_case_study_accessibility_tripchain_ABC_revised.{ext}", bbox_inches="tight")
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
    figure12()
    formula_block()
    table3_image()
    table10_image()
    print("Done.")


if __name__ == "__main__":
    main()
