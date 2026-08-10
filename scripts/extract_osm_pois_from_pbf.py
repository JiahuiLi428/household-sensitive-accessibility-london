from __future__ import annotations

import json
from pathlib import Path

import geopandas as gpd
import osmium
import pandas as pd
from shapely import wkb
from shapely.geometry import Point


ROOT = Path(__file__).resolve().parents[1]
DATA = ROOT / "data"
PBF = DATA / "raw" / "osm" / "greater-london-latest.osm.pbf"
LONDON_LSOA = DATA / "boundaries" / "london_lsoa_2021.gpkg"
POI_DIR = DATA / "poi"
MANIFEST = DATA / "download_manifest.json"

CATEGORIES = [
    "education_schools",
    "healthcare_gps",
    "healthcare_hospitals",
    "healthcare_pharmacies",
    "food_retail_supermarkets",
    "green_space_parks",
    "transport_underground_metro_stations",
    "transport_bus_stops",
]


def classify(tags: dict[str, str]) -> list[str]:
    categories: list[str] = []

    if tags.get("amenity") == "school":
        categories.append("education_schools")
    if tags.get("amenity") == "doctors" or tags.get("healthcare") == "doctor":
        categories.append("healthcare_gps")
    if tags.get("amenity") == "hospital" or tags.get("healthcare") == "hospital":
        categories.append("healthcare_hospitals")
    if tags.get("amenity") == "pharmacy" or tags.get("healthcare") == "pharmacy":
        categories.append("healthcare_pharmacies")
    if tags.get("shop") == "supermarket":
        categories.append("food_retail_supermarkets")
    if tags.get("leisure") == "park":
        categories.append("green_space_parks")
    if tags.get("highway") == "bus_stop" or tags.get("bus") == "yes":
        categories.append("transport_bus_stops")

    station_text = " ".join(
        tags.get(key, "")
        for key in ["station", "subway", "network", "operator", "name", "public_transport"]
    ).lower()
    railway = tags.get("railway", "")
    if (
        railway in {"station", "halt", "subway_entrance"}
        and any(token in station_text for token in ["subway", "underground", "metro", "london underground"])
    ) or tags.get("station") == "subway" or tags.get("subway") == "yes":
        categories.append("transport_underground_metro_stations")

    return categories


def serialise_tags(tags: dict[str, str]) -> dict[str, str]:
    return {str(key): str(value) for key, value in tags.items()}


class AmenityHandler(osmium.SimpleHandler):
    def __init__(self, boundary):
        super().__init__()
        self.boundary = boundary
        self.bounds = boundary.bounds
        self.rows: dict[str, dict[tuple[str, int], dict[str, object]]] = {
            category: {} for category in CATEGORIES
        }
        self.wkb_factory = osmium.geom.WKBFactory()

    def within_london(self, lon: float, lat: float) -> bool:
        minx, miny, maxx, maxy = self.bounds
        if lon < minx or lon > maxx or lat < miny or lat > maxy:
            return False
        return self.boundary.contains(Point(lon, lat))

    def add_feature(
        self,
        osm_type: str,
        osm_id: int,
        tags: dict[str, str],
        lon: float,
        lat: float,
        source_geometry_type: str,
    ) -> None:
        if not self.within_london(lon, lat):
            return

        categories = classify(tags)
        if not categories:
            return

        row = {
            "osm_type": osm_type,
            "osm_id": int(osm_id),
            "lon": lon,
            "lat": lat,
            "source_geometry_type": source_geometry_type,
            **serialise_tags(tags),
        }
        for category in categories:
            self.rows[category][(osm_type, int(osm_id))] = {**row, "category": category}

    def node(self, node) -> None:
        tags = dict(node.tags)
        if not classify(tags):
            return
        if not node.location.valid():
            return
        self.add_feature(
            "node",
            node.id,
            tags,
            float(node.location.lon),
            float(node.location.lat),
            "Point",
        )

    def way(self, way) -> None:
        tags = dict(way.tags)
        if not classify(tags):
            return
        if way.is_closed():
            return

        xs: list[float] = []
        ys: list[float] = []
        for node_ref in way.nodes:
            if node_ref.location.valid():
                xs.append(float(node_ref.location.lon))
                ys.append(float(node_ref.location.lat))
        if not xs:
            return
        self.add_feature(
            "way",
            way.id,
            tags,
            sum(xs) / len(xs),
            sum(ys) / len(ys),
            "LineString",
        )

    def area(self, area) -> None:
        tags = dict(area.tags)
        if not classify(tags):
            return
        try:
            geom = wkb.loads(self.wkb_factory.create_multipolygon(area), hex=True)
        except Exception:
            return
        if geom.is_empty:
            return
        point = geom.representative_point()
        osm_type = "way" if area.from_way() else "relation"
        self.add_feature(
            osm_type,
            area.orig_id(),
            tags,
            float(point.x),
            float(point.y),
            geom.geom_type,
        )


def save_category(category: str, rows: list[dict[str, object]]) -> int:
    if rows:
        df = pd.DataFrame(rows)
        gdf = gpd.GeoDataFrame(
            df,
            geometry=[Point(lon, lat) for lon, lat in zip(df["lon"], df["lat"])],
            crs="EPSG:4326",
        )
    else:
        gdf = gpd.GeoDataFrame(
            columns=["osm_type", "osm_id", "name", "category", "lon", "lat", "geometry"],
            geometry="geometry",
            crs="EPSG:4326",
        )

    gdf.to_file(POI_DIR / f"{category}.geojson", driver="GeoJSON")

    point_cols = ["osm_type", "osm_id", "name", "category", "lon", "lat", "source_geometry_type"]
    for col in point_cols:
        if col not in gdf.columns:
            gdf[col] = None
    gdf[point_cols].to_csv(POI_DIR / f"{category}_points.csv", index=False)
    return len(gdf)


def main() -> None:
    if not PBF.exists():
        raise FileNotFoundError(PBF)
    if not LONDON_LSOA.exists():
        raise FileNotFoundError(LONDON_LSOA)

    POI_DIR.mkdir(parents=True, exist_ok=True)
    boundary = gpd.read_file(LONDON_LSOA).to_crs("EPSG:4326").geometry.union_all()

    handler = AmenityHandler(boundary)
    handler.apply_file(str(PBF), locations=True, idx="sparse_mem_array")

    counts = {
        category: save_category(category, list(handler.rows[category].values()))
        for category in CATEGORIES
    }

    manifest = json.loads(MANIFEST.read_text(encoding="utf-8")) if MANIFEST.exists() else {}
    manifest.setdefault("sources", {})[
        "OSM_POIs"
    ] = "Geofabrik Greater London OSM PBF, extracted with pyosmium and clipped to London LSOA 2021 boundary"
    manifest.setdefault("outputs", {})["OSM_POI_folder"] = str(POI_DIR)
    manifest.setdefault("outputs", {})["Greater_London_OSM_PBF"] = str(PBF)
    manifest["poi_counts"] = counts
    MANIFEST.write_text(json.dumps(manifest, indent=2), encoding="utf-8")

    print(json.dumps({"poi_counts": counts}, indent=2))


if __name__ == "__main__":
    main()
