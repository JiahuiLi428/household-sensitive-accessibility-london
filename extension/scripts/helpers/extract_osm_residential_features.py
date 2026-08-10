from __future__ import annotations

import json
import time
from pathlib import Path

import geopandas as gpd
import osmium
import pandas as pd
from shapely import wkb
from shapely.geometry import Point
from shapely.prepared import prep


ROOT = Path(__file__).resolve().parents[3]
PBF = ROOT / "data" / "raw" / "osm" / "greater-london-latest.osm.pbf"
BOUNDARY = ROOT / "data" / "boundaries" / "london_lsoa_2021.gpkg"
OUT_GPKG = ROOT / "extension" / "data_intermediate" / "osm_residential_features.gpkg"
OUT_SUMMARY = ROOT / "extension" / "outputs" / "tables" / "osm_residential_feature_summary.csv"
LOG_DIR = ROOT / "extension" / "logs"

RESIDENTIAL_BUILDING_VALUES = {
    "apartments",
    "bungalow",
    "cabin",
    "detached",
    "dormitory",
    "farm",
    "ger",
    "house",
    "houseboat",
    "residential",
    "semidetached_house",
    "static_caravan",
    "terrace",
}

RESIDENTIAL_LANDUSE_VALUES = {"residential"}


def safe_tag(tags: dict[str, str], key: str) -> str | None:
    value = tags.get(key)
    if value is None or value == "":
        return None
    return str(value)


class ResidentialAreaHandler(osmium.SimpleHandler):
    def __init__(self, boundary_geom):
        super().__init__()
        self.boundary = boundary_geom
        self.prepared_boundary = prep(boundary_geom)
        self.bounds = boundary_geom.bounds
        self.wkb_factory = osmium.geom.WKBFactory()
        self.building_rows: list[dict[str, object]] = []
        self.landuse_rows: list[dict[str, object]] = []
        self.area_seen = 0
        self.area_errors = 0

    def maybe_in_london(self, point: Point) -> bool:
        minx, miny, maxx, maxy = self.bounds
        if point.x < minx or point.x > maxx or point.y < miny or point.y > maxy:
            return False
        return self.prepared_boundary.contains(point)

    def area(self, area) -> None:
        tags = dict(area.tags)
        building = safe_tag(tags, "building")
        landuse = safe_tag(tags, "landuse")

        is_res_building = building in RESIDENTIAL_BUILDING_VALUES
        is_res_landuse = landuse in RESIDENTIAL_LANDUSE_VALUES
        if not is_res_building and not is_res_landuse:
            return

        self.area_seen += 1
        try:
            geom = wkb.loads(self.wkb_factory.create_multipolygon(area), hex=True)
        except Exception:
            self.area_errors += 1
            return
        if geom.is_empty:
            return

        rep = geom.representative_point()
        if not self.maybe_in_london(rep):
            return

        osm_type = "way" if area.from_way() else "relation"
        row = {
            "osm_type": osm_type,
            "osm_id": int(area.orig_id()),
            "name": safe_tag(tags, "name"),
            "building": building,
            "landuse": landuse,
            "amenity": safe_tag(tags, "amenity"),
            "shop": safe_tag(tags, "shop"),
            "leisure": safe_tag(tags, "leisure"),
            "feature_tags": json.dumps(
                {
                    k: str(v)
                    for k, v in tags.items()
                    if k
                    in {
                        "building",
                        "landuse",
                        "name",
                        "amenity",
                        "shop",
                        "leisure",
                        "addr:housenumber",
                        "addr:street",
                    }
                },
                ensure_ascii=False,
            ),
            "geometry": geom,
        }
        if is_res_building:
            self.building_rows.append({**row, "feature_class": "residential_building"})
        if is_res_landuse:
            self.landuse_rows.append({**row, "feature_class": "residential_landuse"})


def to_gdf(rows: list[dict[str, object]], crs: str = "EPSG:4326") -> gpd.GeoDataFrame:
    if rows:
        return gpd.GeoDataFrame(pd.DataFrame(rows), geometry="geometry", crs=crs)
    return gpd.GeoDataFrame(
        columns=[
            "osm_type",
            "osm_id",
            "name",
            "building",
            "landuse",
            "amenity",
            "shop",
            "leisure",
            "feature_tags",
            "feature_class",
            "geometry",
        ],
        geometry="geometry",
        crs=crs,
    )


def main() -> None:
    started = time.time()
    LOG_DIR.mkdir(parents=True, exist_ok=True)
    OUT_GPKG.parent.mkdir(parents=True, exist_ok=True)
    OUT_SUMMARY.parent.mkdir(parents=True, exist_ok=True)
    log_path = LOG_DIR / f"osm_residential_extract_{time.strftime('%Y%m%d_%H%M%S')}.log"

    if not PBF.exists():
        raise FileNotFoundError(PBF)
    if not BOUNDARY.exists():
        raise FileNotFoundError(BOUNDARY)

    boundary = gpd.read_file(BOUNDARY).to_crs("EPSG:4326").geometry.union_all()
    handler = ResidentialAreaHandler(boundary)
    handler.apply_file(str(PBF), locations=True, idx="sparse_mem_array")

    buildings = to_gdf(handler.building_rows).to_crs("EPSG:27700")
    landuse = to_gdf(handler.landuse_rows).to_crs("EPSG:27700")

    if len(buildings) > 0:
        buildings["area_sqm"] = buildings.geometry.area
    else:
        buildings["area_sqm"] = pd.Series(dtype="float64")
    if len(landuse) > 0:
        landuse["area_sqm"] = landuse.geometry.area
    else:
        landuse["area_sqm"] = pd.Series(dtype="float64")

    if OUT_GPKG.exists():
        OUT_GPKG.unlink()
    buildings.to_file(OUT_GPKG, layer="residential_buildings", driver="GPKG")
    landuse.to_file(OUT_GPKG, layer="residential_landuse", driver="GPKG")

    summary_rows = [
        {
            "layer": "residential_buildings",
            "feature_count": len(buildings),
            "area_sqm": float(buildings["area_sqm"].sum()) if len(buildings) else 0.0,
        },
        {
            "layer": "residential_landuse",
            "feature_count": len(landuse),
            "area_sqm": float(landuse["area_sqm"].sum()) if len(landuse) else 0.0,
        },
    ]
    summary = pd.DataFrame(summary_rows)
    summary["source_pbf"] = str(PBF.relative_to(ROOT))
    summary["elapsed_seconds"] = round(time.time() - started, 2)
    summary.to_csv(OUT_SUMMARY, index=False)

    log_text = "\n".join(
        [
            f"Started: {time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(started))}",
            f"Source PBF: {PBF}",
            f"Boundary: {BOUNDARY}",
            f"Candidate residential OSM areas seen: {handler.area_seen}",
            f"Area geometry errors: {handler.area_errors}",
            f"Residential buildings: {len(buildings)}",
            f"Residential landuse polygons: {len(landuse)}",
            f"Output: {OUT_GPKG}",
            f"Summary: {OUT_SUMMARY}",
            f"Elapsed seconds: {round(time.time() - started, 2)}",
        ]
    )
    log_path.write_text(log_text, encoding="utf-8")
    print(log_text)


if __name__ == "__main__":
    main()
