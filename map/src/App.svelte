<script>
  import { onMount } from 'svelte';
  import maplibregl from 'maplibre-gl';
  import { PMTiles, Protocol } from 'pmtiles';

  // Relative data paths (relative to the hosting page, e.g. docs/srn.html),
  // so they resolve under the /infra-active/ GitHub Pages subpath.
  const LSOA_URL = 'map/data/lsoa.pmtiles';
  const SRN_URL = 'map/data/srn.pmtiles';

  // share_active (0-45+) colour ramp: pale grey-green to deep green.
  const STOPS = [0, 5, 10, 15, 20, 30, 45];
  const RAMP = ['#edf2e6', '#d3e6c6', '#aed69f', '#83c27e', '#58aa62', '#33844d', '#1a6b3c'];

  const BANDS = [
    { value: 0, label: 'All' },
    { value: 1, label: '<500 m' },
    { value: 2, label: '500 m\u20131 km' },
    { value: 3, label: '1\u20132 km' },
    { value: 4, label: '2\u20135 km' },
    { value: 5, label: '>5 km' }
  ];

  const BAND_LABELS = {
    1: '<500 m from SRN',
    2: '500 m\u20131 km from SRN',
    3: '1\u20132 km from SRN',
    4: '2\u20135 km from SRN',
    5: '>5 km from SRN'
  };

  let container;
  let map = null;
  let selectedBand = 0;
  let loading = true;
  let warning = '';
  let tip = null; // { x, y, html }
  let hoveredCode = null;

  const esc = (v) =>
    String(v).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

  const fmtPct = (v) => {
    const n = Number(v);
    if (v == null || Number.isNaN(n)) return '';
    return (Number.isInteger(n) ? n.toString() : n.toFixed(1)) + '%';
  };

  const num = (v) => (v == null || v === '' ? '' : Number(v).toLocaleString('en-GB'));

  function legendGradient() {
    const max = STOPS[STOPS.length - 1];
    const parts = STOPS.map((s, i) => `${RAMP[i]} ${((s / max) * 100).toFixed(1)}%`);
    return `linear-gradient(to right, ${parts.join(', ')})`;
  }

  function bandFilterExpr(v) {
    // to-string covers band stored as number or string in the tiles.
    return v === 0 ? null : ['==', ['to-string', ['get', 'band']], String(v)];
  }

  function applyBandFilter(v) {
    selectedBand = v;
    if (!map) return;
    const f = bandFilterExpr(v);
    if (map.getLayer('lsoa-fill')) map.setFilter('lsoa-fill', f);
    updateHoverFilter();
  }

  function updateHoverFilter() {
    if (!map || !map.getLayer('lsoa-hover')) return;
    const parts = [['==', ['get', 'lsoa21cd'], hoveredCode || '__none__']];
    const bf = bandFilterExpr(selectedBand);
    if (bf) parts.push(bf);
    map.setFilter('lsoa-hover', ['all', ...parts]);
  }

  function tooltipHtml(p) {
    const bl = BAND_LABELS[Number(p.band)] || '';
    return (
      `<strong>${esc(p.name || p.lsoa21cd || 'LSOA')}</strong><br>` +
      `${esc(bl)}<br>` +
      `Active travel: ${esc(fmtPct(p.share_active))}`
    );
  }

  function lsoaPopupHtml(p) {
    const rows = [
      ['LSOA name', p.name],
      ['LSOA code', p.lsoa21cd],
      ['Distance to SRN', BAND_LABELS[Number(p.band)]],
      ['Active travel (walk + cycle)', fmtPct(p.share_active)],
      ['Commuters', num(p.commuters)],
      ['Residents', num(p.residents)],
      ['Density', p.dens_km2 != null ? Math.round(Number(p.dens_km2)).toLocaleString('en-GB') + ' per km\u00b2' : ''],
      ['IMD decile', p.imd_decile != null && p.imd_decile !== '' ? p.imd_decile + ' of 10 (1 = most deprived)' : ''],
      ['Area type', p.rural != null && p.rural !== '' ? (Number(p.rural) === 1 ? 'Rural' : 'Urban') : '']
    ].filter((r) => r[1] !== '' && r[1] != null);
    return (
      '<div class="srn-popup"><dl>' +
      rows.map((r) => `<dt>${esc(r[0])}</dt><dd>${esc(r[1])}</dd>`).join('') +
      '</dl></div>'
    );
  }

  function srnPopupHtml(p) {
    const rows = [
      ['Road', p.roadname],
      ['Link', p.linkref]
    ].filter((r) => r[1] != null && r[1] !== '');
    return (
      '<div class="srn-popup"><dl>' +
      rows.map((r) => `<dt>${esc(r[0])}</dt><dd>${esc(r[1])}</dd>`).join('') +
      '</dl></div>'
    );
  }

  // Validates the PMTiles file is fetchable and returns its vector layer id.
  async function probeLayer(url, fallbackName) {
    const p = new PMTiles(url);
    let layerName = null;
    if (typeof p.getTileJson === 'function') {
      const tj = await p.getTileJson(url);
      layerName = tj && tj.vector_layers && tj.vector_layers[0] && tj.vector_layers[0].id;
    } else if (typeof p.getMetadata === 'function') {
      const meta = await p.getMetadata();
      let vl = meta && meta.vector_layers;
      if (!vl && meta && typeof meta.json === 'string') {
        try {
          vl = JSON.parse(meta.json).vector_layers;
        } catch {}
      } else if (!vl && meta && meta.json) {
        vl = meta.json.vector_layers;
      }
      layerName = vl && vl[0] && vl[0].id;
    }
    return layerName || fallbackName;
  }

  function fillColorExpr() {
    // to-number tolerates share_active stored as string in the tiles.
    return ['interpolate', ['linear'], ['to-number', ['get', 'share_active']]].concat(
      STOPS.flatMap((s, i) => [s, RAMP[i]])
    );
  }

  function addLsoaLayers(layerId) {
    map.addSource('lsoa', { type: 'vector', url: 'pmtiles://' + LSOA_URL });

    map.addLayer({
      id: 'lsoa-fill',
      type: 'fill',
      source: 'lsoa',
      'source-layer': layerId,
      minzoom: 6.5,
      paint: {
        'fill-color': fillColorExpr(),
        'fill-opacity': 0.85,
        'fill-outline-color': '#7d838d'
      }
    });

    map.addLayer({
      id: 'lsoa-hover',
      type: 'line',
      source: 'lsoa',
      'source-layer': layerId,
      minzoom: 6.5,
      filter: ['==', ['get', 'lsoa21cd'], '__none__'],
      paint: { 'line-color': '#1f2937', 'line-width': 1.4 }
    });

    map.on('mouseenter', 'lsoa-fill', () => {
      map.getCanvas().style.cursor = 'pointer';
    });

    map.on('mouseleave', 'lsoa-fill', () => {
      map.getCanvas().style.cursor = '';
      hoveredCode = null;
      updateHoverFilter();
      tip = null;
    });

    map.on('mousemove', 'lsoa-fill', (e) => {
      const f = e.features && e.features[0];
      if (!f) return;
      hoveredCode = f.properties.lsoa21cd;
      updateHoverFilter();
      const w = container.clientWidth || 0;
      const h = container.clientHeight || 0;
      tip = {
        x: Math.min(e.point.x + 14, Math.max(w - 180, 0)),
        y: Math.min(e.point.y + 14, Math.max(h - 60, 0)),
        html: tooltipHtml(f.properties)
      };
    });

    map.on('click', 'lsoa-fill', (e) => {
      const f = e.features && e.features[0];
      if (!f) return;
      new maplibregl.Popup({ closeButton: true, maxWidth: '320px' })
        .setLngLat(e.lngLat)
        .setHTML(lsoaPopupHtml(f.properties))
        .addTo(map);
    });
  }

  function addSrnLayers(layerId) {
    map.addSource('srn', { type: 'vector', url: 'pmtiles://' + SRN_URL });

    map.addLayer({
      id: 'srn-line',
      type: 'line',
      source: 'srn',
      'source-layer': layerId,
      minzoom: 5,
      paint: {
        'line-color': '#334155',
        'line-width': ['interpolate', ['linear'], ['zoom'], 6, 1.2, 10, 2.5]
      }
    });

    map.on('mouseenter', 'srn-line', () => {
      map.getCanvas().style.cursor = 'pointer';
    });
    map.on('mouseleave', 'srn-line', () => {
      map.getCanvas().style.cursor = '';
    });
    map.on('click', 'srn-line', (e) => {
      const f = e.features && e.features[0];
      if (!f) return;
      new maplibregl.Popup({ closeButton: true, maxWidth: '280px' })
        .setLngLat(e.lngLat)
        .setHTML(srnPopupHtml(f.properties))
        .addTo(map);
    });
  }

  // Combine (a) PMTiles metadata probes and (b) the MapLibre 'load' event:
  // layers are added once BOTH are ready, and probe failures surface the
  // warning chip even if the basemap never finishes loading.
  let mapReady = false;
  let probeResults = null;

  // Surface data-availability problems as soon as the probes settle,
  // independently of the (possibly slow) basemap load.
  function handleProbeOutcome() {
    const [lsoaRes, srnRes] = probeResults;
    const missing = [];
    if (lsoaRes.status === 'rejected') missing.push('LSOA');
    if (srnRes.status === 'rejected') missing.push('SRN');
    if (missing.length) {
      warning = `Warning: ${missing.join(' and ')} data not found (map/data/*.pmtiles) \u2014 map shown without ${missing.join('/')}.`;
    }
    if (missing.length === 2) loading = false;
  }

  function tryAddLayers() {
    if (!map || !mapReady || !probeResults) return;
    const [lsoaRes, srnRes] = probeResults;
    if (lsoaRes.status === 'fulfilled') addLsoaLayers(lsoaRes.value);
    if (srnRes.status === 'fulfilled') addSrnLayers(srnRes.value);
    applyBandFilter(selectedBand);
  }

  onMount(() => {
    // PMTiles protocol -> MapLibre (protocol.register() on newer pmtiles,
    // explicit addProtocol otherwise).
    const protocol = new Protocol();
    if (typeof protocol.register === 'function') {
      protocol.register();
    } else {
      maplibregl.addProtocol('pmtiles', protocol.tile);
    }

    const probes = Promise.allSettled([
      probeLayer(LSOA_URL, 'lsoa'),
      probeLayer(SRN_URL, 'srn')
    ]);
    probes.then((r) => {
      probeResults = r;
      handleProbeOutcome();
      tryAddLayers();
    });

    map = new maplibregl.Map({
      container,
      style: {
        version: 8,
        sources: {
          carto: {
            type: 'raster',
            tiles: ['https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png'],
            tileSize: 256,
            attribution: '\u00a9 OpenStreetMap contributors \u00a9 CARTO'
          }
        },
        layers: [{ id: 'carto-light', type: 'raster', source: 'carto' }]
      },
      center: [-1.5, 52.6],
      zoom: 6.2,
      attributionControl: { compact: true }
    });

    map.addControl(new maplibregl.NavigationControl({ showCompass: false }), 'top-right');

    map.on('error', (e) => {
      const src = e && e.sourceId;
      const msg = e && e.error ? String(e.error.message || e.error) : '';
      if (src === 'lsoa' || src === 'srn' || /pmtiles|data/i.test(msg)) {
        if (!warning) warning = 'Warning: map data tiles could not be loaded.';
        loading = false;
      }
    });

    map.on('load', () => {
      mapReady = true;
      tryAddLayers();
    });

    map.once('idle', () => {
      loading = false;
    });
    const timeout = setTimeout(() => {
      loading = false;
    }, 20000);

    return () => {
      clearTimeout(timeout);
      if (map) map.remove();
      map = null;
    };
  });
</script>

<div class="srn-map-root" bind:this={container}>
  {#if tip}
    <div class="tooltip" style="left:{tip.x}px; top:{tip.y}px">{@html tip.html}</div>
  {/if}

  <div class="title-chip">Active travel to work and SRN proximity, England (Census 2021)</div>

  <div class="band-filter" role="group" aria-label="Filter by distance to SRN">
    {#each BANDS as b}
      <button class:active={selectedBand === b.value} onclick={() => applyBandFilter(b.value)}>
        {b.label}
      </button>
    {/each}
  </div>

  {#if loading}
    <div class="load-chip">Loading map data&#8230;</div>
  {/if}

  {#if warning}
    <div class="warn-chip">{warning}</div>
  {/if}

  <div class="legend">
    <div class="legend-title">Share of commuters walking or cycling to work</div>
    <div class="legend-bar" style="background: {legendGradient()}"></div>
    <div class="legend-ticks">
      <span>0%</span><span>15%</span><span>30%</span><span>45%+</span>
    </div>
  </div>
</div>

<style>
  .srn-map-root {
    position: relative;
    height: 560px;
    width: 100%;
    overflow: hidden;
    background: #eef1f4;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial,
      sans-serif;
    font-size: 12px;
    line-height: 1.4;
  }

  .title-chip {
    position: absolute;
    top: 10px;
    left: 10px;
    z-index: 5;
    background: rgba(255, 255, 255, 0.95);
    border: 1px solid #d5dae1;
    border-radius: 6px;
    padding: 5px 10px;
    font-size: 12.5px;
    font-weight: 600;
    color: #1f2937;
    max-width: calc(100% - 80px);
    pointer-events: none;
  }

  .band-filter {
    position: absolute;
    top: 40px;
    left: 10px;
    z-index: 5;
    display: flex;
    background: #ffffff;
    border: 1px solid #d5dae1;
    border-radius: 6px;
    overflow: hidden;
  }

  .band-filter button {
    appearance: none;
    border: 0;
    background: #ffffff;
    color: #374151;
    font-size: 11px;
    line-height: 1;
    padding: 5px 8px;
    cursor: pointer;
    border-right: 1px solid #e5e9ef;
    white-space: nowrap;
  }

  .band-filter button:last-child {
    border-right: 0;
  }

  .band-filter button.active {
    background: #334155;
    color: #ffffff;
  }

  .legend {
    position: absolute;
    left: 10px;
    bottom: 10px;
    z-index: 5;
    background: rgba(255, 255, 255, 0.95);
    border: 1px solid #d5dae1;
    border-radius: 6px;
    padding: 7px 10px 6px;
    width: 200px;
    pointer-events: none;
  }

  .legend-title {
    font-size: 10.5px;
    color: #4b5563;
    margin-bottom: 5px;
  }

  .legend-bar {
    height: 10px;
    border-radius: 3px;
    border: 1px solid #cfd6dd;
  }

  .legend-ticks {
    display: flex;
    justify-content: space-between;
    font-size: 9.5px;
    color: #6b7280;
    margin-top: 2px;
  }

  .load-chip,
  .warn-chip {
    position: absolute;
    top: 10px;
    left: 50%;
    transform: translateX(-50%);
    z-index: 6;
    border-radius: 999px;
    padding: 4px 12px;
    font-size: 11.5px;
    pointer-events: none;
    white-space: nowrap;
  }

  .load-chip {
    background: rgba(255, 255, 255, 0.95);
    border: 1px solid #d5dae1;
    color: #4b5563;
  }

  .warn-chip {
    background: #fef3c7;
    border: 1px solid #f59e0b;
    color: #92400e;
  }

  .tooltip {
    position: absolute;
    z-index: 7;
    background: rgba(255, 255, 255, 0.97);
    border: 1px solid #cbd5e1;
    border-radius: 4px;
    padding: 4px 8px;
    font-size: 11px;
    color: #111827;
    pointer-events: none;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.15);
    white-space: nowrap;
  }

  /* Popup content is injected outside Svelte's scope, so style it globally. */
  :global(.maplibregl-popup-content) {
    font-size: 12px;
    padding: 10px 12px;
  }

  :global(.srn-popup dl) {
    display: grid;
    grid-template-columns: auto auto;
    gap: 2px 12px;
    margin: 0;
  }

  :global(.srn-popup dt) {
    color: #6b7280;
    white-space: nowrap;
  }

  :global(.srn-popup dd) {
    margin: 0;
    color: #111827;
    font-weight: 500;
  }
</style>
