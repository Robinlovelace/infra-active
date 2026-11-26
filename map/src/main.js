import { mount } from 'svelte';
import maplibregl from 'maplibre-gl';
import maplibreCss from 'maplibre-gl/dist/maplibre-gl.css?inline';
import App from './App.svelte';

function injectStyle(css) {
  const el = document.createElement('style');
  el.textContent = css;
  document.head.appendChild(el);
  return el;
}

// MapLibre base CSS + minimal global styles for the mount root.
injectStyle(maplibreCss);
injectStyle(
  '.srn-map-root{position:relative;height:560px;width:100%;}' +
    '.srn-map-root .maplibregl-map{font:inherit;}'
);

function getTarget() {
  let el = document.getElementById('srn-map');
  if (!el) {
    el = document.createElement('div');
    el.id = 'srn-map';
    el.className = 'srn-map-root';
    document.body.appendChild(el);
  }
  return el;
}

mount(App, { target: getTarget() });
