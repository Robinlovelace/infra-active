import { defineConfig } from 'vite';
import { svelte } from '@sveltejs/vite-plugin-svelte';
import { dirname, resolve } from 'node:path';
import { existsSync, cpSync, mkdirSync, readdirSync, rmSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

const root = dirname(fileURLToPath(import.meta.url));

// The R data pipeline owns docs/map/data/*.pmtiles. emptyOutDir: true (below)
// would delete them on rebuild, so snapshot them before the build starts and
// restore afterwards if they vanished. Keeps builds clean without nuking data.
const dataDir = resolve(root, '../docs/map/data');
const dataBackupDir = resolve(root, 'node_modules/.cache/pmtiles-data-backup');
const hasPmtiles = (dir) =>
  existsSync(dir) && readdirSync(dir).some((f) => f.endsWith('.pmtiles'));
if (hasPmtiles(dataDir)) {
  rmSync(dataBackupDir, { recursive: true, force: true });
  mkdirSync(dataBackupDir, { recursive: true });
  cpSync(dataDir, dataBackupDir, { recursive: true });
}

function preservePmtilesData() {
  return {
    name: 'preserve-pmtiles-data',
    closeBundle() {
      if (hasPmtiles(dataBackupDir) && !hasPmtiles(dataDir)) {
        mkdirSync(dataDir, { recursive: true });
        cpSync(dataBackupDir, dataDir, { recursive: true });
        console.log('[preserve-pmtiles-data] restored docs/map/data/*.pmtiles after build');
      }
    }
  };
}

// Builds a single self-executing ES bundle consumed by a static HTML page:
//   <script type="module" src="map/assets/srn-map.js"></script>
// All CSS (MapLibre + Svelte component styles) is injected at runtime by the
// bundle itself, so no sibling CSS file is emitted. All runtime asset paths
// (PMTiles data) are relative so the bundle works from the /infra-active/
// GitHub Pages subpath.
export default defineConfig({
  plugins: [
    preservePmtilesData(),
    svelte({
      compilerOptions: { css: 'injected' }
    })
  ],
  base: './',
  build: {
    outDir: '../docs/map',
    emptyOutDir: true,
    rollupOptions: {
      input: { 'srn-map': resolve(root, 'src/main.js') },
      output: {
        format: 'es',
        entryFileNames: 'assets/srn-map.js',
        chunkFileNames: 'assets/[name]-[hash].js',
        assetFileNames: 'assets/srn-map[extname]',
        inlineDynamicImports: true
      }
    }
  }
});
