import path from "node:path";

import { defineConfig } from "vite";
import { nodeResolve } from "@rollup/plugin-node-resolve";

const rootDir = import.meta.dirname;

export default defineConfig({
  base: "./",

  root: rootDir,

  build: {
    assetsInlineLimit: 0,
  },

  plugins: [nodeResolve()],

  server: {
    watch: {
      ignored: ["**/_opam"],
    },
  },
});
