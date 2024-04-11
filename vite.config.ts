import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vite";
import elm from "vite-plugin-elm";

export default defineConfig({
  root: "src",
  plugins: [elm(), tailwindcss()]
});
